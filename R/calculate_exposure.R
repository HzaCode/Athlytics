# R/calculate_exposure.R

#' Calculate Training Load Exposure (ATL, CTL, ACWR)
#'
#' Calculates training load metrics like ATL, CTL, and ACWR from local Strava data.
#'
#' Calculates daily load, ATL, CTL, and ACWR from Strava activities based on the chosen metric and periods.
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: date, distance, moving_time, elapsed_time, 
#'   average_heartrate, average_watts, type, elevation_gain.
#' @param activity_type Type(s) of activities to include (e.g., "Run", "Ride").
#'   Default includes common run/ride types.
#' @param load_metric Method for calculating daily load (e.g., "duration_mins",
#'   "distance_km", "tss", "hrss"). Default "duration_mins".
#' @param acute_period Days for the acute load window (e.g., 7).
#' @param chronic_period Days for the chronic load window (e.g., 42). Must be greater than `acute_period`.
#' @param user_ftp Required if `load_metric = "tss"`. Your Functional Threshold Power.
#' @param user_max_hr Required if `load_metric = "hrss"`. Your maximum heart rate.
#' @param user_resting_hr Required if `load_metric = "hrss"`. Your resting heart rate.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#'   The analysis period covers the `chronic_period` days ending on this date.
#'
#' @return A data frame with columns: `date`, `daily_load`, `atl` (Acute Load),
#'   `ctl` (Chronic Load), and `acwr` (Acute:Chronic Ratio) for the analysis period.
#'
#' @details Provides data for `plot_exposure`. Requires extra prior data for
#'   accurate initial CTL. Requires FTP/HR parameters for TSS/HRSS metrics.
#'
#' @importFrom dplyr filter select mutate arrange group_by summarise %>% left_join coalesce ungroup
#' @importFrom lubridate as_date date days ymd_hms as_datetime time_length
#' @importFrom zoo rollmean
#' @importFrom rlang .data %||%
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(athlytics_sample_data)
#' print(head(athlytics_sample_exposure))
#'
#' \dontrun{
#' # Example using local Strava export data
#' activities <- load_local_activities("strava_export_data/activities.csv")
#' 
#' # Calculate training load for Rides using TSS
#' ride_exposure_tss <- calculate_exposure(
#'   activities_data = activities,
#'   activity_type = "Ride",
#'   load_metric = "tss",
#'   user_ftp = 280,
#'   acute_period = 7,
#'   chronic_period = 28
#' )
#' print(head(ride_exposure_tss))
#' 
#' # Calculate training load for Runs using HRSS
#' run_exposure_hrss <- calculate_exposure(
#'   activities_data = activities,
#'   activity_type = "Run",
#'   load_metric = "hrss",
#'   user_max_hr = 190,
#'   user_resting_hr = 50
#' )
#' print(tail(run_exposure_hrss))
#' }
calculate_exposure <- function(activities_data,
                               activity_type = c("Run", "Ride", "VirtualRide", "VirtualRun"),
                               load_metric = "duration_mins",
                               acute_period = 7,
                               chronic_period = 42,
                               user_ftp = NULL,
                               user_max_hr = NULL,
                               user_resting_hr = NULL,
                               end_date = NULL) {

  # --- Input Validation ---
  if (missing(activities_data) || is.null(activities_data)) {
    stop("`activities_data` must be provided. Use load_local_activities() to load your Strava export data.")
  }
  
  if (!is.data.frame(activities_data)) {
    stop("`activities_data` must be a data frame (e.g., from load_local_activities()).")
  }
  
  load_metric_options <- c("duration_mins", "distance_km", "hrss", "tss", "elevation_gain_m")
  if (!load_metric %in% load_metric_options) {
    stop(paste("'load_metric' must be one of:", paste(load_metric_options, collapse=", ")))
  }
  if (load_metric == "tss" && is.null(user_ftp)) {
    stop("user_ftp is required when load_metric = 'tss'.")
  }
  if (load_metric == "hrss" && (is.null(user_max_hr) || is.null(user_resting_hr))) {
    stop("user_max_hr and user_resting_hr are required when load_metric = 'hrss'.")
  }
  if (acute_period >= chronic_period) {
    stop("acute_period must be less than chronic_period.")
  }
  
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  analysis_start_date <- analysis_end_date - lubridate::days(chronic_period) + lubridate::days(1)

  message(sprintf("Calculating load exposure data from %s to %s.", analysis_start_date, analysis_end_date))
  message(sprintf("Using metric: %s, Activity types: %s", load_metric, paste(activity_type, collapse=", ")))
  message(sprintf("Acute period: %d days, Chronic period: %d days", acute_period, chronic_period))

  # --- Get Activity Data (Local Only) ---
  fetch_start_date <- analysis_start_date - lubridate::days(chronic_period)
  
  message("Processing local activities data...")
  activities_df_filtered <- activities_data %>%
    dplyr::filter(.data$date >= fetch_start_date & .data$date <= analysis_end_date)
  
  if (!is.null(activity_type)) {
    activities_df_filtered <- activities_df_filtered %>%
      dplyr::filter(.data$type %in% activity_type)
  }
  
  activities_fetched_count <- nrow(activities_df_filtered)
  message(sprintf("Loaded %d activities from local data.", activities_fetched_count))
  
  if (activities_fetched_count == 0) {
    stop("No activities found in local data for the required date range (", fetch_start_date, " to ", analysis_end_date,").")
  }

  # --- Process Activities into Daily Load ---
  safe_as_numeric <- function(x) { as.numeric(x %||% 0) }

  daily_load_df <- purrr::map_dfr(1:nrow(activities_df_filtered), function(i) {
    activity <- activities_df_filtered[i, ]
    activity_date <- activity$date
    act_type <- activity$type %||% "Unknown"

    duration_sec <- safe_as_numeric(activity$moving_time)
    distance_m <- safe_as_numeric(activity$distance)
    elapsed_sec <- safe_as_numeric(activity$elapsed_time)
    avg_hr <- safe_as_numeric(activity$average_heartrate)
    avg_power <- safe_as_numeric(activity$average_watts)
    elevation_gain <- safe_as_numeric(activity$elevation_gain)
    np_proxy <- safe_as_numeric(activity$weighted_average_watts %||% activity$average_watts %||% 0)

    current_load <- 0
    if (load_metric == "duration_mins") {
      current_load <- duration_sec / 60
    } else if (load_metric == "distance_km") {
      current_load <- distance_m / 1000
    } else if (load_metric == "hrss") {
      if (avg_hr > 0 && !is.null(user_max_hr) && !is.null(user_resting_hr)) {
        hr_reserve <- user_max_hr - user_resting_hr
        if (hr_reserve > 0) {
          percent_hrr <- (avg_hr - user_resting_hr) / hr_reserve
          if (percent_hrr > 0) {
            current_load <- (duration_sec / 3600) * percent_hrr * 100
          }
        }
      }
    } else if (load_metric == "tss") {
      if (np_proxy > 0 && !is.null(user_ftp) && user_ftp > 0) {
        intensity_factor <- np_proxy / user_ftp
        current_load <- (duration_sec / 3600) * (intensity_factor^2) * 100
      }
    } else if (load_metric == "elevation_gain_m") {
      current_load <- elevation_gain
    }

    if (current_load > 0) {
      data.frame(
        date = activity_date,
        load = current_load,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  if (nrow(daily_load_df) == 0) {
    stop("No valid load data could be calculated from activities. Check if activities have the required metrics (HR, power, duration, distance).")
  }

  # --- Aggregate Daily Load ---
  daily_load_agg <- daily_load_df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(daily_load = sum(.data$load, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::arrange(.data$date)

  # --- Create Full Date Sequence ---
  all_dates_df <- data.frame(
    date = seq(from = fetch_start_date, to = analysis_end_date, by = "day")
  )

  load_df <- all_dates_df %>%
    dplyr::left_join(daily_load_agg, by = "date") %>%
    dplyr::mutate(daily_load = dplyr::coalesce(.data$daily_load, 0))

  # --- Calculate ATL, CTL, ACWR ---
  if (nrow(load_df) < chronic_period) {
    stop(sprintf("Not enough data to calculate chronic load (%d days required, %d available).", chronic_period, nrow(load_df)))
  }

  load_df <- load_df %>%
    dplyr::mutate(
      atl = zoo::rollmean(.data$daily_load, k = acute_period, fill = NA, align = "right"),
      ctl = zoo::rollmean(.data$daily_load, k = chronic_period, fill = NA, align = "right")
    ) %>%
    dplyr::mutate(
      acwr = ifelse(.data$ctl > 0, .data$atl / .data$ctl, NA_real_)
    )

  # --- Filter to Analysis Window ---
  exposure_data <- load_df %>%
    dplyr::filter(.data$date >= analysis_start_date & .data$date <= analysis_end_date) %>%
    dplyr::select(.data$date, .data$daily_load, .data$atl, .data$ctl, .data$acwr)

  message("Exposure calculation complete.")
  return(exposure_data)
}