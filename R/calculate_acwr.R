# R/calculate_acwr.R

#' Calculate ACWR Data
#'
#' Calculates the Acute:Chronic Workload Ratio (ACWR) from Strava data.
#'
#' This function fetches activities, calculates daily load using a chosen metric,
#' computes Acute Training Load (ATL) and Chronic Training Load (CTL),
#' and finally calculates the raw and smoothed ACWR.
#'
#' @param stoken A valid Strava token from `rStrava::strava_oauth()`.
#' @param activity_type Optional. Filter activities by type (e.g., "Run", "Ride").
#'   Default `NULL` includes all types.
#' @param load_metric Method for calculating daily load (e.g., "duration_mins",
#'   "distance_km", "tss", "hrss"). Default "duration_mins".
#' @param acute_period Days for the acute load window (e.g., 7).
#' @param chronic_period Days for the chronic load window (e.g., 28). Must be greater than `acute_period`.
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to one year ago.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#' @param user_ftp Required if `load_metric = "tss"`. Your Functional Threshold Power.
#' @param user_max_hr Required if `load_metric = "hrss"`. Your maximum heart rate.
#' @param user_resting_hr Required if `load_metric = "hrss"`. Your resting heart rate.
#' @param smoothing_period Days for smoothing the ACWR using a rolling mean (e.g., 7). Default 7.
#'
#' @return A data frame with columns: `date`, `acwr` (raw ACWR), and `acwr_smooth`
#'   (smoothed ACWR) for the specified date range.
#'
#' @details This function provides the data used by `plot_acwr`. It fetches
#'   activity data going back far enough to accurately calculate the initial
#'   chronic load. Note that fetching data can be slow if the period is long.
#'
#' @importFrom rStrava get_activity_list
#' @importFrom dplyr filter select mutate group_by summarise arrange %>% left_join coalesce case_when ungroup
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom zoo rollmean
#' @importFrom tidyr drop_na
#' @importFrom rlang .data %||%
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires authentication first:
#' # stoken <- rStrava::strava_oauth(..., cache = TRUE)
#'
#' # Calculate ACWR for Runs using duration
#' run_acwr <- calculate_acwr(stoken = stoken, activity_type = "Run",
#'                            load_metric = "duration_mins")
#' print(tail(run_acwr))
#'
#' # Calculate ACWR for Rides using TSS (requires FTP)
#' ride_acwr_tss <- calculate_acwr(stoken = stoken, activity_type = "Ride",
#'                                 load_metric = "tss", user_ftp = 280)
#' print(tail(ride_acwr_tss))
#' }
calculate_acwr <- function(stoken,
                           activity_type = NULL,
                           load_metric = "duration_mins",
                           acute_period = 7,
                           chronic_period = 28,
                           start_date = NULL,
                           end_date = NULL,
                           user_ftp = NULL,
                           user_max_hr = NULL,
                           user_resting_hr = NULL,
                           smoothing_period = 7) {

  # --- Input Validation ---
  if (!inherits(stoken, "Token2.0")) {
    stop("`stoken` must be a valid Token2.0 object from rStrava::strava_oauth().")
  }
  if (!is.numeric(acute_period) || acute_period <= 0) stop("`acute_period` must be a positive integer.")
  if (!is.numeric(chronic_period) || chronic_period <= 0) stop("`chronic_period` must be a positive integer.")
  if (acute_period >= chronic_period) stop("`acute_period` must be less than `chronic_period`.")
  if (load_metric == "tss" && is.null(user_ftp)) stop("`user_ftp` is required when `load_metric` is 'tss'.")

  valid_load_metrics <- c("duration_mins", "distance_km", "elapsed_time_mins", "tss", "hrss", "elevation_gain_m")
  if (!load_metric %in% valid_load_metrics) stop("Invalid `load_metric`. Choose from: ", paste(valid_load_metrics, collapse = ", "))
  if (load_metric == "hrss" && (is.null(user_max_hr) || is.null(user_resting_hr))) stop("`user_max_hr` and `user_resting_hr` are required when `load_metric` is 'hrss'.")

  # --- Date Handling ---
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), error = function(e) analysis_end_date - lubridate::days(365))
  if (analysis_start_date >= analysis_end_date) stop("start_date must be before end_date.")

  message(sprintf("Calculating ACWR data from %s to %s.", analysis_start_date, analysis_end_date))
  message(sprintf("Using metric: %s, Activity types: %s", load_metric, paste(activity_type %||% "All", collapse=", ")))
  message(sprintf("Acute period: %d days, Chronic period: %d days", acute_period, chronic_period))

  # --- Fetch Strava Activities ---
  fetch_start_buffer_days <- chronic_period
  fetch_start_date <- analysis_start_date - lubridate::days(fetch_start_buffer_days)
  before_epoch <- as.numeric(lubridate::as_datetime(analysis_end_date + lubridate::days(1)))
  after_epoch <- as.numeric(lubridate::as_datetime(fetch_start_date))

  message("Fetching activities from Strava...")
  activities_list <- list()
  page <- 1
  fetched_all <- FALSE
  max_pages <- 20
  activities_fetched_count <- 0

  while (!fetched_all && page <= max_pages) {
    current_page_activities <- tryCatch({
      rStrava::get_activity_list(stoken, before = before_epoch, after = after_epoch, page = page, per_page = 200)
    }, error = function(e) {
      message(sprintf("Error fetching page %d: %s", page, e$message))
      return(NULL)
    })

    if (is.null(current_page_activities)) {
      fetched_all <- TRUE
    } else if (length(current_page_activities) > 0) {
      activities_list <- c(activities_list, current_page_activities)
      activities_fetched_count <- activities_fetched_count + length(current_page_activities)
      if (length(current_page_activities) < 200) fetched_all <- TRUE
      page <- page + 1
    } else {
      fetched_all <- TRUE
    }
    if (!fetched_all) Sys.sleep(0.5)
  }

  if (length(activities_list) == 0) {
    stop("Could not fetch activities or no relevant activities found in the required date range (", fetch_start_date, " to ", analysis_end_date,").")
  }
  message(sprintf("Fetched %d activities in total.", activities_fetched_count))

  # --- Process Activities into Daily Load ---
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  safe_as_numeric <- function(x) { as.numeric(x %||% 0) }

  daily_load_df <- purrr::map_dfr(activities_list, ~{
    activity_date <- lubridate::as_date(lubridate::ymd_hms(.x$start_date_local %||% NA))
    act_type <- .x$type %||% "Unknown"

    if (is.na(activity_date) || activity_date < fetch_start_date || activity_date > analysis_end_date) return(NULL)
    if (!is.null(activity_type) && !act_type %in% activity_type) return(NULL)

    load_value <- 0
    duration_sec <- safe_as_numeric(.x$moving_time)
    distance_m <- safe_as_numeric(.x$distance)
    elapsed_sec <- safe_as_numeric(.x$elapsed_time)
    avg_hr <- safe_as_numeric(.x$average_heartrate)
    avg_power <- safe_as_numeric(.x$average_watts)
    elevation_gain <- safe_as_numeric(.x$total_elevation_gain)
    np_proxy <- safe_as_numeric(.x$device_watts %||% avg_power)

    if (duration_sec > 0) {
      load_value <- dplyr::case_when(
        load_metric == "duration_mins" ~ duration_sec / 60,
        load_metric == "distance_km" ~ distance_m / 1000,
        load_metric == "elapsed_time_mins" ~ elapsed_sec / 60,
        load_metric == "elevation_gain_m" ~ elevation_gain,
        load_metric == "hrss" & avg_hr > 0 & !is.null(user_max_hr) & !is.null(user_resting_hr) & user_max_hr > user_resting_hr & avg_hr > user_resting_hr & avg_hr <= user_max_hr ~ {
          hr_reserve <- user_max_hr - user_resting_hr
          avg_hr_rel <- (avg_hr - user_resting_hr) / hr_reserve
          (duration_sec / 60) * avg_hr_rel # Simplified TRIMP
        },
        load_metric == "tss" & np_proxy > 0 & !is.null(user_ftp) & user_ftp > 0 ~ {
          intensity_factor <- np_proxy / user_ftp
          (duration_sec * np_proxy * intensity_factor) / (user_ftp * 3600) * 100
        },
        TRUE ~ 0 # Default case if conditions not met
      )
    }

    if (!is.na(load_value) && load_value > 0) {
      data.frame(
        date = activity_date,
        load = load_value,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  if (is.null(daily_load_df) || nrow(daily_load_df) == 0) {
    stop("No activities found with valid load data for the specified criteria.")
  }

  daily_load_summary <- daily_load_df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(daily_load = sum(load, na.rm = TRUE), .groups = 'drop')

  # --- Create Full Time Series & Calculate ATL/CTL ---
  all_dates_sequence <- seq(fetch_start_date, analysis_end_date, by = "day")

  daily_load_complete <- dplyr::tibble(date = all_dates_sequence) %>%
    dplyr::left_join(daily_load_summary, by = "date") %>%
    dplyr::mutate(daily_load = dplyr::coalesce(.data$daily_load, 0)) %>% 
    dplyr::arrange(.data$date)

  if (nrow(daily_load_complete) < chronic_period) {
    warning("Not enough data points (after fetching) to calculate the full chronic period. Results may be unreliable.")
  }

  acwr_data <- daily_load_complete %>%
    dplyr::mutate(
      acute_load = zoo::rollmean(.data$daily_load, k = acute_period, fill = NA, align = "right"),
      chronic_load = zoo::rollmean(.data$daily_load, k = chronic_period, fill = NA, align = "right")
    ) %>% 
    dplyr::filter(.data$date >= analysis_start_date & .data$date <= analysis_end_date) %>% 
    dplyr::mutate(
      acwr = ifelse(.data$chronic_load > 0.01, .data$acute_load / .data$chronic_load, NA)
    ) %>% 
    dplyr::mutate(
      acwr_smooth = zoo::rollmean(.data$acwr, k = smoothing_period, align = "right", fill = NA)
    ) %>% 
    dplyr::select(.data$date, .data$acwr, .data$acwr_smooth) %>% 
    # Keep NAs for smoothing window at start, only drop if smooth is NA for other reasons (like missing ACWR)
    # tidyr::drop_na(.data$acwr_smooth) # Optional: remove rows where smoothed value is NA
    
  if (nrow(acwr_data) == 0) {
    stop("Could not calculate ACWR after processing. Check data availability and periods.")
  }

  message("Calculation complete.")
  return(acwr_data)
}

# Helper needed if not globally available
# `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x 