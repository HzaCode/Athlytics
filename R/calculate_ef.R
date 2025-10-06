# R/calculate_ef.R

#' Calculate Efficiency Factor (EF) Data
#'
#' Calculates Efficiency Factor (Pace/HR or Power/HR) from Strava activities.
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: date, type, moving_time, distance, average_heartrate, average_watts.
#' @param activity_type Character vector or single string specifying activity type(s).
#' @param ef_metric Character string specifying the EF metric ("Pace_HR" or "Power_HR").
#' @param start_date Optional start date (YYYY-MM-DD string or Date object). Defaults to one year ago.
#' @param end_date Optional end date (YYYY-MM-DD string or Date object). Defaults to today.
#' @param min_duration_mins Numeric, minimum activity duration in minutes. Default 20.
#'
#' @return A data frame with columns: date, activity_type, ef_value.
#'
#' @details
#' Calculates EF (output/HR) for each activity from local export data.
#' Provides the data used by `plot_ef`.
#'
#' @importFrom dplyr filter select mutate arrange %>% rename left_join case_when pull
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom purrr map_dfr
#' @importFrom rlang .data %||%
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(athlytics_sample_data)
#' print(head(athlytics_sample_ef))
#'
#' \dontrun{
#' # Example using local Strava export data
#' activities <- load_local_activities("strava_export_data/activities.csv")
#'
#' # Calculate Pace/HR efficiency factor for Runs
#' ef_data_run <- calculate_ef(activities_data = activities, 
#'                              activity_type = "Run", 
#'                              ef_metric = "Pace_HR")
#' print(tail(ef_data_run))
#'
#' # Calculate Power/HR efficiency factor for Rides
#' ef_data_ride <- calculate_ef(activities_data = activities,
#'                               activity_type = "Ride",
#'                               ef_metric = "Power_HR")
#' print(tail(ef_data_ride))
#' }
calculate_ef <- function(activities_data,
                         activity_type = c("Run", "Ride"),
                         ef_metric = c("Pace_HR", "Power_HR"),
                         start_date = NULL,
                         end_date = NULL,
                         min_duration_mins = 20) {

  # --- Input Validation ---
  if (missing(activities_data) || is.null(activities_data)) {
    stop("`activities_data` must be provided. Use load_local_activities() to load your Strava export data.")
  }
  
  if (!is.data.frame(activities_data)) {
    stop("`activities_data` must be a data frame (e.g., from load_local_activities()).")
  }
  
  ef_metric <- match.arg(ef_metric)
  if (!is.numeric(min_duration_mins) || min_duration_mins < 0) {
    stop("`min_duration_mins` must be a non-negative number.")
  }

  # --- Date Handling ---
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), error = function(e) analysis_end_date - lubridate::days(365))
  if (analysis_start_date >= analysis_end_date) stop("start_date must be before end_date.")

  message(sprintf("Calculating EF data from %s to %s.", analysis_start_date, analysis_end_date))
  message(sprintf("Metric: %s, Activity types: %s", ef_metric, paste(activity_type, collapse=", ")))

  # --- Filter Activities ---
  message("Processing local activities data...")
  activities_df_filtered <- activities_data %>%
    dplyr::filter(.data$date >= analysis_start_date & .data$date <= analysis_end_date)
  
  if (!is.null(activity_type)) {
    activities_df_filtered <- activities_df_filtered %>%
      dplyr::filter(.data$type %in% activity_type)
  }
  
  activities_fetched_count <- nrow(activities_df_filtered)
  message(sprintf("Loaded %d activities from local data.", activities_fetched_count))
  
  if (activities_fetched_count == 0) {
    stop("No activities found in local data for the date range.")
  }

  # --- Process Activities & Calculate EF ---
  safe_as_numeric <- function(x) { as.numeric(x %||% 0) }

  ef_data <- purrr::map_dfr(1:nrow(activities_df_filtered), function(i) {
    activity <- activities_df_filtered[i, ]
    act_type <- activity$type %||% "Unknown"
    activity_date <- activity$date
    duration_sec <- safe_as_numeric(activity$moving_time)
    avg_hr <- safe_as_numeric(activity$average_heartrate)
    distance_m <- safe_as_numeric(activity$distance)
    avg_power <- safe_as_numeric(activity$average_watts)
    weighted_power <- safe_as_numeric(activity$weighted_average_watts)
    power_used <- ifelse(weighted_power > 0, weighted_power, avg_power)

    if (is.na(activity_date) || activity_date < analysis_start_date || activity_date > analysis_end_date) return(NULL)
    if (!act_type %in% activity_type) return(NULL)
    if (duration_sec < (min_duration_mins * 60)) return(NULL)
    if (is.na(avg_hr) || avg_hr <= 0) return(NULL)

    ef_value <- NA

    if (ef_metric == "Pace_HR") {
      if (distance_m > 0 && duration_sec > 0) {
        pace_min_per_km <- (duration_sec / 60) / (distance_m / 1000)
        ef_value <- pace_min_per_km / avg_hr
      }
    } else if (ef_metric == "Power_HR") {
      if (power_used > 0) {
        ef_value <- power_used / avg_hr
      }
    }

    if (!is.na(ef_value) && ef_value > 0) {
      data.frame(
        date = activity_date,
        activity_type = act_type,
        ef_value = ef_value,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  if (is.null(ef_data) || nrow(ef_data) == 0) {
    warning("No activities met the EF calculation criteria. Returning empty data frame.")
    return(data.frame(date = lubridate::as_date(character(0)), 
                     activity_type = character(0), 
                     ef_value = numeric(0)))
  }

  ef_data <- ef_data %>%
    dplyr::arrange(.data$date)

  message("EF calculation complete.")
  return(ef_data)
}