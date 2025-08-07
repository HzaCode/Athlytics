# R/calculate_ef.R

#' Calculate Efficiency Factor (EF) Data
#'
#' Calculates Efficiency Factor (Pace/HR or Power/HR) from Strava activities.
#'
#' @param stoken A valid Strava token object obtained using \code{rStrava::strava_oauth()}.
#' @param activity_type Character vector or single string specifying activity type(s).
#' @param ef_metric Character string specifying the EF metric ("Pace_HR" or "Power_HR").
#' @param start_date Optional start date (YYYY-MM-DD string or Date object). Defaults to one year ago.
#' @param end_date Optional end date (YYYY-MM-DD string or Date object). Defaults to today.
#' @param min_duration_mins Numeric, minimum activity duration in minutes. Default 20.
#'
#' @return A data frame with columns: date, activity_type, ef_value.
#'
#' @details
#' Fetches activity summaries and calculates EF (output/HR) for each.
#' Provides the data used by `plot_ef`.
#'
#' @importFrom rStrava get_activity_list
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
#' # Example using real data (requires authentication)
#' # To authenticate (replace with your details):
#' # stoken <- rStrava::strava_oauth(app_name = "YOUR_APP",
#' #                                client_id = "YOUR_ID",
#' #                                client_secret = "YOUR_SECRET",
#' #                                cache = TRUE)
#'
#' # Calculate Pace/HR efficiency factor for Runs
#' # Note: stoken should be defined and valid
#' # ef_data_run <- calculate_ef(stoken = stoken, activity_type = "Run", ef_metric = "Pace_HR")
#' # print(tail(ef_data_run))
#'
#' # Calculate Power/HR efficiency factor for Rides
#' # Note: stoken should be defined and valid
#' # ef_data_ride <- calculate_ef(stoken = stoken, activity_type = "Ride", ef_metric = "Power_HR")
#' # print(tail(ef_data_ride))
#' }
calculate_ef <- function(stoken,
                         activity_type = c("Run", "Ride"),
                         ef_metric = c("Pace_HR", "Power_HR"),
                         start_date = NULL,
                         end_date = NULL,
                         min_duration_mins = 20) {

  # --- Input Validation ---
  if (!inherits(stoken, "Token2.0")) {
    stop("`stoken` must be a valid Token2.0 object from rStrava::strava_oauth().")
  }
  ef_metric <- match.arg(ef_metric)
  if (!is.numeric(min_duration_mins) || min_duration_mins < 0) {
    stop("`min_duration_mins` must be a non-negative number.")
  }

  # --- Date Handling ---
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), error = function(e) analysis_end_date - lubridate::days(365))
  if (analysis_start_date >= analysis_end_date) stop("start_date must be before end_date.")

  message(sprintf("Calculating EF data from %s to %s.", analysis_start_date, analysis_end_date))
  message(sprintf("Metric: %s, Activity types: %s", ef_metric, paste(activity_type, collapse=", ")))

  # --- Fetch Strava Activities ---
  fetch_start_date <- analysis_start_date
  fetch_before_date <- analysis_end_date + lubridate::days(1)
  fetch_after_date <- fetch_start_date

  message("Fetching activities from Strava...")
  activities_list <- list()
  page <- 1
  fetched_all <- FALSE
  max_pages <- 10
  activities_fetched_count <- 0

  while (!fetched_all && page <= max_pages) {
    current_page_activities <- tryCatch({
      rStrava::get_activity_list(stoken, before = fetch_before_date, after = fetch_after_date)
    }, error = function(e) {
      message(sprintf("Error fetching activities: %s", e$message))
      return(NULL)
    })

    if (is.null(current_page_activities)) {
      fetched_all <- TRUE
    } else if (length(current_page_activities) > 0) {
      activities_list <- c(activities_list, current_page_activities)
      activities_fetched_count <- activities_fetched_count + length(current_page_activities)
      fetched_all <- TRUE
    } else {
      fetched_all <- TRUE
    }
    if (!fetched_all) Sys.sleep(0.5)
  }

  if (length(activities_list) == 0) stop("Could not fetch activities or no activities found in the date range.")
  message(sprintf("Fetched %d activities.", activities_fetched_count))

  # --- Process Activities & Calculate EF ---
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  safe_as_numeric <- function(x) { as.numeric(x %||% 0) }

  ef_data <- purrr::map_dfr(activities_list, ~{
    act_type <- .x$type %||% "Unknown"
    activity_date <- lubridate::as_date(lubridate::ymd_hms(.x$start_date_local %||% NA))
    duration_sec <- safe_as_numeric(.x$moving_time)
    avg_hr <- safe_as_numeric(.x$average_heartrate)
    distance_m <- safe_as_numeric(.x$distance)
    avg_power <- safe_as_numeric(.x$average_watts)
    device_power <- safe_as_numeric(.x$device_watts)
    power_used <- ifelse(device_power > 0, device_power, avg_power)

    if (is.na(activity_date) || activity_date < analysis_start_date || activity_date > analysis_end_date) return(NULL)
    if (!act_type %in% activity_type) return(NULL)
    if (duration_sec < (min_duration_mins * 60)) return(NULL)
    if (avg_hr <= 0) return(NULL)

    ef_value <- NA
    if (ef_metric == "Pace_HR") {
      if (distance_m > 0 && duration_sec > 0) {
        speed_mps <- distance_m / duration_sec
        ef_value <- speed_mps / avg_hr
      }
    } else if (ef_metric == "Power_HR") {
      if (power_used > 0) {
        ef_value <- power_used / avg_hr
      }
    }

    if (!is.na(ef_value)) {
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

  if (nrow(ef_data) == 0) {
    stop("No suitable activities found with the required data (duration, HR",
         if(ef_metric == "Power_HR") ", power" else if(ef_metric == "Pace_HR") ", distance", "",
         ") for the specified criteria.")
  }

  message(sprintf("Calculation complete. Found EF data for %d activities.", nrow(ef_data)))
  return(ef_data)
}

# Helper needed if not globally available
# `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x 