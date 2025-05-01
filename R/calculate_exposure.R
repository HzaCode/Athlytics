# R/calculate_exposure.R

#' Calculate Training Load Exposure (ATL, CTL, ACWR)
#'
#' Calculates training load metrics like ATL, CTL, and ACWR from Strava data.
#'
#' Fetches activities within a specified date range, calculates daily load
#' based on a chosen metric (e.g., duration, TSS, HRSS), and computes
#' Acute Training Load (ATL), Chronic Training Load (CTL), and the
#' Acute:Chronic Workload Ratio (ACWR).
#'
#' @param stoken A valid Strava token from `rStrava::strava_oauth()`.
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
#' @details This function provides the data used by `plot_exposure`. It automatically
#'   fetches activities for a longer period before the analysis start date to ensure
#'   the initial CTL calculation is accurate. Depending on the chosen `load_metric`,
#'   you may need to provide `user_ftp` (for TSS) or `user_max_hr` and `user_resting_hr` (for HRSS).
#'
#' @importFrom rStrava get_activity_list
#' @importFrom dplyr filter select mutate arrange group_by summarise %>% left_join coalesce ungroup
#' @importFrom lubridate as_date date days ymd_hms as_datetime time_length
#' @importFrom zoo rollmean
#' @importFrom rlang .data %||%
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires authentication first:
#' # stoken <- rStrava::strava_oauth(..., cache = TRUE)
#'
#' # Exposure using TSS for Rides
#' ride_exposure_tss <- calculate_exposure(
#'     stoken = stoken,
#'     activity_type = "Ride",
#'     load_metric = "tss",
#'     user_ftp = 280,
#'     acute_period = 7,
#'     chronic_period = 28
#' )
#' print(head(ride_exposure_tss))
#'
#' # Exposure using HRSS for Runs
#' run_exposure_hrss <- calculate_exposure(
#'     stoken = stoken,
#'     activity_type = "Run",
#'     load_metric = "hrss",
#'     user_max_hr = 190,
#'     user_resting_hr = 50
#' )
#' print(tail(run_exposure_hrss))
#' }
calculate_exposure <- function(stoken,
                               activity_type = c("Run", "Ride", "VirtualRide", "VirtualRun"),
                               load_metric = "duration_mins",
                               acute_period = 7,
                               chronic_period = 42,
                               user_ftp = NULL,
                               user_max_hr = NULL,
                               user_resting_hr = NULL,
                               end_date = NULL) { # Removed risk_zones from calc function

  # --- Input Validation ---
  if (missing(stoken)) stop("Strava token 'stoken' is required.")
  if (!inherits(stoken, "Token2.0")) {
      warning("stoken does not appear to be a Token2.0 object from rStrava. Trying anyway.")
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
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  # Define the start date for the *analysis window* (not the fetch window)
  analysis_start_date <- analysis_end_date - lubridate::days(chronic_period) + lubridate::days(1)

  message(sprintf("Calculating load exposure data from %s to %s.", analysis_start_date, analysis_end_date))
  message(sprintf("Using metric: %s, Activity types: %s", load_metric, paste(activity_type, collapse=", ")))
  message(sprintf("Acute period: %d days, Chronic period: %d days", acute_period, chronic_period))

  # --- Get Activity Data ---
  # Need activities from analysis_start_date - chronic_period days to calculate initial chronic load correctly
  fetch_start_date <- analysis_start_date - lubridate::days(chronic_period)
  # Convert end_date to Date object for the 'before' parameter
  # Note: Strava API 'before' is exclusive, so add 1 day. Check if rStrava handles this.
  fetch_before_date <- analysis_end_date + lubridate::days(1) 
  # 'after' parameter uses the calculated fetch_start_date (already a Date object)
  fetch_after_date <- fetch_start_date 

  message("Fetching activities from Strava...")
  activities_list <- list()
  page <- 1 # Reset page counter, although it's not used in the call anymore
  fetched_all <- FALSE
  max_pages <- 10 # Safety limit remains, in case rStrava has internal limits
  activities_fetched_count <- 0

  # Simplified loop: Call get_activity_list once, assuming it handles pagination or returns enough data
  # If full pagination is needed later, this loop needs to be redesigned based on how get_activity_list behaves.
  # For now, focus on fixing the Date error.
  current_page_activities <- tryCatch({
      # Call rStrava::get_activity_list with Date objects
      rStrava::get_activity_list(stoken, before = fetch_before_date, after = fetch_after_date)
  }, error = function(e) {
      # On ANY error during get_activity_list, return NULL
      # Let the subsequent check handle the failure to fetch activities
      # message(sprintf("Error captured during rStrava::get_activity_list: %s. Returning NULL.", e$message))
      return(NULL)
  })
  
  if (is.null(current_page_activities)) {
     # Handle potential NULL or empty list even from the single call 
     activities_list <- list() 
     activities_fetched_count <- 0
  } else {
      activities_list <- current_page_activities # Assign directly
      activities_fetched_count <- length(activities_list)
  }

  if (length(activities_list) == 0) {
    stop("Could not fetch activities or no activities found in the required date range.")
  }
  message(sprintf("Fetched %d activities in total (up to rStrava limit).", activities_fetched_count))

  # --- Process Activities into Daily Load ---
  safe_as_numeric <- function(x) { as.numeric(x %||% 0) }

  daily_load_df <- purrr::map_dfr(activities_list, ~{
    # ~.x represents each element (activity) in activities_list
    message(paste("--- Processing Activity ID:", .x$id %||% "NA", "---")) # Start activity processing message
    
    # Robust date handling
    start_dt_val <- .x$start_date_local %||% NA
    activity_date <- NA
    if (inherits(start_dt_val, "POSIXt")) { # Check if it's already POSIXct/POSIXlt
      activity_date <- lubridate::as_date(start_dt_val)
      message(paste("Parsed activity_date (from POSIXt):", activity_date))
    } else {
      # Attempt to parse if it's likely character
      parsed_dt <- lubridate::ymd_hms(as.character(start_dt_val), quiet = FALSE) # Set quiet=FALSE to see warnings
      activity_date <- lubridate::as_date(parsed_dt)
      message(paste("Parsed activity_date (from char):", activity_date))
    }

    act_type <- .x$type %||% "Unknown"
    message(paste("Activity Type:", act_type))
    
    # Ensure date variables are Date objects before comparison
    if (!inherits(fetch_start_date, "Date") || !inherits(analysis_end_date, "Date")) {
        stop("Internal error: fetch_start_date or analysis_end_date are not Date objects.")
    }
    message(paste("Fetch Start Date:", fetch_start_date, "Analysis End Date:", analysis_end_date))

    # Filter by date range required for calculation and activity type
    date_filter_condition <- is.na(activity_date) || activity_date < fetch_start_date || activity_date > analysis_end_date
    message(paste("Date Filter Condition (is.na || < fetch_start || > analysis_end):", date_filter_condition))
    if (date_filter_condition) {
        message(paste("Activity", .x$id %||% "NA", "filtered out by date.")) 
        return(NULL)
    }
    
    # Only filter by activity type if activity_type is not NULL and not empty
    type_filter_condition <- FALSE
    if (!is.null(activity_type) && length(activity_type) > 0) {
      type_filter_condition <- !act_type %in% activity_type
      message(paste("Type Filter Condition (!act_type %in% activity_type):", type_filter_condition))
      if (type_filter_condition) {
          message(paste("Activity", .x$id %||% "NA", "filtered out by type."))
          return(NULL)
      }
    }

    load_value <- 0
    duration_sec <- safe_as_numeric(.x$moving_time)
    message(paste("Raw moving_time:", .x$moving_time, "| duration_sec:", duration_sec))
    distance_m <- safe_as_numeric(.x$distance)
    avg_hr <- safe_as_numeric(.x$average_heartrate)
    avg_power <- safe_as_numeric(.x$average_watts)
    elevation_gain <- safe_as_numeric(.x$total_elevation_gain)
    np_proxy <- safe_as_numeric(.x$device_watts %||% avg_power)

    if (duration_sec > 0) {
        message(paste("Duration > 0, calculating load for metric:", load_metric))
        if (load_metric == "duration_mins") {
            load_value <- duration_sec / 60
            message(paste("Calculated load_value (duration_mins):", load_value))
        } else if (load_metric == "distance_km") {
            load_value <- distance_m / 1000
            message(paste("Calculated load_value (distance_km):", load_value))
        } else if (load_metric == "elevation_gain_m") {
            load_value <- elevation_gain
            message(paste("Calculated load_value (elevation_gain_m):", load_value))
        } else if (load_metric == "hrss") {
            if (avg_hr > 0 && !is.null(user_max_hr) && !is.null(user_resting_hr) && user_max_hr > user_resting_hr) {
              if (avg_hr > user_resting_hr && avg_hr <= user_max_hr) {
                 hr_reserve <- user_max_hr - user_resting_hr
                 avg_hr_rel <- (avg_hr - user_resting_hr) / hr_reserve
                 load_value <- (duration_sec / 60) * avg_hr_rel # Simplified TRIMP
              }
            }
        } else if (load_metric == "tss") {
            if (np_proxy > 0 && !is.null(user_ftp) && user_ftp > 0) {
              intensity_factor <- np_proxy / user_ftp
              load_value <- (duration_sec * np_proxy * intensity_factor) / (user_ftp * 3600) * 100
            }
        }
    } else {
        message("Duration <= 0, load_value remains 0.")
    }

    final_check_condition <- !is.na(load_value) && load_value > 0
    message(paste("Final check condition (!is.na(load_value) && load_value > 0):", final_check_condition))
     if (final_check_condition) {
      message(paste("Activity", .x$id %||% "NA", "PASSED filters and check, returning data frame."))
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
      stop("No activities with valid load data found for the selected metric and types.")
  }

   # Summarise load per day
  daily_load_summary <- daily_load_df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(daily_load = sum(load, na.rm = TRUE), .groups = 'drop')

  # --- Create Full Time Series & Calculate ATL/CTL ---
  all_dates_sequence <- seq(fetch_start_date, analysis_end_date, by = "day")

  load_ts <- dplyr::tibble(date = all_dates_sequence) %>%
      dplyr::left_join(daily_load_summary, by = "date") %>%
      dplyr::mutate(daily_load = dplyr::coalesce(.data$daily_load, 0)) %>%
      dplyr::arrange(date) %>% 
      dplyr::mutate(
          ctl = zoo::rollmean(.data$daily_load, k = chronic_period, fill = NA, align = "right"),
          atl = zoo::rollmean(.data$daily_load, k = acute_period, fill = NA, align = "right")
      ) %>% 
      # Calculate ACWR after ATL/CTL
       dplyr::mutate(acwr = ifelse(.data$ctl > 0.01, .data$atl / .data$ctl, NA)) %>%
      # Filter for the actual analysis period requested
      dplyr::filter(date >= analysis_start_date & date <= analysis_end_date)
      
  if (nrow(load_ts) == 0 || all(is.na(load_ts$atl)) || all(is.na(load_ts$ctl))) {
      stop("Could not calculate ATL/CTL. Check activity data and date ranges.")
  }

  # Return the data frame
  message("Calculation complete.")
  return(load_ts)
}

# Helper needed if not globally available
# `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x 