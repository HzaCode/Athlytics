# R/calculate_acwr.R

#' Calculate ACWR Data
#'
#' Calculates the Acute:Chronic Workload Ratio (ACWR) from Strava data.
#'
#' Calculates daily load, ATL, CTL, raw ACWR, and smoothed ACWR from Strava activities.
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
#' @return A data frame with columns: `date`, `atl` (Acute Load), `ctl` (Chronic Load),
#' `acwr` (raw ACWR), and `acwr_smooth` (smoothed ACWR) for the specified date range.
#'
#' @details Provides data for `plot_acwr`. Fetches extra prior data for accurate
#'   initial CTL. Fetching can be slow for long periods.
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
#' # Example using simulated data (Note: sample data is pre-calculated, shown for demonstration)
#' data(Athlytics_sample_data)
#' if (!is.null(athlytics_sample_acwr)) {
#'   print(head(athlytics_sample_acwr))
#' }
#'
#' \donttest{
#' # Example using real data (requires authentication and app setup)
#' # Replace with your actual app_name, client_id, and secret or ensure stoken is pre-configured
#' # stoken <- rStrava::strava_oauth(
#' #   app_name = "YOUR_APP_NAME",
#' #   client_id = "YOUR_CLIENT_ID",
#' #   client_secret = "YOUR_SECRET",
#' #   cache = TRUE 
#' # )
#' # if (interactive() && exists("stoken")) { # Proceed if stoken is available
#' #   # Calculate ACWR for Runs (using duration)
#' #   run_acwr <- calculate_acwr(stoken = stoken, activity_type = "Run",
#' #                              load_metric = "duration_mins")
#' #   print(tail(run_acwr))
#' #
#' #   # Calculate ACWR for Rides (using TSS, requires FTP)
#' #   ride_acwr_tss <- calculate_acwr(stoken = stoken, activity_type = "Ride",
#' #                                   load_metric = "tss", user_ftp = 280)
#' #   print(tail(ride_acwr_tss))
#' # } else {
#' #   message("Strava token not available or not in interactive session, skipping real data example.")
#' # }
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
  fetch_before_date <- analysis_end_date + lubridate::days(1)
  fetch_after_date <- fetch_start_date

  message("Fetching activities from Strava...")
  activities_list <- list()
  page <- 1
  fetched_all <- FALSE
  max_pages <- 20
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

  if (length(activities_list) == 0) {
    stop("Could not fetch activities or no relevant activities found in the required date range (", fetch_start_date, " to ", analysis_end_date,").")
  }
  message(sprintf("Fetched %d activities in total.", activities_fetched_count))

  # --- Process Activities into Daily Load ---
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  safe_as_numeric <- function(x) { as.numeric(x %||% 0) }

  daily_load_df <- purrr::map_dfr(activities_list, ~{
    # --- DEBUG REMOVED: Remove browser() call ---
    # if (!exists("debug_invoked_acwr", where = .GlobalEnv)) { 
    #     message("DEBUG: Entering map_dfr function body for the first time. Invoking browser().")
    #     browser() # <<< PAUSES EXECUTION HERE FOR INTERACTIVE DEBUGGING >>>
    #     assign("debug_invoked_acwr", TRUE, envir = .GlobalEnv) # Prevent browser() on subsequent loops
    # }
    # --- End DEBUG ---
    
    # Add index or ID for debugging
    act_id <- as.character(.x$id %||% "UNKNOWN_ID")
    # message(sprintf("  Processing activity %s...", act_id)) # Keep messages commented out for now
    
    # Correctly convert POSIXct to Date without trying to parse as string
    activity_date <- lubridate::as_date(.x$start_date_local %||% NA)
    act_type <- .x$type %||% "Unknown"
    # message(sprintf("    Date: %s, Type: %s", activity_date, act_type))

    if (is.na(activity_date) || activity_date < fetch_start_date || activity_date > analysis_end_date) {
        # message("    -> Filtered out by date.")
        return(NULL)
    }
    if (!is.null(activity_type) && !act_type %in% activity_type) {
        message("    -> Filtered out by type.")
        return(NULL)
    }

    duration_sec <- safe_as_numeric(.x$moving_time)
    distance_m <- safe_as_numeric(.x$distance)
    elapsed_sec <- safe_as_numeric(.x$elapsed_time)
    avg_hr <- safe_as_numeric(.x$average_heartrate)
    avg_power <- safe_as_numeric(.x$average_watts)
    elevation_gain <- safe_as_numeric(.x$total_elevation_gain)
    # Use device_watts if available (direct power), otherwise fallback to average_watts (estimated power)
    np_proxy <- safe_as_numeric(.x$device_watts %||% .x$average_watts %||% 0) 
    message(sprintf("    Duration: %.0f sec", duration_sec))

    # --- Added Debugging and Refined Logic --- 
    message(sprintf("    Inputs check: load_metric='%s', duration_sec=%.1f, distance_m=%.1f, avg_hr=%.1f, np_proxy=%.1f, user_ftp=%s, user_max_hr=%s, user_resting_hr=%s", 
                    load_metric, duration_sec, distance_m, avg_hr, np_proxy, 
                    deparse(user_ftp), deparse(user_max_hr), deparse(user_resting_hr)))
    
    if (duration_sec > 0) {
      # Initialize load_value outside case_when to handle default case cleanly
      load_value <- 0 
      
      if (load_metric == "duration_mins") {
          load_value <- duration_sec / 60
      } else if (load_metric == "distance_km") {
          load_value <- distance_m / 1000
      } else if (load_metric == "elapsed_time_mins") {
          load_value <- elapsed_sec / 60
      } else if (load_metric == "elevation_gain_m") {
          load_value <- elevation_gain
      } else if (load_metric == "hrss") {
          # Check required HR parameters before calculating
          if (!is.null(user_max_hr) && !is.null(user_resting_hr) && is.numeric(user_max_hr) && is.numeric(user_resting_hr) && 
              user_max_hr > user_resting_hr && avg_hr > user_resting_hr && avg_hr <= user_max_hr) {
            hr_reserve <- user_max_hr - user_resting_hr
            avg_hr_rel <- (avg_hr - user_resting_hr) / hr_reserve
            load_value <- (duration_sec / 60) * avg_hr_rel # Simplified TRIMP
          } else {
              message("    Skipping HRSS calculation: Missing/invalid HR parameters or avg_hr out of range.")
          }
      } else if (load_metric == "tss") {
           # Check required FTP parameter before calculating
           if (!is.null(user_ftp) && is.numeric(user_ftp) && user_ftp > 0 && np_proxy > 0) {
             intensity_factor <- np_proxy / user_ftp
             load_value <- (duration_sec * np_proxy * intensity_factor) / (user_ftp * 3600) * 100
           } else {
               message("    Skipping TSS calculation: Missing/invalid FTP or power data (np_proxy).")
           }
      }
      
      message(sprintf("    Calculated load_value: %.2f", load_value))
    } else {
        message("    Duration <= 0, load is 0.")
        load_value <- 0 # Ensure load_value is defined even if duration is 0
    }

    if (!is.na(load_value) && load_value > 0) {
      message("    -> Activity PASSED filters, returning load data.")
      data.frame(
        date = activity_date,
        load = load_value,
        stringsAsFactors = FALSE
      )
    } else {
      message("    -> Activity FAILED final check (load NA or <= 0).")
      NULL
    }
  })

  message("Finished processing activity list.")

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
    
  # --- Force evaluation to potentially resolve lazy-eval issues ---
  force(daily_load_complete)
  # --- End force eval ---
    
  # --- DEBUG REMOVED: Check daily_load_complete before pipeline ---
  # message("--- Checking daily_load_complete structure and summary ---")
  # print(utils::str(daily_load_complete))
  # print(summary(daily_load_complete))
  # --- End DEBUG ---

  if (nrow(daily_load_complete) < chronic_period) {
    warning("Not enough data points (after fetching) to calculate the full chronic period. Results may be unreliable.")
  }
  
  # --- DEBUG REMOVED: Pause before main calculation pipeline ---
  # message("--- Pausing execution before main ACWR calculation pipeline. Type 'c' to continue or 'n' to step through the pipe. ---")
  # browser()
  # --- End DEBUG ---

  # --- DEBUG: Check colnames before the main pipeline ---
  message("DEBUG: Colnames of daily_load_complete BEFORE main ACWR pipeline:")
  print(colnames(daily_load_complete))

  acwr_data_intermediate <- daily_load_complete %>%
    dplyr::mutate(
      # Ensure daily_load is numeric before rollmean
      daily_load = as.numeric(.data$daily_load),
      acute_load = zoo::rollmean(.data$daily_load, k = acute_period, fill = NA, align = "right"),
      chronic_load = zoo::rollmean(.data$daily_load, k = chronic_period, fill = NA, align = "right")
    ) %>%
    # --- Add check/coercion for chronic_load before filtering/mutate ---
    dplyr::mutate(chronic_load = as.numeric(.data$chronic_load)) %>% # Ensure numeric
    dplyr::filter(.data$date >= analysis_start_date & .data$date <= analysis_end_date)

  # --- DEBUG: Check colnames after adding acute_load and chronic_load ---
  message("DEBUG: Colnames AFTER adding acute/chronic load and filtering date:")
  print(colnames(acwr_data_intermediate))

  acwr_data_intermediate <- acwr_data_intermediate %>% # Continue pipe from intermediate result
    dplyr::mutate(
      # Explicitly handle potential NA in chronic_load within the condition
      acwr = ifelse(!is.na(.data$chronic_load) & .data$chronic_load > 0.01,
                    .data$acute_load / .data$chronic_load,
                    NA)
    ) %>%
    # --- Ensure acwr is numeric before next rollmean ---
    dplyr::mutate(acwr = as.numeric(.data$acwr)) %>% # Ensure numeric
    dplyr::mutate(
      acwr_smooth = zoo::rollmean(.data$acwr, k = smoothing_period, align = "right", fill = NA)
    )

  # --- DEBUG: Check colnames right BEFORE the final select ---
  message("DEBUG: Colnames right BEFORE final select:")
  print(colnames(acwr_data_intermediate))

  acwr_data <- acwr_data_intermediate %>%
    dplyr::select(.data$date, atl = .data$acute_load, ctl = .data$chronic_load, .data$acwr, .data$acwr_smooth)

  # --- DEBUG: Check colnames AFTER the final select ---
  message("DEBUG: Colnames AFTER final select (in acwr_data):")
  print(colnames(acwr_data))

  if (nrow(acwr_data) == 0) {
    stop("Could not calculate ACWR after processing. Check data availability and periods.")
  }

  message("Calculation complete.")
  return(acwr_data)
}

# Helper needed if not globally available
# `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x 