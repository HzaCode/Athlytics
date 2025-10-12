# R/calculate_acwr.R

#' Calculate Acute:Chronic Workload Ratio (ACWR)
#'
#' Computes the Acute:Chronic Workload Ratio (ACWR) from local Strava activity data
#' using rolling average methods. ACWR is a key metric for monitoring training load
#' and injury risk in athletes (Gabbett, 2016; Hulin et al., 2016).
#'
#' @description
#' This function calculates daily training load and derives acute (short-term) and
#' chronic (long-term) load averages, then computes their ratio (ACWR). The ACWR
#' helps identify periods of rapid training load increases that may elevate injury risk.
#' 
#' **Key Concepts:**
#' \itemize{
#'   \item **Acute Load (ATL)**: Rolling average of recent training (default: 7 days)
#'   \item **Chronic Load (CTL)**: Rolling average of longer-term training (default: 28 days)
#'   \item **ACWR**: Ratio of ATL to CTL (ATL / CTL)
#'   \item **Safe Zone**: ACWR between 0.8-1.3 (optimal training stimulus)
#'   \item **Danger Zone**: ACWR > 1.5 (increased injury risk)
#' }
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: `date`, `distance`, `moving_time`, `elapsed_time`, 
#'   `average_heartrate`, `average_watts`, `type`, `elevation_gain`.
#' @param activity_type **Required** character vector. Filter activities by type 
#'   (e.g., `"Run"`, `"Ride"`). **Must specify** to avoid mixing incompatible load metrics.
#' @param load_metric Character string specifying the load calculation method:
#'   \itemize{
#'     \item `"duration_mins"`: Training duration in minutes (default)
#'     \item `"distance_km"`: Distance in kilometers
#'     \item `"elapsed_time_mins"`: Total elapsed time including stops
#'     \item `"tss"`: Training Stress Score approximation using NP/FTP ratio (requires `user_ftp`)
#'     \item `"hrss"`: Heart Rate Stress Score approximation using simplified TRIMP (requires `user_max_hr` and `user_resting_hr`)
#'     \item `"elevation_gain_m"`: Elevation gain in meters
#'   }
#' @param acute_period Integer. Number of days for the acute load window (default: 7).
#'   Represents recent training stimulus. Common values: 3-7 days.
#' @param chronic_period Integer. Number of days for the chronic load window (default: 28).
#'   Represents fitness/adaptation level. Must be greater than `acute_period`.
#'   Common values: 21-42 days.
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string, Date, or POSIXct).
#'   Defaults to one year before `end_date`. Earlier data is used for calculating
#'   initial chronic load.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string, Date, or POSIXct).
#'   Defaults to current date (Sys.Date()).
#' @param user_ftp Numeric. Your Functional Threshold Power in watts. Required only
#'   when `load_metric = "tss"`. Used to normalize power-based training stress.
#' @param user_max_hr Numeric. Your maximum heart rate in bpm. Required only when
#'   `load_metric = "hrss"`. Used for heart rate reserve calculations.
#' @param user_resting_hr Numeric. Your resting heart rate in bpm. Required only when
#'   `load_metric = "hrss"`. Used for heart rate reserve calculations.
#' @param smoothing_period Integer. Number of days for smoothing the ACWR using a
#'   rolling mean (default: 7). Reduces day-to-day noise for clearer trend visualization.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{date}{Date (Date class)}
#'   \item{atl}{Acute Training Load - rolling average over `acute_period` days (numeric)}
#'   \item{ctl}{Chronic Training Load - rolling average over `chronic_period` days (numeric)}
#'   \item{acwr}{Raw ACWR value (atl / ctl) (numeric)}
#'   \item{acwr_smooth}{Smoothed ACWR using `smoothing_period` rolling mean (numeric)}
#' }
#'
#' @details
#' **Algorithm:**
#' 1. **Daily Aggregation**: Sum all activities by date to compute daily load
#' 2. **Complete Time Series**: Fill missing days with zero load (critical for ACWR accuracy)
#' 3. **Acute Load (ATL)**: Rolling mean over `acute_period` days (default: 7)
#' 4. **Chronic Load (CTL)**: Rolling mean over `chronic_period` days (default: 28)
#' 5. **ACWR Calculation**: ATL / CTL (set to NA when CTL < 0.01 to avoid division by zero)
#' 6. **Smoothing**: Optional rolling mean over `smoothing_period` days for visualization
#'
#' **Data Requirements:**
#' The function automatically fetches additional historical data (chronic_period days
#' before start_date) to ensure accurate chronic load calculations at the analysis
#' start point. Ensure your Strava export contains sufficient historical activities.
#'
#' **Load Metric Implementations:**
#' - `"tss"`: Uses normalized power (NP) and FTP to approximate Training Stress Score.
#'   Formula: `(duration * NP * (NP/FTP)^2) / (FTP * 3600) * 100`. This is an approximation
#'   and may differ from TrainingPeaks' official TSS calculation.
#' - `"hrss"`: Uses simplified TRIMP (Training Impulse) based on heart rate reserve.
#'   Formula: `duration * (HR - resting_HR) / (max_HR - resting_HR)`. This is a simplified
#'   version and may differ from other HRSS implementations.
#'
#' **Interpretation Guidelines:**
#' \itemize{
#'   \item ACWR < 0.8: May indicate detraining or insufficient load
#'   \item ACWR 0.8-1.3: "Sweet spot" - optimal training stimulus with lower injury risk
#'   \item ACWR 1.3-1.5: Caution zone - monitor for fatigue
#'   \item ACWR > 1.5: High risk zone - consider load management
#' }
#'
#' **Multi-Athlete Studies:**
#' For cohort analyses, add an `athlete_id` column before calculation and use
#' `group_by(athlete_id)` with `group_modify()`. See examples below and vignettes for details.
#'
#' @references
#' Gabbett, T. J. (2016). The training-injury prevention paradox: should athletes
#' be training smarter and harder? *British Journal of Sports Medicine*, 50(5), 273-280.
#'
#' Hulin, B. T., et al. (2016). The acute:chronic workload ratio predicts injury:
#' high chronic workload may decrease injury risk in elite rugby league players.
#' *British Journal of Sports Medicine*, 50(4), 231-236.
#'
#' @seealso
#' \code{\link{plot_acwr}} for visualization,
#' \code{\link{calculate_acwr_ewma}} for EWMA-based ACWR,
#' \code{\link{load_local_activities}} for data loading,
#' \code{\link{cohort_reference}} for multi-athlete comparisons
#'
#' @importFrom dplyr filter select mutate group_by summarise arrange %>% left_join coalesce case_when ungroup
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom zoo rollmean
#' @importFrom tidyr drop_na
#' @importFrom rlang .data %||%
#' @export
#'
#' @examples
#' # Example using simulated data (Note: sample data is pre-calculated, shown for demonstration)
#' data(athlytics_sample_acwr)
#' print(head(athlytics_sample_acwr))
#'
#' \dontrun{
#' # Example using local Strava export data
#' # Step 1: Download your Strava data export
#' # Go to Strava.com > Settings > My Account > Download or Delete Your Account
#' # You'll receive a ZIP file via email (e.g., export_12345678.zip)
#' 
#' # Step 2: Load activities directly from ZIP (no extraction needed!)
#' activities <- load_local_activities("export_12345678.zip")
#' 
#' # Or from extracted CSV
#' activities <- load_local_activities("strava_export_data/activities.csv")
#' 
#' # Step 3: Calculate ACWR for Runs (using distance)
#' run_acwr <- calculate_acwr(activities_data = activities, 
#'                            activity_type = "Run",
#'                            load_metric = "distance_km")
#' print(tail(run_acwr))
#' 
#' # Calculate ACWR for Rides (using TSS, requires FTP)
#' ride_acwr_tss <- calculate_acwr(activities_data = activities,
#'                                 activity_type = "Ride",
#'                                 load_metric = "tss", 
#'                                 user_ftp = 280)
#' print(tail(ride_acwr_tss))
#' 
#' # Plot the results
#' plot_acwr(run_acwr, highlight_zones = TRUE)
#' 
#' # Multi-athlete cohort analysis
#' library(dplyr)
#' 
#' # Load data for multiple athletes and add athlete_id
#' athlete1 <- load_local_activities("athlete1_export.zip") %>%
#'   mutate(athlete_id = "athlete1")
#' 
#' athlete2 <- load_local_activities("athlete2_export.zip") %>%
#'   mutate(athlete_id = "athlete2")
#' 
#' # Combine all athletes
#' cohort_data <- bind_rows(athlete1, athlete2)
#' 
#' # Calculate ACWR for each athlete using group_modify()
#' cohort_acwr <- cohort_data %>%
#'   group_by(athlete_id) %>%
#'   group_modify(~ calculate_acwr(.x, 
#'                                  activity_type = "Run",
#'                                  load_metric = "duration_mins")) %>%
#'   ungroup()
#' 
#' # View results
#' print(cohort_acwr)
#' }
calculate_acwr <- function(activities_data,
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
  if (missing(activities_data) || is.null(activities_data)) {
    stop("`activities_data` must be provided. Use load_local_activities() to load your Strava export data.")
  }
  
  if (!is.data.frame(activities_data)) {
    stop("`activities_data` must be a data frame (e.g., from load_local_activities()).")
  }
  if (!is.numeric(acute_period) || acute_period <= 0) stop("`acute_period` must be a positive integer.")
  if (!is.numeric(chronic_period) || chronic_period <= 0) stop("`chronic_period` must be a positive integer.")
  if (acute_period >= chronic_period) stop("`acute_period` must be less than `chronic_period`.")
  if (load_metric == "tss" && is.null(user_ftp)) stop("`user_ftp` is required when `load_metric` is 'tss'.")

  valid_load_metrics <- c("duration_mins", "distance_km", "elapsed_time_mins", "tss", "hrss", "elevation_gain_m")
  if (!load_metric %in% valid_load_metrics) stop("Invalid `load_metric`. Choose from: ", paste(valid_load_metrics, collapse = ", "))
  if (load_metric == "hrss" && (is.null(user_max_hr) || is.null(user_resting_hr))) stop("`user_max_hr` and `user_resting_hr` are required when `load_metric` is 'hrss'.")
  
  # Force explicit activity_type specification to prevent mixing incompatible sports
  if (is.null(activity_type) || length(activity_type) == 0) {
    stop("`activity_type` must be explicitly specified (e.g., 'Run' or 'Ride'). ",
         "Mixing different activity types can lead to incompatible load metrics. ",
         "Please specify the activity type(s) you want to analyze.")
  }

  # --- Date Handling ---
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), error = function(e) analysis_end_date - lubridate::days(365))
  if (analysis_start_date >= analysis_end_date) stop("start_date must be before end_date.")

  message(sprintf("Calculating ACWR data from %s to %s.", analysis_start_date, analysis_end_date))
  message(sprintf("Using metric: %s, Activity types: %s", load_metric, paste(activity_type %||% "All", collapse=", ")))
  message(sprintf("Acute period: %d days, Chronic period: %d days", acute_period, chronic_period))

  # --- Get Activities Data (Local Only) ---
  fetch_start_buffer_days <- chronic_period
  fetch_start_date <- analysis_start_date - lubridate::days(fetch_start_buffer_days)
  
  # Use local activities data
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
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  safe_as_numeric <- function(x) { as.numeric(x %||% 0) }

  # Convert data frame to list format for processing
  daily_load_df <- purrr::map_dfr(1:nrow(activities_df_filtered), function(i) {
    activity <- activities_df_filtered[i, ]
    # Get activity date and type
    activity_date <- activity$date
    act_type <- activity$type %||% "Unknown"

    # Extract metrics from data frame columns
    duration_sec <- safe_as_numeric(activity$moving_time)
    distance_m <- safe_as_numeric(activity$distance)
    elapsed_sec <- safe_as_numeric(activity$elapsed_time)
    avg_hr <- safe_as_numeric(activity$average_heartrate)
    avg_power <- safe_as_numeric(activity$average_watts)
    elevation_gain <- safe_as_numeric(activity$elevation_gain)
    # Use weighted_average_watts if available, otherwise average_watts
    np_proxy <- safe_as_numeric(activity$weighted_average_watts %||% activity$average_watts %||% 0) 
    # message(sprintf("    Duration: %.0f sec", duration_sec))

    # --- Added Debugging and Refined Logic --- 
    # message(sprintf("    Inputs check: load_metric='%s', duration_sec=%.1f, distance_m=%.1f, avg_hr=%.1f, np_proxy=%.1f, user_ftp=%s, user_max_hr=%s, user_resting_hr=%s", 
    #                 load_metric, duration_sec, distance_m, avg_hr, np_proxy, 
    #                 deparse(user_ftp), deparse(user_max_hr), deparse(user_resting_hr)))
    
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
              # message("    Skipping HRSS calculation: Missing/invalid HR parameters or avg_hr out of range.")
          }
      } else if (load_metric == "tss") {
           # Check required FTP parameter before calculating
           if (!is.null(user_ftp) && is.numeric(user_ftp) && user_ftp > 0 && np_proxy > 0) {
             intensity_factor <- np_proxy / user_ftp
             load_value <- (duration_sec * np_proxy * intensity_factor) / (user_ftp * 3600) * 100
           } else {
               # message("    Skipping TSS calculation: Missing/invalid FTP or power data (np_proxy).")
           }
      }
      
      # message(sprintf("    Calculated load_value: %.2f", load_value))
    } else {
        # message("    Duration <= 0, load is 0.")
        load_value <- 0 # Define load_value even if duration is 0
    }

    if (!is.na(load_value) && load_value > 0) {
      # message("    -> Activity PASSED filters, returning load data.")
      data.frame(
        date = activity_date,
        load = load_value,
        stringsAsFactors = FALSE
      )
    } else {
      # message("    -> Activity FAILED final check (load NA or <= 0).")
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
  # print(colnames(daily_load_complete))

  acwr_data_intermediate <- daily_load_complete %>%
    dplyr::mutate(
      # Convert daily_load to numeric before rollmean
      daily_load = as.numeric(.data$daily_load),
      acute_load = zoo::rollmean(.data$daily_load, k = acute_period, fill = NA, align = "right"),
      chronic_load = zoo::rollmean(.data$daily_load, k = chronic_period, fill = NA, align = "right")
    ) %>%
    # --- Add check/coercion for chronic_load before filtering/mutate ---
    dplyr::mutate(chronic_load = as.numeric(.data$chronic_load)) %>%
    dplyr::filter(.data$date >= analysis_start_date & .data$date <= analysis_end_date)

  # --- DEBUG: Check colnames after adding acute_load and chronic_load ---
  message("DEBUG: Colnames AFTER adding acute/chronic load and filtering date:")
  # print(colnames(acwr_data_intermediate))

  acwr_data_intermediate <- acwr_data_intermediate %>% # Continue pipe from intermediate result
    dplyr::mutate(
      # Explicitly handle potential NA in chronic_load within the condition
      acwr = ifelse(!is.na(.data$chronic_load) & .data$chronic_load > 0.01,
                    .data$acute_load / .data$chronic_load,
                    NA)
    ) %>%
    # --- Ensure acwr is numeric before next rollmean ---
    dplyr::mutate(acwr = as.numeric(.data$acwr)) %>%
    dplyr::mutate(
      acwr_smooth = zoo::rollmean(.data$acwr, k = smoothing_period, align = "right", fill = NA)
    )

  # --- DEBUG: Check colnames right BEFORE the final select ---
  message("DEBUG: Colnames right BEFORE final select:")
  # print(colnames(acwr_data_intermediate))

  acwr_data <- acwr_data_intermediate %>%
    dplyr::select(.data$date, atl = .data$acute_load, ctl = .data$chronic_load, .data$acwr, .data$acwr_smooth)

  # --- DEBUG: Check colnames AFTER the final select ---
  message("DEBUG: Colnames AFTER final select (in acwr_data):")
  # print(colnames(acwr_data))

  if (nrow(acwr_data) == 0) {
    stop("Could not calculate ACWR after processing. Check data availability and periods.")
  }

  message("Calculation complete.")
  return(acwr_data)
}

# Helper needed if not globally available
# `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x 