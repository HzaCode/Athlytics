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
#' - **Acute Load (ATL)**: Rolling average of recent training (default: 7 days)
#' - **Chronic Load (CTL)**: Rolling average of longer-term training (default: 28 days)
#' - **ACWR**: Ratio of ATL to CTL (ATL / CTL)
#' - **Safe Zone**: ACWR between 0.8-1.3 (optimal training stimulus)
#' - **Danger Zone**: ACWR > 1.5 (increased injury risk)
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: `date`, `distance`, `moving_time`, `elapsed_time`,
#'   `average_heartrate`, `average_watts`, `type`, `elevation_gain`.
#' @param activity_type **Required** character vector. Filter activities by type
#'   (e.g., `"Run"`, `"Ride"`). **Must specify** to avoid mixing incompatible load metrics.
#' @param load_metric Character string specifying the load calculation method:
#'   - `"duration_mins"`: Training duration in minutes (default)
#'   - `"distance_km"`: Distance in kilometers
#'   - `"elapsed_time_mins"`: Total elapsed time including stops
#'   - `"tss"`: Training Stress Score approximation using NP/FTP ratio (requires `user_ftp`)
#'   - `"hrss"`: Heart Rate Stress Score approximation using simplified TRIMP (requires `user_max_hr` and `user_resting_hr`)
#'   - `"elevation_gain_m"`: Elevation gain in meters
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
#' @param verbose Logical. If TRUE, prints progress messages. Default FALSE.
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
#' - `"tss"`: Uses normalized power (NP) and FTP to approximate Training Stress Score (TSS).
#'   Formula: `(duration_sec × NP × IF) / (FTP × 3600) × 100`, where `IF = NP/FTP`
#'   (equivalently: `hours × IF^2 × 100`).
#' - `"hrss"`: HR-based load using heart rate reserve (simplified TRIMP; **not** TrainingPeaks hrTSS).
#'   Formula: `duration_sec * (HR - resting_HR) / (max_HR - resting_HR)`.
#'
#' **Interpretation Guidelines:**
#' - ACWR < 0.8: May indicate detraining or insufficient load
#' - ACWR 0.8-1.3: "Sweet spot" - optimal training stimulus with lower injury risk
#' - ACWR 1.3-1.5: Caution zone - monitor for fatigue
#' - ACWR > 1.5: High risk zone - consider load management
#'
#' **Multi-Athlete Studies:**
#' For cohort analyses, add an `athlete_id` column before calculation and use
#' `group_by(athlete_id)` with `group_modify()`. See examples below and vignettes for details.
#'
#' @references
#' Gabbett, T. J. (2016). The training-injury prevention paradox: should athletes
#' be training smarter and harder? *British Journal of Sports Medicine*, 50(5), 273-280.
#' \doi{10.1136/bjsports-2015-095788}
#'
#' Hulin, B. T., Gabbett, T. J., Lawson, D. W., Caputi, P., & Sampson, J. A. (2016).
#' The acute:chronic workload ratio predicts injury: high chronic workload may decrease
#' injury risk in elite rugby league players. *British Journal of Sports Medicine*,
#' 50(4), 231-236. \doi{10.1136/bjsports-2015-094817}
#'
#' @section Scientific Considerations:
#' **Important**: The predictive value of ACWR for injury risk has been debated in
#' recent literature. Some researchers argue that ACWR may have limited utility for
#' predicting injuries (Impellizzeri et al., 2020), and a subsequent analysis has
#' called for dismissing the ACWR framework entirely (Impellizzeri et al., 2021).
#' Users should interpret ACWR risk zones with caution and consider them as
#' descriptive heuristics rather than validated injury predictors.
#'
#' Impellizzeri, F. M., Tenan, M. S., Kempton, T., Novak, A., & Coutts, A. J. (2020).
#' Acute:chronic workload ratio: conceptual issues and fundamental pitfalls.
#' *International Journal of Sports Physiology and Performance*, 15(6), 907-913.
#' \doi{10.1123/ijspp.2019-0864}
#'
#' Impellizzeri, F. M., Woodcock, S., Coutts, A. J., Fanchini, M., McCall, A.,
#' & Vigotsky, A. D. (2021). What role do chronic workloads play in the acute to
#' chronic workload ratio? Time to dismiss ACWR and its underlying theory.
#' *Sports Medicine*, 51(3), 581-592. \doi{10.1007/s40279-020-01378-6}
#'
#' @seealso
#' [plot_acwr()] for visualization,
#' [calculate_acwr_ewma()] for EWMA-based ACWR,
#' [load_local_activities()] for data loading,
#' [calculate_cohort_reference()] for multi-athlete comparisons
#'
#' @export
#'
#' @examples
#' # Example using simulated data (Note: sample data is pre-calculated, shown for demonstration)
#' data(sample_acwr)
#' print(head(sample_acwr))
#'
#' # Runnable example with dummy data:
#' end <- Sys.Date()
#' dates <- seq(end - 59, end, by = "day")
#' dummy_activities <- data.frame(
#'   date = dates,
#'   type = "Run",
#'   moving_time = rep(3600, length(dates)), # 1 hour
#'   distance = rep(10000, length(dates)), # 10 km
#'   average_heartrate = rep(140, length(dates)),
#'   suffer_score = rep(50, length(dates)),
#'   tss = rep(50, length(dates)),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Calculate ACWR
#' result <- calculate_acwr(
#'   activities_data = dummy_activities,
#'   activity_type = "Run",
#'   load_metric = "distance_km",
#'   acute_period = 7,
#'   chronic_period = 28,
#'   end_date = end
#' )
#' print(head(result))
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
#' run_acwr <- calculate_acwr(
#'   activities_data = activities,
#'   activity_type = "Run",
#'   load_metric = "distance_km"
#' )
#' print(tail(run_acwr))
#'
#' # Calculate ACWR for Rides (using TSS, requires FTP)
#' ride_acwr_tss <- calculate_acwr(
#'   activities_data = activities,
#'   activity_type = "Ride",
#'   load_metric = "tss",
#'   user_ftp = 280
#' )
#' print(tail(ride_acwr_tss))
#'
#' # Plot the results
#' plot_acwr(run_acwr, highlight_zones = TRUE)
#'
#' # Multi-athlete cohort analysis
#'
#' # Load data for multiple athletes and add athlete_id
#' athlete1 <- load_local_activities("athlete1_export.zip") %>%
#'   dplyr::mutate(athlete_id = "athlete1")
#'
#' athlete2 <- load_local_activities("athlete2_export.zip") %>%
#'   dplyr::mutate(athlete_id = "athlete2")
#'
#' # Combine all athletes
#' cohort_data <- dplyr::bind_rows(athlete1, athlete2)
#'
#' # Calculate ACWR for each athlete using group_modify()
#' cohort_acwr <- cohort_data %>%
#'   dplyr::group_by(athlete_id) %>%
#'   dplyr::group_modify(~ calculate_acwr(.x,
#'     activity_type = "Run",
#'     load_metric = "duration_mins"
#'   )) %>%
#'   dplyr::ungroup()
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
                           end_date = Sys.Date(),
                           user_ftp = NULL,
                           user_max_hr = NULL,
                           user_resting_hr = NULL,
                           smoothing_period = 7,
                           verbose = FALSE) {
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

  # Validate load metric parameters using internal helper
  validate_load_metric_params(load_metric, user_ftp, user_max_hr, user_resting_hr)

  # Force explicit activity_type specification to prevent mixing incompatible sports
  if (is.null(activity_type) || length(activity_type) == 0) {
    stop(
      "`activity_type` must be explicitly specified (e.g., 'Run' or 'Ride'). ",
      "Mixing different activity types can lead to incompatible load metrics. ",
      "Please specify the activity type(s) you want to analyze."
    )
  }

  # --- Date Handling ---
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), error = function(e) analysis_end_date - lubridate::days(365))
  if (analysis_start_date >= analysis_end_date) stop("start_date must be before end_date.")

  verbose_on <- isTRUE(verbose) || athlytics_is_verbose()

  athlytics_message(sprintf("Calculating ACWR data from %s to %s.", analysis_start_date, analysis_end_date), .verbose = verbose_on)
  athlytics_message(sprintf("Using metric: %s, Activity types: %s", load_metric, paste(activity_type %||% "All", collapse = ", ")), .verbose = verbose_on)
  athlytics_message(sprintf("Acute period: %d days, Chronic period: %d days", acute_period, chronic_period), .verbose = verbose_on)

  # --- Get Activities Data (Local Only) ---
  fetch_start_buffer_days <- chronic_period
  fetch_start_date <- analysis_start_date - lubridate::days(fetch_start_buffer_days)

  # Use local activities data
  athlytics_message("Processing local activities data...", .verbose = verbose_on)
  activities_df_filtered <- activities_data %>%
    dplyr::filter(.data$date >= fetch_start_date & .data$date <= analysis_end_date)

  if (!is.null(activity_type)) {
    activities_df_filtered <- activities_df_filtered %>%
      dplyr::filter(.data$type %in% activity_type)
  }

  activities_fetched_count <- nrow(activities_df_filtered)
  athlytics_message(sprintf("Loaded %d activities from local data.", activities_fetched_count), .verbose = verbose_on)

  if (activities_fetched_count == 0) {
    stop("No activities found in local data for the required date range (", fetch_start_date, " to ", analysis_end_date, ").")
  }

  # --- Process Activities into Daily Load (using internal helper) ---
  daily_load_df <- calculate_daily_load_internal(
    activities_df = activities_df_filtered,
    load_metric = load_metric,
    user_ftp = user_ftp,
    user_max_hr = user_max_hr,
    user_resting_hr = user_resting_hr
  )

  athlytics_message("Finished processing activity list.", .verbose = verbose_on)

  if (is.null(daily_load_df) || nrow(daily_load_df) == 0) {
    stop("No activities found with valid load data for the specified criteria.")
  }

  daily_load_summary <- daily_load_df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(daily_load = sum(load, na.rm = TRUE), .groups = "drop")

  # --- Create Full Time Series & Calculate ATL/CTL ---
  all_dates_sequence <- seq(fetch_start_date, analysis_end_date, by = "day")

  daily_load_complete <- dplyr::tibble(date = all_dates_sequence) %>%
    dplyr::left_join(daily_load_summary, by = "date") %>%
    dplyr::mutate(daily_load = dplyr::coalesce(.data$daily_load, 0)) %>%
    dplyr::arrange(.data$date)

  # --- Force evaluation to potentially resolve lazy-eval issues ---
  force(daily_load_complete)

  if (nrow(daily_load_complete) < chronic_period) {
    warning("Not enough data points (after fetching) to calculate the full chronic period. Results may be unreliable.")
  }

  acwr_data_intermediate <- daily_load_complete %>%
    dplyr::mutate(
      # Convert daily_load to numeric before rollmean
      daily_load = as.numeric(.data$daily_load),
      acute_load = zoo::rollmean(.data$daily_load, k = acute_period, fill = NA, align = "right"),
      chronic_load = zoo::rollmean(.data$daily_load, k = chronic_period, fill = NA, align = "right")
    ) %>%
    # --- Add check/coercion for chronic_load before filtering/mutate ---
    dplyr::mutate(chronic_load = as.numeric(.data$chronic_load)) %>%
    dplyr::filter(.data$date >= analysis_start_date & .data$date <= analysis_end_date) %>%
    dplyr::mutate(
      # Explicitly handle potential NA in chronic_load within the condition
      acwr = ifelse(!is.na(.data$chronic_load) & .data$chronic_load > 0.01,
        .data$acute_load / .data$chronic_load,
        NA
      )
    ) %>%
    # --- Ensure acwr is numeric before next rollmean ---
    dplyr::mutate(acwr = as.numeric(.data$acwr)) %>%
    dplyr::mutate(
      acwr_smooth = zoo::rollmean(.data$acwr, k = smoothing_period, align = "right", fill = NA)
    )

  acwr_data <- acwr_data_intermediate %>%
    dplyr::select("date", atl = "acute_load", ctl = "chronic_load", "acwr", "acwr_smooth")


  if (nrow(acwr_data) == 0) {
    stop("Could not calculate ACWR after processing. Check data availability and periods.")
  }

  athlytics_message("Calculation complete.", .verbose = verbose_on)

  # Add parameters as attributes for plotting
  attr(acwr_data, "params") <- list(
    activity_type = activity_type,
    load_metric = load_metric,
    acute_period = acute_period,
    chronic_period = chronic_period,
    smoothing_period = smoothing_period
  )

  # Add S3 class for type identification

  class(acwr_data) <- c("athlytics_acwr", class(acwr_data))
  return(acwr_data)
}
