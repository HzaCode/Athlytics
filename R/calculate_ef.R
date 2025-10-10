# R/calculate_ef.R

#' Calculate Efficiency Factor (EF)
#'
#' Computes Efficiency Factor (EF) for endurance activities, quantifying the
#' relationship between performance output (pace or power) and heart rate.
#' EF is a key indicator of aerobic fitness and training adaptation (Allen et al., 2019).
#'
#' @description
#' Efficiency Factor measures how much work you perform per unit of cardiovascular
#' effort. Higher EF indicates better aerobic fitness - you're able to maintain faster
#' pace or higher power at the same heart rate. Tracking EF over time helps monitor
#' aerobic base development and training effectiveness.
#'
#' **EF Metrics:**
#' \itemize{
#'   \item **Pace_HR** (for running): Speed (m/s) / Average HR
#'     - Higher values = faster pace at same HR = better fitness
#'   \item **Power_HR** (for cycling): Average Power (watts) / Average HR
#'     - Higher values = more power at same HR = better fitness
#' }
#'
#' **What Improves EF?**
#' \itemize{
#'   \item Aerobic base building (Zone 2 training)
#'   \item Improved running/cycling economy
#'   \item Enhanced cardiovascular efficiency
#'   \item Increased mitochondrial density
#' }
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: `date`, `type`, `moving_time`, `distance`, 
#'   `average_heartrate`, and `average_watts` (for Power_HR metric).
#' @param activity_type Character vector or single string specifying activity type(s)
#'   to analyze. Common values: `"Run"`, `"Ride"`, or `c("Run", "Ride")`.
#'   Default: `c("Run", "Ride")`.
#' @param ef_metric Character string specifying the efficiency metric:
#'   \itemize{
#'     \item `"pace_hr"` or `"Pace_HR"`: Pace-based efficiency (for running). 
#'       Formula: speed (m/s) / avg_HR. Units: m·s⁻¹·bpm⁻¹ (higher = better fitness)
#'     \item `"power_hr"` or `"Power_HR"`: Power-based efficiency (for cycling). 
#'       Formula: avg_watts / avg_HR. Units: W·bpm⁻¹ (higher = better fitness)
#'   }
#'   Default: `c("pace_hr", "power_hr")` (uses first matching metric for activity type).
#'   Note: Older capitalized names ("Pace_HR", "Power_HR") are supported for backward compatibility.
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string, Date, or POSIXct).
#'   Defaults to one year before `end_date`.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string, Date, or POSIXct).
#'   Defaults to current date (Sys.Date()).
#' @param min_duration_mins Numeric. Minimum activity duration in minutes to include
#'   in analysis (default: 20). Filters out very short activities that may not
#'   represent steady-state aerobic efforts.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{date}{Activity date (Date class)}
#'   \item{activity_type}{Activity type (character: "Run" or "Ride")}
#'   \item{ef_value}{Efficiency Factor value (numeric). Higher = better fitness.
#'     Units: m·s⁻¹·bpm⁻¹ for pace_hr, W·bpm⁻¹ for power_hr.}
#'   \item{status}{Character. "ok" for successful calculation, "non_steady" if steady-state 
#'     criteria not met, "insufficient_data" if data quality issues. (Only if quality checks enabled)}
#' }
#'
#' @details
#' **Algorithm:**
#' 1. Filter activities by type, date range, and minimum duration
#' 2. For each activity, calculate:
#'    - Pace_HR: (distance / moving_time) / average_heartrate
#'    - Power_HR: average_watts / average_heartrate
#' 3. Return one EF value per activity
#'
#' **Data Quality Considerations:**
#' \itemize{
#'   \item Requires heart rate data (activities without HR are excluded)
#'   \item Power_HR requires power meter data (cycling with power)
#'   \item Best for steady-state endurance efforts (tempo runs, long rides)
#'   \item Interval workouts may give misleading EF values
#'   \item Environmental factors (heat, altitude) can affect EF
#' }
#'
#' **Interpretation:**
#' \itemize{
#'   \item **Upward trend**: Improving aerobic fitness
#'   \item **Stable**: Maintenance phase
#'   \item **Downward trend**: Possible overtraining, fatigue, or environmental stress
#'   \item **Sudden drop**: Check for illness, equipment change, or data quality
#' }
#'
#' **Typical EF Ranges (Pace_HR for running):**
#' \itemize{
#'   \item Beginner: 0.01 - 0.015 (m/s per bpm)
#'   \item Intermediate: 0.015 - 0.020
#'   \item Advanced: 0.020 - 0.025
#'   \item Elite: > 0.025
#' }
#'
#' Note: EF values are relative to individual baseline. Focus on personal trends
#' rather than absolute comparisons with other athletes.
#'
#' @references
#' Allen, H., Coggan, A. R., & McGregor, S. (2019). *Training and Racing with a
#' Power Meter* (3rd ed.). VeloPress.
#'
#' @seealso
#' \code{\link{plot_ef}} for visualization with trend lines,
#' \code{\link{calculate_decoupling}} for within-activity efficiency analysis,
#' \code{\link{load_local_activities}} for data loading
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
#'                              ef_metric = "pace_hr")
#' print(tail(ef_data_run))
#'
#' # Calculate Power/HR efficiency factor for Rides
#' ef_data_ride <- calculate_ef(activities_data = activities,
#'                               activity_type = "Ride",
#'                               ef_metric = "power_hr")
#' print(tail(ef_data_ride))
#' }
calculate_ef <- function(activities_data,
                         activity_type = c("Run", "Ride"),
                         ef_metric = c("pace_hr", "power_hr", "Pace_HR", "Power_HR"),
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
  
  # Normalize to lowercase (support legacy capitalized names)
  ef_metric <- tolower(ef_metric)
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

    if (ef_metric == "pace_hr") {
      if (distance_m > 0 && duration_sec > 0) {
        # Calculate speed (m/s) / HR - EF is speed per unit HR
        speed_ms <- distance_m / duration_sec
        ef_value <- speed_ms / avg_hr
      }
    } else if (ef_metric == "power_hr") {
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