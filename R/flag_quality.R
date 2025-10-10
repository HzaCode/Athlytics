# R/flag_quality.R

#' Flag Data Quality Issues in Activity Streams
#'
#' Detects and flags potential data quality issues in activity stream data,
#' including HR/power spikes, GPS drift, and identifies steady-state segments
#' suitable for physiological metrics calculation.
#'
#' @param streams A data frame containing activity stream data with time-series
#'   measurements. Expected columns: `time` (seconds), `heartrate` (bpm),
#'   `watts` (W), `velocity_smooth` or `speed` (m/s), `distance` (m).
#' @param sport Type of activity (e.g., "Run", "Ride"). Default "Run".
#' @param hr_range Valid heart rate range as c(min, max). Default c(30, 220).
#' @param pw_range Valid power range as c(min, max). Default c(0, 1500).
#' @param max_run_speed Maximum plausible running speed in m/s. Default 7.0 (≈2:23/km).
#' @param max_ride_speed Maximum plausible riding speed in m/s. Default 25.0 (≈90 km/h).
#' @param max_accel Maximum plausible acceleration in m/s². Default 3.0.
#' @param max_hr_jump Maximum plausible HR change per second (bpm/s). Default 10.
#' @param max_pw_jump Maximum plausible power change per second (W/s). Default 300.
#' @param min_steady_minutes Minimum duration (minutes) for steady-state segment. Default 20.
#' @param steady_cv_threshold Coefficient of variation threshold for steady-state (\\%). Default 8.
#'
#' @return A data frame identical to `streams` with additional flag columns:
#'   \describe{
#'     \item{flag_hr_spike}{Logical. TRUE if HR is out of range or has excessive jump.}
#'     \item{flag_pw_spike}{Logical. TRUE if power is out of range or has excessive jump.}
#'     \item{flag_gps_drift}{Logical. TRUE if speed or acceleration is implausible.}
#'     \item{flag_any}{Logical. TRUE if any quality flag is raised.}
#'     \item{is_steady_state}{Logical. TRUE if segment meets steady-state criteria.}
#'     \item{quality_score}{Numeric 0-1. Proportion of clean data (1 = perfect).}
#'   }
#'
#' @details
#' This function performs several quality checks:
#' \itemize{
#'   \item **HR/Power Spikes**: Flags values outside physiological ranges or with
#'     sudden jumps (Delta HR > 10 bpm/s, Delta P > 300 W/s).
#'   \item **GPS Drift**: Flags implausible speeds or accelerations based on sport type.
#'   \item **Steady-State Detection**: Identifies segments with low variability
#'     (CV < 8\%) lasting >= 20 minutes, suitable for EF/decoupling calculations.
#' }
#'
#' The function is sport-aware and adjusts thresholds accordingly. All thresholds
#' are configurable to accommodate different athlete profiles and data quality.
#'
#' @importFrom dplyr mutate lag lead if_else coalesce %>%
#' @importFrom zoo rollmean rollapply
#' @importFrom stats sd
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample activity stream data
#' stream_data <- data.frame(
#'   time = seq(0, 3600, by = 1),
#'   heartrate = rnorm(3601, mean = 150, sd = 10),
#'   watts = rnorm(3601, mean = 200, sd = 20),
#'   velocity_smooth = rnorm(3601, mean = 3.5, sd = 0.3)
#' )
#'
#' # Flag quality issues
#' flagged_data <- flag_quality(stream_data, sport = "Run")
#'
#' # Check summary
#' summary(flagged_data$quality_score)
#' table(flagged_data$flag_any)
#' }
flag_quality <- function(streams,
                        sport = "Run",
                        hr_range = c(30, 220),
                        pw_range = c(0, 1500),
                        max_run_speed = 7.0,
                        max_ride_speed = 25.0,
                        max_accel = 3.0,
                        max_hr_jump = 10,
                        max_pw_jump = 300,
                        min_steady_minutes = 20,
                        steady_cv_threshold = 8) {
  
  # --- Input Validation ---
  if (!is.data.frame(streams)) {
    stop("`streams` must be a data frame.")
  }
  
  if (nrow(streams) == 0) {
    warning("Empty streams data provided. Returning empty data frame with flag columns.")
    streams$flag_hr_spike <- logical(0)
    streams$flag_pw_spike <- logical(0)
    streams$flag_gps_drift <- logical(0)
    streams$flag_any <- logical(0)
    streams$is_steady_state <- logical(0)
    streams$quality_score <- numeric(0)
    return(streams)
  }
  
  # Check for required columns (at least time should exist)
  if (!"time" %in% colnames(streams)) {
    stop("`streams` must contain a 'time' column.")
  }
  
  # Initialize flag columns
  streams$flag_hr_spike <- FALSE
  streams$flag_pw_spike <- FALSE
  streams$flag_gps_drift <- FALSE
  streams$flag_any <- FALSE
  streams$is_steady_state <- FALSE
  streams$quality_score <- 1.0
  
  # --- HR Spike Detection ---
  if ("heartrate" %in% colnames(streams)) {
    hr <- streams$heartrate
    
    # Flag out-of-range HR
    hr_out_of_range <- !is.na(hr) & (hr < hr_range[1] | hr > hr_range[2])
    
    # Flag excessive HR jumps
    hr_diff <- c(0, diff(hr))
    hr_excessive_jump <- !is.na(hr_diff) & abs(hr_diff) > max_hr_jump
    
    streams$flag_hr_spike <- hr_out_of_range | hr_excessive_jump
  }
  
  # --- Power Spike Detection ---
  if ("watts" %in% colnames(streams)) {
    pw <- streams$watts
    
    # Flag out-of-range power
    pw_out_of_range <- !is.na(pw) & (pw < pw_range[1] | pw > pw_range[2])
    
    # Flag excessive power jumps
    pw_diff <- c(0, diff(pw))
    pw_excessive_jump <- !is.na(pw_diff) & abs(pw_diff) > max_pw_jump
    
    streams$flag_pw_spike <- pw_out_of_range | pw_excessive_jump
  }
  
  # --- GPS Drift Detection ---
  # Check for velocity_smooth or speed column
  speed_col <- NULL
  if ("velocity_smooth" %in% colnames(streams)) {
    speed_col <- "velocity_smooth"
  } else if ("speed" %in% colnames(streams)) {
    speed_col <- "speed"
  }
  
  if (!is.null(speed_col)) {
    speed <- streams[[speed_col]]
    
    # Set max speed based on sport
    max_speed <- if (tolower(sport) == "ride") max_ride_speed else max_run_speed
    
    # Flag implausible speeds
    speed_implausible <- !is.na(speed) & speed > max_speed
    
    # Flag excessive acceleration
    speed_diff <- c(0, diff(speed))
    time_diff <- c(1, diff(streams$time))
    time_diff[time_diff == 0] <- 1  # Avoid division by zero
    accel <- speed_diff / time_diff
    accel_excessive <- !is.na(accel) & abs(accel) > max_accel
    
    streams$flag_gps_drift <- speed_implausible | accel_excessive
  }
  
  # --- Aggregate Quality Flags ---
  streams$flag_any <- streams$flag_hr_spike | streams$flag_pw_spike | streams$flag_gps_drift
  
  # --- Calculate Quality Score (proportion of clean data) ---
  if (nrow(streams) > 0) {
    streams$quality_score <- 1 - (sum(streams$flag_any, na.rm = TRUE) / nrow(streams))
  }
  
  # --- Steady-State Detection ---
  # Requires sufficient data and a metric to evaluate (prefer power, then speed)
  min_samples <- min_steady_minutes * 60  # Convert to seconds
  
  if (nrow(streams) >= min_samples) {
    # Determine which metric to use for steady-state
    ss_metric <- NULL
    if ("watts" %in% colnames(streams) && any(!is.na(streams$watts))) {
      ss_metric <- streams$watts
    } else if (!is.null(speed_col) && any(!is.na(streams[[speed_col]]))) {
      ss_metric <- streams[[speed_col]]
    }
    
    if (!is.null(ss_metric)) {
      # Calculate rolling CV (coefficient of variation)
      window_size <- min_samples
      
      if (length(ss_metric) >= window_size) {
        rolling_cv <- zoo::rollapply(
          ss_metric,
          width = window_size,
          FUN = function(x) {
            if (all(is.na(x)) || mean(x, na.rm = TRUE) == 0) return(NA)
            (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
          },
          align = "center",
          fill = NA
        )
        
        # Mark as steady-state if CV is below threshold and no flags
        streams$is_steady_state <- !is.na(rolling_cv) & 
                                    rolling_cv < steady_cv_threshold &
                                    !streams$flag_any
      }
    }
  }
  
  # --- Summary Message ---
  n_flagged <- sum(streams$flag_any, na.rm = TRUE)
  n_steady <- sum(streams$is_steady_state, na.rm = TRUE)
  pct_flagged <- round(100 * n_flagged / nrow(streams), 1)
  pct_steady <- round(100 * n_steady / nrow(streams), 1)
  
  message(sprintf("Quality check complete: %.1f%% flagged, %.1f%% steady-state", 
                  pct_flagged, pct_steady))
  
  return(streams)
}


#' Get Quality Summary Statistics
#'
#' Provides a summary of quality flags and steady-state segments.
#'
#' @param flagged_streams A data frame returned by `flag_quality()`.
#'
#' @return A list with summary statistics:
#'   \describe{
#'     \item{total_points}{Total number of data points}
#'     \item{flagged_points}{Number of flagged points}
#'     \item{flagged_pct}{Percentage of flagged points}
#'     \item{steady_state_points}{Number of steady-state points}
#'     \item{steady_state_pct}{Percentage in steady-state}
#'     \item{quality_score}{Overall quality score (0-1)}
#'     \item{hr_spike_pct}{Percentage with HR spikes}
#'     \item{pw_spike_pct}{Percentage with power spikes}
#'     \item{gps_drift_pct}{Percentage with GPS drift}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flagged_data <- flag_quality(stream_data)
#' quality_summary(flagged_data)
#' }
quality_summary <- function(flagged_streams) {
  if (!is.data.frame(flagged_streams)) {
    stop("`flagged_streams` must be a data frame from flag_quality().")
  }
  
  required_cols <- c("flag_any", "is_steady_state", "quality_score")
  if (!all(required_cols %in% colnames(flagged_streams))) {
    stop("`flagged_streams` must have been processed by flag_quality().")
  }
  
  n <- nrow(flagged_streams)
  if (n == 0) {
    return(list(
      total_points = 0,
      flagged_points = 0,
      flagged_pct = 0,
      steady_state_points = 0,
      steady_state_pct = 0,
      quality_score = NA,
      hr_spike_pct = 0,
      pw_spike_pct = 0,
      gps_drift_pct = 0
    ))
  }
  
  list(
    total_points = n,
    flagged_points = sum(flagged_streams$flag_any, na.rm = TRUE),
    flagged_pct = round(100 * sum(flagged_streams$flag_any, na.rm = TRUE) / n, 2),
    steady_state_points = sum(flagged_streams$is_steady_state, na.rm = TRUE),
    steady_state_pct = round(100 * sum(flagged_streams$is_steady_state, na.rm = TRUE) / n, 2),
    quality_score = round(mean(flagged_streams$quality_score, na.rm = TRUE), 3),
    hr_spike_pct = if ("flag_hr_spike" %in% colnames(flagged_streams)) {
      round(100 * sum(flagged_streams$flag_hr_spike, na.rm = TRUE) / n, 2)
    } else 0,
    pw_spike_pct = if ("flag_pw_spike" %in% colnames(flagged_streams)) {
      round(100 * sum(flagged_streams$flag_pw_spike, na.rm = TRUE) / n, 2)
    } else 0,
    gps_drift_pct = if ("flag_gps_drift" %in% colnames(flagged_streams)) {
      round(100 * sum(flagged_streams$flag_gps_drift, na.rm = TRUE) / n, 2)
    } else 0
  )
}


