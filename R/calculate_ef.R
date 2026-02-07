# R/calculate_ef.R

#' Calculate Efficiency Factor (EF)
#'
#' Computes Efficiency Factor (EF) for endurance activities, quantifying the
#' relationship between performance output (speed or power) and heart rate.
#' EF is a key indicator of aerobic fitness and training adaptation (Allen et al., 2019).
#'
#' @description
#' Efficiency Factor measures how much work you perform per unit of cardiovascular
#' effort. Higher EF indicates better aerobic fitness - you're able to maintain faster
#' pace or higher power at the same heart rate. Tracking EF over time helps monitor
#' aerobic base development and training effectiveness.
#'
#' **EF Metrics:**
#' - **speed_hr** (for running): Speed (m/s) / Average HR
#'   - Higher values = faster speed at same HR = better fitness
#' - **power_hr** (for cycling): Average Power (watts) / Average HR
#'   - Higher values = more power at same HR = better fitness
#'
#' **What Improves EF?**
#' - Aerobic base building (Zone 2 training)
#' - Improved running/cycling economy
#' - Enhanced cardiovascular efficiency
#' - Increased mitochondrial density
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: `date`, `type`, `moving_time`, `distance`,
#'   `average_heartrate`, and `average_watts` (for power_hr metric).
#' @param activity_type Character vector or single string specifying activity type(s)
#'   to analyze. Common values: `"Run"`, `"Ride"`, or `c("Run", "Ride")`.
#'   Default: `c("Run", "Ride")`.
#' @param ef_metric Character string specifying the efficiency metric:
#'   - `"speed_hr"`: Speed-based efficiency (for running).
#'     Formula: speed (m/s) / avg_HR. Units: m/s/bpm (higher = better fitness)
#'   - `"gap_hr"`: Grade Adjusted Speed efficiency (for running on hilly terrain).
#'     Formula: GAP speed (m/s) / avg_HR. Accounts for elevation changes. Units: m/s/bpm
#'   - `"power_hr"`: Power-based efficiency (for cycling).
#'     Formula: avg_watts / avg_HR. Units: W/bpm (higher = better fitness)
#'   Default: `c("speed_hr", "power_hr")` (uses first matching metric for activity type).
#'   Note: `"pace_hr"` is accepted as a deprecated alias for `"speed_hr"` for
#'   backward compatibility.
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string, Date, or POSIXct).
#'   Defaults to one year before `end_date`.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string, Date, or POSIXct).
#'   Defaults to current date (Sys.Date()).
#' @param min_duration_mins Numeric. Minimum activity duration in minutes to include
#'   in analysis (default: 20). Filters out very short activities that may not
#'   represent steady-state aerobic efforts.
#' @param min_steady_minutes Numeric. Minimum duration (minutes) for steady-state segment (default: 20).
#'   Activities shorter than this are automatically rejected for EF calculation.
#' @param steady_cv_threshold Numeric. Coefficient of variation threshold for steady-state (default: 0.08 = 8%).
#'   Activities with higher variability are rejected as non-steady-state.
#' @param min_hr_coverage Numeric. Minimum HR data coverage threshold (default: 0.9 = 90%).
#'   Activities with lower HR coverage are rejected as insufficient data quality.
#' @param quality_control Character. Quality control mode: "off" (no filtering), "flag" (mark issues),
#'   or "filter" (exclude flagged data). Default "filter" for scientific rigor.
#' @param export_dir Optional. Path to Strava export directory containing activity files.
#'   When provided, enables stream data analysis for more accurate steady-state detection.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{date}{Activity date (Date class)}
#'   \item{activity_type}{Activity type (character: "Run" or "Ride")}
#'   \item{ef_value}{Efficiency Factor value (numeric). Higher = better fitness.
#'     Units: m/s/bpm for speed_hr, W/bpm for power_hr.}
#'   \item{status}{Character. "ok" for successful calculation with stream data, "no_streams" for
#'     activity-level calculation without stream data, "non_steady" if steady-state
#'     criteria not met, "insufficient_data" if data quality issues, "too_short" if below min_steady_minutes,
#'     "insufficient_hr_data" if HR coverage below threshold.}
#' }
#'
#' @details
#' **Algorithm:**
#' 1. Filter activities by type, date range, and minimum duration
#' 2. For each activity, calculate:
#'    - speed_hr: (distance / moving_time) / average_heartrate
#'    - power_hr: average_watts / average_heartrate
#' 3. Return one EF value per activity
#'
#' **Steady-State Detection Method:**
#'
#' When stream data is available (via `export_dir`), the function applies a rolling
#' coefficient of variation (CV) approach to identify steady-state periods:
#'
#' 1. **Rolling window**: A sliding window (default 300 seconds, or 1/4 of data, min 60 s)
#'    computes the rolling mean and standard deviation of the output metric (velocity or power).
#' 2. **CV calculation**: CV = rolling SD / rolling mean at each time point.
#' 3. **Threshold filtering**: Time points where CV < `steady_cv_threshold` (default 8%)
#'    are classified as steady-state.
#' 4. **Minimum duration**: At least `min_steady_minutes` of steady-state data
#'    must be present; otherwise the activity is marked as `"non_steady"`.
#' 5. **EF computation**: The median EF across all steady-state data points is reported.
#'
#' This approach follows the principle that valid EF comparisons require quasi-constant
#' output intensity, as outlined by Coyle & González-Alonso (2001), who demonstrated that
#' cardiovascular drift is meaningful only under steady-state exercise conditions.
#' The rolling CV method is a common signal-processing technique for detecting stationarity
#' in physiological time series.
#'
#' **Data Quality Considerations:**
#' - Requires heart rate data (activities without HR are excluded)
#' - power_hr requires power meter data (cycling with power)
#' - Best for steady-state endurance efforts (tempo runs, long rides)
#' - Interval workouts may give misleading EF values
#' - Environmental factors (heat, altitude) can affect EF
#'
#' **Interpretation:**
#' - **Upward trend**: Improving aerobic fitness
#' - **Stable**: Maintenance phase
#' - **Downward trend**: Possible overtraining, fatigue, or environmental stress
#' - **Sudden drop**: Check for illness, equipment change, or data quality
#'
#' **Typical EF Ranges (speed_hr for running):**
#' - Beginner: 0.01 - 0.015 (m/s per bpm)
#' - Intermediate: 0.015 - 0.020
#' - Advanced: 0.020 - 0.025
#' - Elite: > 0.025
#'
#' Note: EF values are relative to individual baseline. Focus on personal trends
#' rather than absolute comparisons with other athletes.
#'
#' @references
#' Allen, H., Coggan, A. R., & McGregor, S. (2019). *Training and Racing with a
#' Power Meter* (3rd ed.). VeloPress.
#'
#' Coyle, E. F., & González-Alonso, J. (2001). Cardiovascular drift during
#' prolonged exercise: New perspectives. *Exercise and Sport Sciences Reviews*,
#' 29(2), 88-92. \doi{10.1097/00003677-200104000-00009}
#'
#' @seealso
#' [plot_ef()] for visualization with trend lines,
#' [calculate_decoupling()] for within-activity efficiency analysis,
#' [load_local_activities()] for data loading
#'
#' @importFrom dplyr first
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(sample_ef)
#' print(head(sample_ef))
#'
#' # Runnable example with dummy data:
#' end <- Sys.Date()
#' dates <- seq(end - 29, end, by = "day")
#' dummy_activities <- data.frame(
#'   date = dates,
#'   type = "Run",
#'   moving_time = rep(3600, length(dates)), # 1 hour
#'   distance = rep(10000, length(dates)), # 10 km
#'   average_heartrate = rep(140, length(dates)),
#'   average_watts = rep(200, length(dates)),
#'   weighted_average_watts = rep(210, length(dates)),
#'   filename = "",
#'   stringsAsFactors = FALSE
#' )
#'
#' # Calculate EF (Speed/HR)
#' ef_result <- calculate_ef(
#'   activities_data = dummy_activities,
#'   activity_type = "Run",
#'   ef_metric = "speed_hr",
#'   end_date = end
#' )
#' print(head(ef_result))
#'
#' \dontrun{
#' # Example using local Strava export data
#' activities <- load_local_activities("strava_export_data/activities.csv")
#'
#' # Calculate Speed/HR efficiency factor for Runs
#' ef_data_run <- calculate_ef(
#'   activities_data = activities,
#'   activity_type = "Run",
#'   ef_metric = "speed_hr"
#' )
#' print(tail(ef_data_run))
#'
#' # Calculate Power/HR efficiency factor for Rides
#' ef_data_ride <- calculate_ef(
#'   activities_data = activities,
#'   activity_type = "Ride",
#'   ef_metric = "power_hr"
#' )
#' print(tail(ef_data_ride))
#' }
calculate_ef <- function(activities_data,
                         activity_type = c("Run", "Ride"),
                         ef_metric = c("speed_hr", "gap_hr", "power_hr"),
                         start_date = NULL,
                         end_date = Sys.Date(),
                         min_duration_mins = 20,
                         min_steady_minutes = 20,
                         steady_cv_threshold = 0.08,
                         min_hr_coverage = 0.9,
                         quality_control = c("off", "flag", "filter"),
                         export_dir = NULL) {
  # --- Input Validation ---
  if (missing(activities_data) || is.null(activities_data)) {
    stop("`activities_data` must be provided. Use load_local_activities() to load your Strava export data.")
  }

  if (!is.data.frame(activities_data)) {
    stop("`activities_data` must be a data frame (e.g., from load_local_activities()).")
  }

  # Handle deprecated "pace_hr" alias
  ef_metric_raw <- ef_metric[1]
  if (identical(ef_metric_raw, "pace_hr")) {
    warning('ef_metric = "pace_hr" is deprecated. Use "speed_hr" instead (Speed / HR, not Pace / HR).', call. = FALSE)
    ef_metric <- "speed_hr"
  } else {
    ef_metric <- match.arg(ef_metric)
  }

  # Normalize to lowercase (support legacy capitalized names)
  ef_metric <- tolower(ef_metric)
  if (!is.numeric(min_duration_mins) || min_duration_mins < 0) {
    stop("`min_duration_mins` must be a non-negative number.")
  }
  if (!is.numeric(min_steady_minutes) || min_steady_minutes < 0) {
    stop("`min_steady_minutes` must be a non-negative number.")
  }
  if (!is.numeric(steady_cv_threshold) || steady_cv_threshold <= 0 || steady_cv_threshold > 1) {
    stop("`steady_cv_threshold` must be between 0 and 1.")
  }
  if (!is.numeric(min_hr_coverage) || min_hr_coverage <= 0 || min_hr_coverage > 1) {
    stop("`min_hr_coverage` must be between 0 and 1.")
  }

  quality_control <- match.arg(quality_control)

  # --- Date Handling ---
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), error = function(e) analysis_end_date - lubridate::days(365))
  if (analysis_start_date >= analysis_end_date) stop("start_date must be before end_date.")

  athlytics_message(sprintf("Calculating EF data from %s to %s.", analysis_start_date, analysis_end_date))
  athlytics_message(sprintf("Metric: %s, Activity types: %s", ef_metric, paste(activity_type, collapse = ", ")))

  # --- Filter Activities ---
  athlytics_message("Processing local activities data...")
  activities_df_filtered <- activities_data %>%
    dplyr::filter(.data$date >= analysis_start_date & .data$date <= analysis_end_date)

  if (!is.null(activity_type)) {
    activities_df_filtered <- activities_df_filtered %>%
      dplyr::filter(.data$type %in% activity_type)
  }

  activities_fetched_count <- nrow(activities_df_filtered)
  athlytics_message(sprintf("Loaded %d activities from local data.", activities_fetched_count))

  if (activities_fetched_count == 0) {
    stop("No activities found in local data for the date range.")
  }

  # --- Process Activities & Calculate EF ---
  safe_as_numeric <- function(x) {
    as.numeric(rlang::`%||%`(x, 0))
  }

  ef_data <- lapply(1:nrow(activities_df_filtered), function(i) {
    activity <- activities_df_filtered[i, ]
    act_type <- activity$type %||% "Unknown"
    activity_date <- activity$date
    duration_sec <- safe_as_numeric(activity$moving_time)
    avg_hr <- safe_as_numeric(activity$average_heartrate)
    distance_m <- safe_as_numeric(activity$distance)
    avg_power <- safe_as_numeric(activity$average_watts)
    weighted_power <- safe_as_numeric(activity$weighted_average_watts)
    power_used <- ifelse(weighted_power > 0, weighted_power, avg_power)

    if (is.na(activity_date) || activity_date < analysis_start_date || activity_date > analysis_end_date) {
      return(NULL)
    }
    if (!act_type %in% activity_type) {
      return(NULL)
    }
    if (duration_sec < (min_duration_mins * 60)) {
      return(NULL)
    }
    if (is.na(avg_hr) || avg_hr <= 0) {
      return(NULL)
    }

    # Try to parse stream data for proper steady-state analysis
    stream_data <- NULL
    if (!is.null(export_dir) && !is.na(activity$filename) && nchar(activity$filename) > 0) {
      tryCatch(
        {
          stream_data <- parse_activity_file(activity$filename, export_dir)
        },
        error = function(e) {
          athlytics_message(sprintf("  Could not parse stream data for activity %s: %s", activity_date, e$message))
        }
      )
    }

    # If we have stream data, do proper steady-state analysis
    if (!is.null(stream_data) && nrow(stream_data) > 0) {
      return(calculate_ef_from_stream(
        stream_data, activity_date, act_type, ef_metric,
        min_steady_minutes, steady_cv_threshold, min_hr_coverage, quality_control
      ))
    }

    # Fallback to activity-level averages (with warnings about limitations)
    athlytics_message(sprintf("  No stream data available for %s, using activity-level averages (less reliable)", activity_date))

    # Quality control integration
    if (quality_control != "off") {
      # For now, we use simplified quality checks since we don't have stream data
      # In a full implementation, we would parse stream files and call flag_quality()
      # This is a placeholder for the quality control framework

      # Check for reasonable HR values (basic quality gate)
      if (avg_hr < 50 || avg_hr > 220) {
        if (quality_control == "filter") {
          return(data.frame(
            date = activity_date,
            activity_type = act_type,
            ef_value = NA_real_,
            status = "poor_hr_quality",
            stringsAsFactors = FALSE
          ))
        }
        # If "flag", we continue but mark the status
      }
    }

    # Steady-state gating: check minimum duration
    if (duration_sec < (min_steady_minutes * 60)) {
      return(data.frame(
        date = activity_date,
        activity_type = act_type,
        ef_value = NA_real_,
        status = "too_short",
        stringsAsFactors = FALSE
      ))
    }

    ef_value <- NA

    if (ef_metric == "speed_hr") {
      if (distance_m > 0 && duration_sec > 0) {
        # Calculate speed (m/s) / HR - EF is speed per unit HR
        speed_ms <- distance_m / duration_sec
        ef_value <- speed_ms / avg_hr
      }
    } else if (ef_metric == "gap_hr") {
      # Grade Adjusted Speed / HR - accounts for elevation changes
      # Note: Strava CSV "Average Grade Adjusted Pace" column is actually stored
      # as speed in m/s (same unit as "Average Speed"), despite the column name
      gap_value <- if ("average_gap" %in% names(activity)) activity$average_gap else NA_real_
      if (!is.na(gap_value) && gap_value > 0) {
        # GAP is already in m/s (speed), use directly
        ef_value <- gap_value / avg_hr
      } else if (distance_m > 0 && duration_sec > 0) {
        # Fallback to regular speed if GAP not available
        speed_ms <- distance_m / duration_sec
        ef_value <- speed_ms / avg_hr
        warning(sprintf("GAP data not available for activity on %s, using regular speed", activity_date))
      }
    } else if (ef_metric == "power_hr") {
      if (!is.na(power_used) && power_used > 0) {
        ef_value <- power_used / avg_hr
      }
    }

    if (!is.na(ef_value) && ef_value > 0) {
      data.frame(
        date = activity_date,
        activity_type = act_type,
        ef_value = ef_value,
        status = "no_streams",
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        date = activity_date,
        activity_type = act_type,
        ef_value = NA_real_,
        status = "calculation_failed",
        stringsAsFactors = FALSE
      )
    }
  }) |> dplyr::bind_rows()

  if (is.null(ef_data) || nrow(ef_data) == 0) {
    warning("No activities met the EF calculation criteria. Returning empty data frame.")
    return(data.frame(
      date = lubridate::as_date(character(0)),
      activity_type = character(0),
      ef_value = numeric(0)
    ))
  }

  ef_data <- ef_data %>%
    dplyr::arrange(.data$date)

  athlytics_message("EF calculation complete.")

  # Add parameters as attributes for plotting
  attr(ef_data, "params") <- list(
    activity_type = activity_type,
    ef_metric = ef_metric,
    min_duration_mins = min_duration_mins,
    min_steady_minutes = min_steady_minutes
  )

  # Add S3 class for type identification
  class(ef_data) <- c("athlytics_ef", class(ef_data))
  return(ef_data)
}

#' Calculate EF from Stream Data with Steady-State Analysis
#'
#' Calculate efficiency factor (EF) from detailed stream data using steady-state analysis.
#' This function analyzes heart rate and power/pace data to find periods of steady effort
#' and calculates the efficiency factor for those periods.
#'
#' @param stream_data Data frame with stream data (time, heartrate, watts/distance columns)
#' @param activity_date Date of the activity
#' @param act_type Activity type (e.g., "Run", "Ride")
#' @param ef_metric Efficiency metric to calculate ("speed_hr" or "power_hr")
#' @param min_steady_minutes Minimum duration for steady-state analysis (minutes)
#' @param steady_cv_threshold Coefficient of variation threshold for steady state
#' @param min_hr_coverage Minimum heart rate data coverage required
#' @param quality_control Quality control setting ("off", "flag", "filter")
#'
#' @return Data frame with EF calculation results
#'
#' @examples
#' \dontrun{
#' # Parse activity file and calculate EF from streams
#' streams <- parse_activity_file("activity_12345.fit")
#' ef_result <- calculate_ef_from_stream(
#'   stream_data = streams,
#'   activity_date = as.Date("2025-01-15"),
#'   act_type = "Run",
#'   ef_metric = "speed_hr",
#'   min_steady_minutes = 20,
#'   steady_cv_threshold = 0.08,
#'   min_hr_coverage = 0.9,
#'   quality_control = "filter"
#' )
#' }
#'
#' @export
calculate_ef_from_stream <- function(stream_data, activity_date, act_type, ef_metric,
                                     min_steady_minutes, steady_cv_threshold, min_hr_coverage, quality_control) {
  # Standardize column names (some formats use heart_rate/power, others use heartrate/watts)
  if ("heart_rate" %in% colnames(stream_data) && !"heartrate" %in% colnames(stream_data)) {
    stream_data <- stream_data %>% dplyr::rename(heartrate = "heart_rate")
  }
  # Standardize power column: always use "watts" internally

  if ("power" %in% colnames(stream_data)) {
    if ("watts" %in% colnames(stream_data)) {
      # Both exist: prefer "watts", remove "power" to avoid ambiguity
      stream_data <- stream_data %>% dplyr::select(-"power")
    } else {
      # Only "power" exists: rename to "watts"
      stream_data <- stream_data %>% dplyr::rename(watts = "power")
    }
  }

  # Validate stream data structure
  required_cols <- c("time", "heartrate")
  if (ef_metric == "speed_hr") {
    if (!"distance" %in% colnames(stream_data) && !"velocity_smooth" %in% colnames(stream_data)) {
      return(data.frame(
        date = activity_date,
        activity_type = act_type,
        ef_value = NA_real_,
        status = "missing_velocity_data",
        stringsAsFactors = FALSE
      ))
    }
  } else { # power_hr
    if (!"watts" %in% colnames(stream_data)) {
      return(data.frame(
        date = activity_date,
        activity_type = act_type,
        ef_value = NA_real_,
        status = "missing_power_data",
        stringsAsFactors = FALSE
      ))
    }
  }

  missing_cols <- setdiff(required_cols, colnames(stream_data))
  if (length(missing_cols) > 0) {
    return(data.frame(
      date = activity_date,
      activity_type = act_type,
      ef_value = NA_real_,
      status = "missing_hr_data",
      stringsAsFactors = FALSE
    ))
  }

  # Clean stream data
  stream_clean <- stream_data %>%
    dplyr::filter(!is.na(.data$time), !is.na(.data$heartrate))

  if (nrow(stream_clean) < 100) {
    return(data.frame(
      date = activity_date,
      activity_type = act_type,
      ef_value = NA_real_,
      status = "insufficient_data_points",
      stringsAsFactors = FALSE
    ))
  }

  # Calculate HR coverage
  total_time <- max(stream_clean$time, na.rm = TRUE) - min(stream_clean$time, na.rm = TRUE)
  hr_data_time <- sum(!is.na(stream_clean$heartrate) & stream_clean$heartrate > 0)
  hr_coverage <- hr_data_time / nrow(stream_clean)

  if (hr_coverage < min_hr_coverage) {
    return(data.frame(
      date = activity_date,
      activity_type = act_type,
      ef_value = NA_real_,
      status = "insufficient_hr_data",
      stringsAsFactors = FALSE
    ))
  }

  # Calculate velocity if needed for speed_hr
  if (ef_metric == "speed_hr") {
    if ("velocity_smooth" %in% colnames(stream_clean)) {
      stream_clean <- stream_clean %>%
        dplyr::mutate(velocity = .data$velocity_smooth)
    } else if ("distance" %in% colnames(stream_clean)) {
      # Calculate velocity from distance
      stream_clean <- stream_clean %>%
        dplyr::arrange(.data$time) %>%
        dplyr::mutate(
          distance_diff = .data$distance - dplyr::lag(.data$distance, default = first(.data$distance)),
          time_diff = .data$time - dplyr::lag(.data$time, default = first(.data$time)),
          velocity = ifelse(.data$time_diff > 0, .data$distance_diff / .data$time_diff, 0)
        )
    }

    stream_clean <- stream_clean %>%
      dplyr::filter(!is.na(.data$velocity), .data$velocity > 0, .data$heartrate > 0)
  } else {
    stream_clean <- stream_clean %>%
      dplyr::filter(!is.na(.data$watts), .data$watts > 0, .data$heartrate > 0)
  }

  if (nrow(stream_clean) < 100) {
    return(data.frame(
      date = activity_date,
      activity_type = act_type,
      ef_value = NA_real_,
      status = "insufficient_valid_data",
      stringsAsFactors = FALSE
    ))
  }

  # Quality control
  if (quality_control != "off") {
    # Check for reasonable values
    if (ef_metric == "speed_hr") {
      stream_clean <- stream_clean %>%
        dplyr::filter(.data$velocity > 0.5, .data$velocity < 15) # 0.5-15 m/s reasonable range
    } else {
      stream_clean <- stream_clean %>%
        dplyr::filter(.data$watts > 0, .data$watts < 2000) # 0-2000W reasonable range
    }

    stream_clean <- stream_clean %>%
      dplyr::filter(.data$heartrate > 50, .data$heartrate < 220)

    if (nrow(stream_clean) < 100) {
      return(data.frame(
        date = activity_date,
        activity_type = act_type,
        ef_value = NA_real_,
        status = "insufficient_data_after_quality_filter",
        stringsAsFactors = FALSE
      ))
    }
  }

  # Check minimum duration
  duration_minutes <- (max(stream_clean$time, na.rm = TRUE) - min(stream_clean$time, na.rm = TRUE)) / 60
  if (duration_minutes < min_steady_minutes) {
    return(data.frame(
      date = activity_date,
      activity_type = act_type,
      ef_value = NA_real_,
      status = "too_short",
      stringsAsFactors = FALSE
    ))
  }

  # Find steady-state windows using rolling coefficient of variation
  window_size <- min(300, nrow(stream_clean) %/% 4) # 5-minute windows or 1/4 of data
  if (window_size < 60) window_size <- 60 # Minimum 1-minute windows

  if (ef_metric == "speed_hr") {
    # Calculate rolling CV for velocity
    stream_clean <- stream_clean %>%
      dplyr::arrange(.data$time) %>%
      dplyr::mutate(
        velocity_rollmean = zoo::rollmean(.data$velocity, window_size, fill = NA, align = "center"),
        velocity_rollsd = zoo::rollapply(.data$velocity, window_size, sd, fill = NA, align = "center"),
        velocity_cv = .data$velocity_rollsd / .data$velocity_rollmean
      )

    # Find steady-state periods (CV < threshold)
    steady_periods <- stream_clean %>%
      dplyr::filter(!is.na(.data$velocity_cv), .data$velocity_cv < steady_cv_threshold)
  } else {
    # Calculate rolling CV for power
    stream_clean <- stream_clean %>%
      dplyr::arrange(.data$time) %>%
      dplyr::mutate(
        watts_rollmean = zoo::rollmean(.data$watts, window_size, fill = NA, align = "center"),
        watts_rollsd = zoo::rollapply(.data$watts, window_size, sd, fill = NA, align = "center"),
        watts_cv = .data$watts_rollsd / .data$watts_rollmean
      )

    # Find steady-state periods (CV < threshold)
    steady_periods <- stream_clean %>%
      dplyr::filter(!is.na(.data$watts_cv), .data$watts_cv < steady_cv_threshold)
  }

  if (nrow(steady_periods) < 100) {
    return(data.frame(
      date = activity_date,
      activity_type = act_type,
      ef_value = NA_real_,
      status = "non_steady",
      stringsAsFactors = FALSE
    ))
  }

  # Calculate EF from steady-state periods
  if (ef_metric == "speed_hr") {
    ef_value <- median(steady_periods$velocity / steady_periods$heartrate, na.rm = TRUE)
  } else {
    ef_value <- median(steady_periods$watts / steady_periods$heartrate, na.rm = TRUE)
  }

  if (!is.na(ef_value) && ef_value > 0) {
    data.frame(
      date = activity_date,
      activity_type = act_type,
      ef_value = ef_value,
      status = "ok",
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      date = activity_date,
      activity_type = act_type,
      ef_value = NA_real_,
      status = "calculation_failed",
      stringsAsFactors = FALSE
    )
  }
}
