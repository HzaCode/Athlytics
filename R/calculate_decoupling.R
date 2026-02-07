# R/calculate_decoupling.R

#' Calculate Aerobic Decoupling
#'
#' Calculates aerobic decoupling for Strava activities from local export data.
#'
#' Calculates aerobic decoupling (HR drift relative to pace/power) using detailed
#' activity stream data from local FIT/TCX/GPX files.
#'
#' @param activities_data A data frame from `load_local_activities()`. Required unless `stream_df` is provided.
#' @param export_dir Base directory of Strava export containing the activities folder.
#'   Default is "strava_export_data".
#' @param activity_type Type(s) of activities to analyze (e.g., "Run", "Ride").
#' @param decouple_metric Basis for calculation: "speed_hr" or "power_hr".
#'   Note: `"pace_hr"` is accepted as a deprecated alias for `"speed_hr"`.
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to one year ago.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#' @param min_duration_mins Minimum activity duration (minutes) to include. Default 40.
#' @param min_steady_minutes Minimum duration (minutes) for steady-state segment (default: 40).
#'   Activities shorter than this are automatically rejected for decoupling calculation.
#' @param steady_cv_threshold Coefficient of variation threshold for steady-state (default: 0.08 = 8%).
#'   Activities with higher variability are rejected as non-steady-state.
#' @param min_hr_coverage Minimum HR data coverage threshold (default: 0.9 = 90%).
#'   Activities with lower HR coverage are rejected as insufficient data quality.
#' @param quality_control Quality control mode: "off" (no filtering), "flag" (mark issues),
#'   or "filter" (exclude flagged data). Default "filter" for scientific rigor.
#' @param stream_df Optional. A pre-fetched data frame for a *single* activity's stream.
#'   If provided, calculates decoupling for this data directly, ignoring other parameters.
#'   Must include columns: `time`, `heartrate`, and either `velocity_smooth`/`distance`
#'   (for speed_hr) or `watts` (for power_hr).
#' @param verbose Logical. If TRUE, prints progress messages. Default FALSE.
#'
#' @return Returns a data frame with columns:
#'   \describe{
#'     \item{date}{Activity date (Date class)}
#'     \item{decoupling}{Decoupling percentage (\\%). Positive = HR drift, negative = improved efficiency}
#'     \item{status}{Character. "ok" for successful calculation, "non_steady" if steady-state
#'       criteria not met, "insufficient_data" if data quality issues}
#'   }
#'   OR a single numeric decoupling value if `stream_df` is provided.
#'
#' @details Provides data for `plot_decoupling`. Compares output/HR efficiency
#'   between first and second halves of activities. Positive values indicate
#'   HR drift (cardiovascular drift).
#'
#'   **Best practice**: Use `load_local_activities()` to load data, then pass to this function.
#'
#'   The function parses FIT/TCX/GPX files from your Strava export to extract detailed
#'   stream data (time, heartrate, distance/power). Activities are split into two halves,
#'   and the efficiency factor (output/HR) is compared between halves.
#'
#'   **Steady-State Detection Method:**
#'
#'   Before computing decoupling, the function applies a rolling coefficient of
#'   variation (CV) filter to identify steady-state segments:
#'
#'   1. A sliding window (default 300 s) computes the rolling mean and standard
#'      deviation of the output metric (velocity or power).
#'   2. The CV (= rolling SD / rolling mean) is calculated at each time point.
#'   3. Time points with CV < `steady_cv_threshold` (default 8 %) are classified
#'      as steady-state.
#'   4. At least `min_steady_minutes` of steady-state data must be present;
#'      otherwise the activity is marked `"non_steady"`.
#'   5. Decoupling is then calculated by comparing the EF (output / HR) of the
#'      first half vs. the second half of the steady-state segment.
#'
#'   This ensures that measured decoupling reflects true cardiovascular drift
#'   rather than pacing variability or interval efforts (Coyle & González-Alonso,
#'   2001). The rolling CV approach is a standard signal-processing technique for
#'   detecting stationarity in physiological time series.
#'
#' @references
#' Coyle, E. F., & González-Alonso, J. (2001). Cardiovascular drift during
#' prolonged exercise: New perspectives. *Exercise and Sport Sciences Reviews*,
#' 29(2), 88-92. \doi{10.1097/00003677-200104000-00009}
#'
#' @importFrom dplyr first
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(sample_decoupling)
#' print(head(sample_decoupling))
#'
#' # Runnable example with dummy stream data (single activity analysis):
#' dummy_stream <- data.frame(
#'   time = 1:3600, # 1 hour
#'   heartrate = rep(140, 3600),
#'   velocity_smooth = rep(3, 3600), # 3 m/s
#'   watts = rep(200, 3600),
#'   distance = cumsum(rep(3, 3600)),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Calculate decoupling for this specific activity stream
#' result <- calculate_decoupling(
#'   stream_df = dummy_stream,
#'   decouple_metric = "speed_hr"
#' )
#' print(result)
#'
#' \dontrun{
#' # Load local activities
#' activities <- load_local_activities("strava_export_data/activities.csv")
#'
#' # Calculate Speed/HR decoupling for recent runs
#' run_decoupling <- calculate_decoupling(
#'   activities_data = activities,
#'   export_dir = "strava_export_data",
#'   activity_type = "Run",
#'   decouple_metric = "speed_hr",
#'   start_date = "2024-01-01"
#' )
#' print(tail(run_decoupling))
#'
#' # Calculate for a single activity stream
#' # stream_data <- parse_activity_file("strava_export_data/activities/12345.fit")
#' # single_decoupling <- calculate_decoupling(stream_df = stream_data, decouple_metric = "speed_hr")
#' }
calculate_decoupling <- function(activities_data = NULL,
                                 export_dir = "strava_export_data",
                                 activity_type = c("Run", "Ride"),
                                 decouple_metric = c("speed_hr", "power_hr"),
                                 start_date = NULL,
                                 end_date = Sys.Date(),
                                 min_duration_mins = 40,
                                 min_steady_minutes = 40,
                                 steady_cv_threshold = 0.08,
                                 min_hr_coverage = 0.9,
                                 quality_control = c("off", "flag", "filter"),
                                 stream_df = NULL,
                                 verbose = FALSE) {
  # --- Input Validation ---
  # Handle deprecated "pace_hr" alias
  decouple_metric_raw <- decouple_metric[1]
  if (identical(decouple_metric_raw, "pace_hr")) {
    warning('decouple_metric = "pace_hr" is deprecated. Use "speed_hr" instead (Speed / HR, not Pace / HR).', call. = FALSE)
    decouple_metric <- "speed_hr"
  } else {
    decouple_metric <- match.arg(decouple_metric)
  }

  # Normalize to lowercase (support legacy capitalized names)
  decouple_metric <- tolower(decouple_metric)

  # If stream_df provided, calculate for single activity
  if (!is.null(stream_df)) {
    result <- calculate_single_decoupling(stream_df, decouple_metric)
    # Return just the numeric value for backward compatibility
    return(result$value)
  }

  # Otherwise, need activities_data
  if (missing(activities_data) || is.null(activities_data) || !is.data.frame(activities_data)) {
    stop("`activities_data` must be provided as a data frame from load_local_activities().")
  }

  if (!is.numeric(min_duration_mins) || min_duration_mins <= 0) {
    stop("`min_duration_mins` must be a positive number.")
  }
  if (!is.numeric(min_steady_minutes) || min_steady_minutes <= 0) {
    stop("`min_steady_minutes` must be a positive number.")
  }
  if (!is.numeric(steady_cv_threshold) || steady_cv_threshold <= 0 || steady_cv_threshold > 1) {
    stop("`steady_cv_threshold` must be between 0 and 1.")
  }
  if (!is.numeric(min_hr_coverage) || min_hr_coverage <= 0 || min_hr_coverage > 1) {
    stop("`min_hr_coverage` must be between 0 and 1.")
  }

  quality_control <- match.arg(quality_control)

  # --- Date Handling ---
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()),
    error = function(e) Sys.Date()
  )
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))),
    error = function(e) analysis_end_date - lubridate::days(365)
  )

  if (analysis_start_date >= analysis_end_date) {
    stop("start_date must be before end_date.")
  }

  verbose_on <- isTRUE(verbose) || athlytics_is_verbose()

  athlytics_message(sprintf(
    "Calculating decoupling (%s) from %s to %s.",
    decouple_metric, analysis_start_date, analysis_end_date
  ), .verbose = verbose_on)

  # --- Filter Activities ---
  filtered_activities <- activities_data %>%
    dplyr::filter(
      .data$date >= analysis_start_date,
      .data$date <= analysis_end_date,
      .data$type %in% activity_type,
      (.data$moving_time / 60) >= min_duration_mins
    ) %>%
    dplyr::arrange(dplyr::desc(.data$date))

  if (nrow(filtered_activities) == 0) {
    stop("No activities found matching the specified criteria.")
  }

  athlytics_message(sprintf("Found %d activities meeting criteria. Processing...", nrow(filtered_activities)), .verbose = verbose_on)

  # --- Process Each Activity ---
  decoupling_results <- lapply(1:nrow(filtered_activities), function(i) {
    activity <- filtered_activities[i, ]

    if (is.na(activity$filename) || activity$filename == "") {
      athlytics_message(sprintf(
        "[%d/%d] Skipping activity %s (no filename)",
        i, nrow(filtered_activities), activity$date
      ), .verbose = verbose_on)
      return(NULL)
    }

    # Construct file path
    file_path <- file.path(export_dir, activity$filename)

    if (!file.exists(file_path)) {
      athlytics_message(sprintf(
        "[%d/%d] Skipping activity %s (file not found: %s)",
        i, nrow(filtered_activities), activity$date, basename(file_path)
      ), .verbose = verbose_on)
      return(NULL)
    }

    athlytics_message(sprintf(
      "[%d/%d] Processing %s (%s)",
      i, nrow(filtered_activities), activity$date, basename(file_path)
    ), .verbose = verbose_on)

    # Parse activity file
    stream_data <- tryCatch(
      {
        parse_activity_file(file_path)
      },
      error = function(e) {
        athlytics_message(sprintf("  Error parsing file: %s", e$message), .verbose = verbose_on)
        return(NULL)
      }
    )

    if (is.null(stream_data) || nrow(stream_data) == 0) {
      athlytics_message("  No stream data extracted", .verbose = verbose_on)
      return(NULL)
    }

    # Calculate decoupling for this activity
    decoupling_result <- tryCatch(
      {
        calculate_single_decoupling(stream_data, decouple_metric, quality_control, min_steady_minutes, steady_cv_threshold, min_hr_coverage)
      },
      error = function(e) {
        athlytics_message(sprintf("  Error calculating decoupling: %s", e$message), .verbose = verbose_on)
        return(list(value = NA_real_, status = "calculation_error"))
      }
    )

    # Handle both old format (just numeric) and new format (list with status)
    if (is.list(decoupling_result)) {
      decoupling_value <- decoupling_result$value
      status <- decoupling_result$status
    } else {
      decoupling_value <- decoupling_result
      status <- if (is.na(decoupling_value)) "insufficient_data" else "ok"
    }

    # Return result with status
    data.frame(
      date = activity$date,
      decoupling = decoupling_value,
      status = status,
      stringsAsFactors = FALSE
    )
  }) |> dplyr::bind_rows()

  if (is.null(decoupling_results) || nrow(decoupling_results) == 0) {
    stop("No decoupling values could be calculated. Check that activity files contain stream data.")
  }

  athlytics_message(sprintf("Successfully calculated decoupling for %d activities.", nrow(decoupling_results)), .verbose = verbose_on)

  result <- decoupling_results %>% dplyr::arrange(.data$date)

  # Add parameters as attributes
  attr(result, "params") <- list(
    activity_type = activity_type,
    decouple_metric = decouple_metric,
    min_duration_mins = min_duration_mins
  )

  # Add S3 class for type identification
  class(result) <- c("athlytics_decoupling", class(result))
  return(result)
}


#' Internal: Calculate Decoupling for Single Activity Stream
#' @keywords internal
#' @noRd
calculate_single_decoupling <- function(stream_df, decouple_metric, quality_control = "filter", min_steady_minutes = 40, steady_cv_threshold = 0.08, min_hr_coverage = 0.9) {
  # Standardize column names (some formats use heart_rate/power, others use heartrate/watts)
  if ("heart_rate" %in% colnames(stream_df) && !"heartrate" %in% colnames(stream_df)) {
    stream_df <- stream_df %>% dplyr::rename(heartrate = "heart_rate")
  }
  if ("power" %in% colnames(stream_df) && !"watts" %in% colnames(stream_df)) {
    stream_df <- stream_df %>% dplyr::rename(watts = "power")
  }

  # Validate stream_df structure
  required_cols <- c("time", "heartrate")
  if (decouple_metric == "speed_hr") {
    if (!"distance" %in% colnames(stream_df) && !"velocity_smooth" %in% colnames(stream_df)) {
      stop("For speed_hr decoupling, stream_df must contain 'distance' or 'velocity_smooth' column.")
    }
  } else { # power_hr
    if (!"watts" %in% colnames(stream_df)) {
      stop("For power_hr decoupling, stream_df must contain 'watts' column.")
    }
  }

  missing_cols <- setdiff(required_cols, colnames(stream_df))
  if (length(missing_cols) > 0) {
    stop("stream_df missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Remove NA values
  stream_clean <- stream_df %>%
    dplyr::filter(!is.na(.data$time), !is.na(.data$heartrate))

  if (nrow(stream_clean) < 100) {
    return(list(value = NA_real_, status = "insufficient_data_points"))
  }

  # Calculate velocity if needed
  if (decouple_metric == "speed_hr") {
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
    return(list(value = NA_real_, status = "insufficient_valid_data"))
  }

  # Apply quality control and steady-state gating
  if (quality_control != "off") {
    # Basic quality gates
    if (decouple_metric == "speed_hr") {
      # Check for reasonable velocity values
      stream_clean <- stream_clean %>%
        dplyr::filter(.data$velocity > 0.5, .data$velocity < 15) # 0.5-15 m/s reasonable range
    } else {
      # Check for reasonable power values
      stream_clean <- stream_clean %>%
        dplyr::filter(.data$watts > 0, .data$watts < 2000) # 0-2000W reasonable range
    }

    # Check for reasonable HR values
    stream_clean <- stream_clean %>%
      dplyr::filter(.data$heartrate > 50, .data$heartrate < 220)

    if (nrow(stream_clean) < 100) {
      return(list(value = NA_real_, status = "insufficient_data_after_quality_filter"))
    }
  }

  # Calculate HR coverage
  hr_coverage <- sum(!is.na(stream_clean$heartrate) & stream_clean$heartrate > 0) / nrow(stream_clean)
  if (hr_coverage < min_hr_coverage) {
    return(list(value = NA_real_, status = "insufficient_hr_data"))
  }

  # Find steady-state windows using rolling coefficient of variation
  window_size <- min(300, nrow(stream_clean) %/% 4) # 5-minute windows or 1/4 of data
  if (window_size < 60) window_size <- 60 # Minimum 1-minute windows

  if (decouple_metric == "speed_hr") {
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

  # Check minimum duration for steady-state periods
  if (nrow(steady_periods) > 0) {
    steady_duration_minutes <- (max(steady_periods$time, na.rm = TRUE) - min(steady_periods$time, na.rm = TRUE)) / 60
    if (steady_duration_minutes < min_steady_minutes) {
      return(list(value = NA_real_, status = "insufficient_steady_duration"))
    }
  }

  if (nrow(steady_periods) < 100) {
    return(list(value = NA_real_, status = "non_steady"))
  }

  # Split steady-state periods into two halves
  midpoint <- floor(nrow(steady_periods) / 2)
  first_half <- steady_periods[1:midpoint, ]
  second_half <- steady_periods[(midpoint + 1):nrow(steady_periods), ]

  # Calculate efficiency factor for each half from steady-state data only
  if (decouple_metric == "speed_hr") {
    ef_first <- median(first_half$velocity / first_half$heartrate, na.rm = TRUE)
    ef_second <- median(second_half$velocity / second_half$heartrate, na.rm = TRUE)
  } else {
    ef_first <- median(first_half$watts / first_half$heartrate, na.rm = TRUE)
    ef_second <- median(second_half$watts / second_half$heartrate, na.rm = TRUE)
  }

  # Calculate decoupling percentage
  if (ef_first > 0) {
    decoupling_pct <- ((ef_first - ef_second) / ef_first) * 100
    status <- "ok"
  } else {
    decoupling_pct <- NA_real_
    status <- "calculation_failed"
  }

  return(list(value = decoupling_pct, status = status))
}
