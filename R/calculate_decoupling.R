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
#' @param return_diagnostics Logical. Only consulted when `stream_df` is
#'   supplied. When `FALSE` (default) the function returns a bare numeric
#'   decoupling value for backward compatibility with early releases. When
#'   `TRUE` it returns a one-row data frame with the same columns as the
#'   activities-level path (`decoupling`, `status`, `quality_score`,
#'   `hr_coverage`, `steady_duration_minutes`, `sampling_interval_seconds`)
#'   so callers can distinguish rejection reasons from a genuine `NA`
#'   decoupling value.
#' @param verbose Logical. If TRUE, prints progress messages. Default FALSE.
#'
#' @return Returns a data frame with columns:
#'   \describe{
#'     \item{date}{Activity date (Date class)}
#'     \item{decoupling}{Decoupling percentage (\\%). Positive = HR drift, negative = improved efficiency}
#'     \item{status}{Character status code describing the outcome of the
#'       calculation. See **Status vocabulary** below.}
#'     \item{quality_score}{Numeric in \[0, 1\]. Fraction of stream samples
#'       that passed quality-control range checks. `NA` if the activity was
#'       rejected before the QC stage.}
#'     \item{hr_coverage}{Numeric in \[0, 1\]. Time-weighted fraction of the
#'       stream that carried a valid heart-rate sample.}
#'     \item{steady_duration_minutes}{Wall-clock duration of the contiguous
#'       steady-state block the decoupling was derived from. `NA` when no
#'       qualifying block existed.}
#'     \item{sampling_interval_seconds}{Observed median sampling interval of
#'       the stream (seconds). Useful for auditing whether the rolling-CV
#'       window was well-calibrated.}
#'   }
#'   When `stream_df` is provided the default return is a single numeric
#'   decoupling value (backward-compatible with early releases). Pass
#'   `return_diagnostics = TRUE` to get the full one-row diagnostics frame
#'   instead.
#'
#' @section Status vocabulary:
#' - `"ok"`: Decoupling computed from a contiguous steady-state block.
#' - `"missing_hr_data"`: Stream lacked a `heartrate` / `heart_rate` column.
#' - `"missing_velocity_data"` / `"missing_power_data"`: Stream lacked the
#'   column required by the chosen `decouple_metric`.
#' - `"insufficient_hr_data"`: Time-weighted HR coverage < `min_hr_coverage`.
#' - `"insufficient_data_points"`: Fewer than 100 non-NA stream samples.
#' - `"insufficient_valid_data"`: Fewer than 100 samples survived basic
#'   positivity filtering (`velocity > 0` or `watts > 0`).
#' - `"insufficient_data_after_quality_filter"`: `quality_control = "filter"`
#'   removed enough out-of-range samples to drop the stream below 100 rows.
#' - `"insufficient_steady_duration"`: No contiguous steady-state block met
#'   the `min_steady_minutes` threshold.
#' - `"non_steady"`: No rolling-CV windows cleared `steady_cv_threshold`, or
#'   time-midpoint split produced an empty half.
#' - `"calculation_failed"`: Median first-half EF was non-positive or not
#'   finite, so decoupling could not be expressed as a percentage.
#' - `"calculation_error"`: An exception was raised during per-activity
#'   processing (caught by the outer loop).
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
                                 return_diagnostics = FALSE,
                                 verbose = FALSE) {
  # --- Input Validation ---
  # Normalize case first so legacy capitalized names (e.g. "Speed_HR") survive
  # match.arg(), which is case-sensitive by default.
  decouple_metric_raw <- decouple_metric[1]
  decouple_metric_norm <- if (is.character(decouple_metric_raw)) {
    tolower(decouple_metric_raw)
  } else {
    decouple_metric_raw
  }

  if (identical(decouple_metric_norm, "pace_hr")) {
    warning('decouple_metric = "pace_hr" is deprecated. Use "speed_hr" instead (Speed / HR, not Pace / HR).', call. = FALSE)
    decouple_metric <- "speed_hr"
  } else {
    decouple_metric <- match.arg(decouple_metric_norm, c("speed_hr", "power_hr"))
  }

  # Validate parameters that are used by both stream_df and activities_data paths.
  # Previously these validations (and quality_control match.arg) ran only after
  # the stream_df early-return, so stream_df callers silently got defaults.
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

  # If stream_df provided, calculate for single activity
  if (!is.null(stream_df)) {
    result <- calculate_single_decoupling(
      stream_df = stream_df,
      decouple_metric = decouple_metric,
      quality_control = quality_control,
      min_steady_minutes = min_steady_minutes,
      steady_cv_threshold = steady_cv_threshold,
      min_hr_coverage = min_hr_coverage
    )
    if (isTRUE(return_diagnostics)) {
      # Expose the full diagnostics record so callers can distinguish
      # "NA = no steady block" from "NA = no HR coverage" etc. without
      # having to re-run the stream through calculate_single_decoupling()
      # directly. Older callers that default to FALSE still receive a
      # bare numeric value.
      return(data.frame(
        decoupling = result$value,
        status = result$status %||% NA_character_,
        quality_score = result$quality_score %||% NA_real_,
        hr_coverage = result$hr_coverage %||% NA_real_,
        steady_duration_minutes = result$steady_duration_minutes %||% NA_real_,
        sampling_interval_seconds = result$sampling_interval_seconds %||% NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    # Default behaviour preserved for backward compatibility: a single
    # numeric decoupling percentage (or NA if the activity was rejected).
    return(result$value)
  }

  # Otherwise, need activities_data
  if (missing(activities_data) || is.null(activities_data) || !is.data.frame(activities_data)) {
    stop("`activities_data` must be provided as a data frame from load_local_activities().")
  }

  if (!is.numeric(min_duration_mins) || min_duration_mins <= 0) {
    stop("`min_duration_mins` must be a positive number.")
  }

  # --- Export directory / zip validation ---
  is_zip_export <- is.character(export_dir) &&
    length(export_dir) == 1 &&
    file.exists(export_dir) &&
    tolower(tools::file_ext(export_dir)) == "zip"
  is_dir_export <- is.character(export_dir) &&
    length(export_dir) == 1 &&
    dir.exists(export_dir)

  if (!is_zip_export && !is_dir_export) {
    stop("`export_dir` must be an existing directory or a .zip file: ", export_dir)
  }

  # --- Date Handling ---
  analysis_end_date <- parse_analysis_date(end_date, default = Sys.Date(), arg_name = "end_date")
  analysis_start_date <- parse_analysis_date(
    start_date,
    default = analysis_end_date - lubridate::days(365),
    arg_name = "start_date"
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

    athlytics_message(sprintf(
      "[%d/%d] Processing %s (%s)",
      i, nrow(filtered_activities), activity$date, basename(activity$filename)
    ), .verbose = verbose_on)

    # Parse activity file. parse_activity_file() resolves both directory and
    # .zip export_dir values via its zip-aware logic, so we pass export_dir
    # straight through rather than pre-building a path with file.path().
    stream_data <- tryCatch(
      {
        suppressWarnings(parse_activity_file(activity$filename, export_dir))
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

    # Handle both old format (just numeric) and new format (list with status,
    # optionally carrying quality_score, hr_coverage, and the newly-added
    # steady_duration_minutes / sampling_interval_seconds diagnostics).
    if (is.list(decoupling_result)) {
      decoupling_value <- decoupling_result$value
      status <- decoupling_result$status
      quality_score <- decoupling_result$quality_score %||% NA_real_
      hr_coverage <- decoupling_result$hr_coverage %||% NA_real_
      steady_duration_minutes <- decoupling_result$steady_duration_minutes %||% NA_real_
      sampling_interval_seconds <- decoupling_result$sampling_interval_seconds %||% NA_real_
    } else {
      decoupling_value <- decoupling_result
      status <- if (is.na(decoupling_value)) "insufficient_data" else "ok"
      quality_score <- NA_real_
      hr_coverage <- NA_real_
      steady_duration_minutes <- NA_real_
      sampling_interval_seconds <- NA_real_
    }

    # Return result with status plus per-activity QC + steady-state metadata.
    # Downstream consumers (plots, cohort comparisons, audits) can now see
    # *why* an activity was accepted or rejected and the effective sampling
    # frequency of its stream without re-running the analysis.
    data.frame(
      date = activity$date,
      decoupling = decoupling_value,
      status = status,
      quality_score = quality_score,
      hr_coverage = hr_coverage,
      steady_duration_minutes = steady_duration_minutes,
      sampling_interval_seconds = sampling_interval_seconds,
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
  # Uniform diagnostics record. Every rejection path (and the success path)
  # returns a list with the same fields so the outer activities loop can
  # rbind them without special-casing missing keys. This is also what
  # `return_diagnostics = TRUE` exposes to users.
  make_rec <- function(value = NA_real_, status = NA_character_,
                       hr_coverage = NA_real_, quality_score = NA_real_,
                       steady_duration_minutes = NA_real_,
                       sampling_interval_seconds = NA_real_) {
    list(
      value = value,
      status = status,
      hr_coverage = hr_coverage,
      quality_score = quality_score,
      steady_duration_minutes = steady_duration_minutes,
      sampling_interval_seconds = sampling_interval_seconds
    )
  }

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

  # Estimate the stream's median sampling interval once so the rolling-CV
  # window, continuous-block duration and min_steady_minutes gate all
  # operate in wall-clock units regardless of recording frequency.
  sampling_interval_seconds <- estimate_sampling_interval(stream_df, default = 1)

  # HR coverage must be measured against the ORIGINAL stream and weighted by
  # time (not row count). Row-fraction coverage overweights densely-sampled
  # sections and cannot catch streams where the watch stopped recording HR
  # during long time gaps.
  hr_coverage_raw <- NA_real_
  if (nrow(stream_df) > 0) {
    hr_coverage_raw <- time_weighted_coverage(stream_df, "heartrate")
    if (hr_coverage_raw < min_hr_coverage) {
      return(make_rec(
        status = "insufficient_hr_data",
        hr_coverage = hr_coverage_raw,
        sampling_interval_seconds = sampling_interval_seconds
      ))
    }
  }

  # Remove NA values
  stream_clean <- stream_df %>%
    dplyr::filter(!is.na(.data$time), !is.na(.data$heartrate))

  if (nrow(stream_clean) < 100) {
    return(make_rec(
      status = "insufficient_data_points",
      hr_coverage = hr_coverage_raw,
      sampling_interval_seconds = sampling_interval_seconds
    ))
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
    return(make_rec(
      status = "insufficient_valid_data",
      hr_coverage = hr_coverage_raw,
      sampling_interval_seconds = sampling_interval_seconds
    ))
  }

  # Apply quality control and steady-state gating
  quality_score <- 1.0
  if (quality_control != "off") {
    if (decouple_metric == "speed_hr") {
      flag_output <- !is.na(stream_clean$velocity) &
        (stream_clean$velocity <= 0.5 | stream_clean$velocity >= 15)
    } else {
      flag_output <- !is.na(stream_clean$watts) &
        (stream_clean$watts <= 0 | stream_clean$watts >= 2000)
    }
    flag_hr <- !is.na(stream_clean$heartrate) &
      (stream_clean$heartrate <= 50 | stream_clean$heartrate >= 220)
    flag_any <- flag_output | flag_hr
    n_flagged <- sum(flag_any, na.rm = TRUE)
    quality_score <- if (length(flag_any) > 0) {
      1 - n_flagged / length(flag_any)
    } else {
      NA_real_
    }

    if (quality_control == "filter") {
      stream_clean <- stream_clean[!flag_any, , drop = FALSE]
      if (nrow(stream_clean) < 100) {
        return(make_rec(
          status = "insufficient_data_after_quality_filter",
          hr_coverage = hr_coverage_raw,
          quality_score = quality_score,
          sampling_interval_seconds = sampling_interval_seconds
        ))
      }
    } else if (quality_control == "flag" && n_flagged > 0) {
      # Report flags but keep rows; median-based decoupling is robust to a
      # small fraction of out-of-range samples.
      athlytics_message(sprintf(
        "  Quality flag: %d of %d stream points outside reasonable range (kept; quality_control = 'flag').",
        n_flagged, length(flag_any)
      ))
    }
  }

  # (HR coverage was already validated against the original stream above,
  # before the NA filter, so a second post-filter check would always be ~1.0.)

  # Find steady-state windows using rolling coefficient of variation. Window
  # width is targeted at 5 minutes of wall-clock time (not rows) so smart-
  # recording and multi-Hz streams produce comparable CVs.
  stream_clean <- stream_clean %>% dplyr::arrange(.data$time)
  window_size <- time_based_window_size(
    stream_clean,
    window_seconds = 300,
    min_rows = max(60L, ceiling(60 / sampling_interval_seconds)),
    max_rows = nrow(stream_clean) %/% 4
  )

  # Rolling CV (use a unified metric_cv column so the continuous-block logic
  # below doesn't need to branch on decouple_metric twice).
  metric_vec <- if (decouple_metric == "speed_hr") stream_clean$velocity else stream_clean$watts

  metric_rollmean <- zoo::rollmean(metric_vec, window_size, fill = NA, align = "center")
  metric_rollsd <- zoo::rollapply(metric_vec, window_size, sd, fill = NA, align = "center")
  metric_cv <- metric_rollsd / metric_rollmean

  stream_clean$metric_cv <- metric_cv
  stream_clean$is_steady <- !is.na(metric_cv) & metric_cv < steady_cv_threshold

  if (!any(stream_clean$is_steady)) {
    return(make_rec(
      status = "non_steady",
      hr_coverage = hr_coverage_raw,
      quality_score = quality_score,
      sampling_interval_seconds = sampling_interval_seconds
    ))
  }

  # Identify contiguous steady-state blocks. Prior versions collected every
  # steady point across the whole activity and treated separated islands as one
  # block, which inflated max-min "duration" and mixed unrelated segments into
  # the first/second-half split.
  runs <- rle(stream_clean$is_steady)
  run_ends <- cumsum(runs$lengths)
  run_starts <- c(1L, utils::head(run_ends, -1) + 1L)
  time_num <- as.numeric(stream_clean$time)

  run_duration_min <- ifelse(
    runs$values,
    (time_num[run_ends] - time_num[run_starts]) / 60,
    0
  )
  run_sample_count <- ifelse(runs$values, runs$lengths, 0L)

  qualifying <- which(runs$values &
    run_duration_min >= min_steady_minutes &
    run_sample_count >= 100)

  if (length(qualifying) == 0) {
    return(make_rec(
      status = "insufficient_steady_duration",
      hr_coverage = hr_coverage_raw,
      quality_score = quality_score,
      sampling_interval_seconds = sampling_interval_seconds
    ))
  }

  best_run_i <- qualifying[which.max(run_duration_min[qualifying])]
  steady_block <- stream_clean[run_starts[best_run_i]:run_ends[best_run_i], , drop = FALSE]
  steady_duration_minutes <- run_duration_min[best_run_i]

  # Split by time midpoint (not row midpoint) so irregular sampling does not
  # bias the first/second-half comparison.
  t_mid <- (time_num[run_starts[best_run_i]] + time_num[run_ends[best_run_i]]) / 2
  block_time_num <- as.numeric(steady_block$time)
  first_half <- steady_block[block_time_num <= t_mid, , drop = FALSE]
  second_half <- steady_block[block_time_num > t_mid, , drop = FALSE]

  if (nrow(first_half) == 0 || nrow(second_half) == 0) {
    return(make_rec(
      status = "non_steady",
      hr_coverage = hr_coverage_raw,
      quality_score = quality_score,
      steady_duration_minutes = steady_duration_minutes,
      sampling_interval_seconds = sampling_interval_seconds
    ))
  }

  # Calculate efficiency factor for each half from the chosen steady block
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

  return(make_rec(
    value = decoupling_pct,
    status = status,
    hr_coverage = hr_coverage_raw,
    quality_score = quality_score,
    steady_duration_minutes = steady_duration_minutes,
    sampling_interval_seconds = sampling_interval_seconds
  ))
}
