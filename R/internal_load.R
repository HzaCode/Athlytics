# R/internal_load.R
# Internal helper functions for load calculations
# These functions are not exported and are used internally by calculate_acwr(),
# calculate_exposure(), and calculate_acwr_ewma().

#' Calculate Daily Load from Activities Data Frame
#'
#' Internal function that computes load values for each activity based on
#' the specified metric. This centralizes the load calculation logic to
#' reduce code duplication and complexity in main calculation functions.
#'
#' Every activity row is preserved in the output along with a per-row
#' `load_status` column, so callers can distinguish:
#' - a *real* rest day (no activity ever recorded on that date), from
#' - a training day on which the chosen `load_metric` was not computable
#'   because the prerequisite inputs (HR / power / FTP / threshold HR)
#'   were missing.
#'
#' Previously, activities with `load_value <= 0` (for example an HRSS call
#' on an activity with no HR samples) were silently dropped from the
#' output, which downstream ACWR / exposure code then coalesced to zero
#' via the full-date left-join. That produced the same value as a true
#' rest day and masked data-quality problems. Keeping the row with
#' `load = NA` and a descriptive status is now the default; how the
#' downstream rolling means treat those NA days is controlled by the
#' `missing_load` argument on `calculate_acwr()`, `calculate_acwr_ewma()`
#' and `calculate_exposure()`.
#'
#' @param activities_df A data frame of filtered activities.
#' @param load_metric Character string specifying the load calculation method.
#' @param user_ftp Numeric. Functional Threshold Power (required for "tss").
#' @param user_max_hr Numeric. Maximum heart rate (required for "hrss").
#' @param user_resting_hr Numeric. Resting heart rate (required for "hrss").
#'
#' @return A data frame with columns `date`, `load` (NA when the metric
#'   could not be computed), and `load_status` (a character code from
#'   `compute_single_load()`).
#'
#' @keywords internal
#' @noRd
calculate_daily_load_internal <- function(activities_df,
                                          load_metric,
                                          user_ftp = NULL,
                                          user_max_hr = NULL,
                                          user_resting_hr = NULL) {
  safe_as_numeric <- function(x) {
    as.numeric(rlang::`%||%`(x, 0))
  }

  lapply(seq_len(nrow(activities_df)), function(i) {
    activity <- activities_df[i, ]
    activity_date <- activity$date

    # Extract metrics from data frame columns
    duration_sec <- safe_as_numeric(activity$moving_time)
    distance_m <- safe_as_numeric(activity$distance)
    elapsed_sec <- safe_as_numeric(activity$elapsed_time)
    avg_hr <- safe_as_numeric(activity$average_heartrate)
    elevation_gain <- safe_as_numeric(activity$elevation_gain)
    np_proxy <- safe_as_numeric(activity$weighted_average_watts %||%
      activity$average_watts %||% 0)

    # Calculate load based on metric
    load_rec <- compute_single_load(
      load_metric = load_metric,
      duration_sec = duration_sec,
      distance_m = distance_m,
      elapsed_sec = elapsed_sec,
      avg_hr = avg_hr,
      elevation_gain = elevation_gain,
      np_proxy = np_proxy,
      user_ftp = user_ftp,
      user_max_hr = user_max_hr,
      user_resting_hr = user_resting_hr
    )

    # Keep the row even when the metric could not be computed. The status
    # is what ACWR / exposure inspect to honour `missing_load`.
    data.frame(
      date = activity_date,
      load = load_rec$value,
      load_status = load_rec$status,
      stringsAsFactors = FALSE
    )
  }) |> dplyr::bind_rows()
}


#' Compute Load Value for a Single Activity
#'
#' Internal function that computes the load value for a single activity
#' based on the specified metric. Uses switch() for cleaner branching.
#'
#' Returns a list with `value` and `status` so callers can distinguish a
#' true rest day (never passed here) from a training session where the
#' prerequisites for the chosen metric were missing (e.g. an HRSS call on
#' an activity with no HR samples).
#'
#' @param load_metric Character string specifying the load calculation method.
#' @param duration_sec Numeric. Activity duration in seconds.
#' @param distance_m Numeric. Activity distance in meters.
#' @param elapsed_sec Numeric. Total elapsed time in seconds.
#' @param avg_hr Numeric. Average heart rate.
#' @param elevation_gain Numeric. Elevation gain in meters.
#' @param np_proxy Numeric. Normalized power proxy (weighted avg watts or avg watts).
#' @param user_ftp Numeric. Functional Threshold Power.
#' @param user_max_hr Numeric. Maximum heart rate.
#' @param user_resting_hr Numeric. Resting heart rate.
#'
#' @return A list `list(value, status)`. `value` is a numeric load, or
#'   `NA_real_` when the metric was not computable. `status` is one of
#'   `"ok"`, `"missing_duration"`, `"missing_heart_rate"`,
#'   `"missing_threshold_hr"`, `"hr_out_of_range"`, `"missing_power"`,
#'   `"missing_ftp"`, or `"unsupported_metric"`.
#'
#' @keywords internal
#' @noRd
compute_single_load <- function(load_metric,
                                duration_sec,
                                distance_m,
                                elapsed_sec,
                                avg_hr,
                                elevation_gain,
                                np_proxy,
                                user_ftp,
                                user_max_hr,
                                user_resting_hr) {
  if (is.na(duration_sec) || duration_sec <= 0) {
    return(list(value = NA_real_, status = "missing_duration"))
  }

  switch(load_metric,
    "duration_mins" = list(value = duration_sec / 60, status = "ok"),
    "distance_km" = list(value = distance_m / 1000, status = "ok"),
    "elapsed_time_mins" = list(value = elapsed_sec / 60, status = "ok"),
    "elevation_gain_m" = list(value = elevation_gain, status = "ok"),
    "hrss" = {
      if (is.null(user_max_hr) || is.null(user_resting_hr) ||
        !is.numeric(user_max_hr) || !is.numeric(user_resting_hr) ||
        user_max_hr <= user_resting_hr) {
        list(value = NA_real_, status = "missing_threshold_hr")
      } else if (is.na(avg_hr) || avg_hr <= 0) {
        list(value = NA_real_, status = "missing_heart_rate")
      } else if (avg_hr <= user_resting_hr || avg_hr > user_max_hr) {
        list(value = NA_real_, status = "hr_out_of_range")
      } else {
        hr_reserve <- user_max_hr - user_resting_hr
        avg_hr_rel <- (avg_hr - user_resting_hr) / hr_reserve
        list(value = (duration_sec / 60) * avg_hr_rel, status = "ok")
      }
    },
    "tss" = {
      if (is.null(user_ftp) || !is.numeric(user_ftp) || user_ftp <= 0) {
        list(value = NA_real_, status = "missing_ftp")
      } else if (is.na(np_proxy) || np_proxy <= 0) {
        list(value = NA_real_, status = "missing_power")
      } else {
        intensity_factor <- np_proxy / user_ftp
        list(
          value = (duration_sec * np_proxy * intensity_factor) /
            (user_ftp * 3600) * 100,
          status = "ok"
        )
      }
    },

    # Default case
    list(value = NA_real_, status = "unsupported_metric")
  )
}


#' Validate Load Metric Parameters
#'
#' Internal function that validates required parameters for each load metric.
#' Centralizes validation logic to reduce complexity in main functions.
#'
#' @param load_metric Character string specifying the load calculation method.
#' @param user_ftp Numeric. Functional Threshold Power.
#' @param user_max_hr Numeric. Maximum heart rate.
#' @param user_resting_hr Numeric. Resting heart rate.
#'
#' @return NULL (invisibly). Stops with error if validation fails.
#'
#' @keywords internal
#' @noRd
validate_load_metric_params <- function(load_metric,
                                        user_ftp = NULL,
                                        user_max_hr = NULL,
                                        user_resting_hr = NULL) {
  valid_metrics <- c(
    "duration_mins", "distance_km", "elapsed_time_mins",
    "tss", "hrss", "elevation_gain_m"
  )

  if (!load_metric %in% valid_metrics) {
    stop("Invalid `load_metric`. Choose from: ", paste(valid_metrics, collapse = ", "))
  }

  if (load_metric == "tss" && is.null(user_ftp)) {
    stop("`user_ftp` is required when `load_metric` is 'tss'.")
  }

  if (load_metric == "hrss" && (is.null(user_max_hr) || is.null(user_resting_hr))) {
    stop("`user_max_hr` and `user_resting_hr` are required when `load_metric` is 'hrss'.")
  }

  invisible(NULL)
}
