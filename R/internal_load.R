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
#' @param activities_df A data frame of filtered activities.
#' @param load_metric Character string specifying the load calculation method.
#' @param user_ftp Numeric. Functional Threshold Power (required for "tss").
#' @param user_max_hr Numeric. Maximum heart rate (required for "hrss").
#' @param user_resting_hr Numeric. Resting heart rate (required for "hrss").
#'
#' @return A data frame with columns: date, load.
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

  purrr::map_dfr(seq_len(nrow(activities_df)), function(i) {
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
    load_value <- compute_single_load(
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

    if (!is.na(load_value) && load_value > 0) {
      data.frame(
        date = activity_date,
        load = load_value,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })
}


#' Compute Load Value for a Single Activity
#'
#' Internal function that computes the load value for a single activity
#' based on the specified metric. Uses switch() for cleaner branching.
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
#' @return Numeric load value, or 0 if calculation not possible.
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
  if (duration_sec <= 0) {
    return(0)
  }

  switch(load_metric,
    "duration_mins" = duration_sec / 60,
    "distance_km" = distance_m / 1000,
    "elapsed_time_mins" = elapsed_sec / 60,
    "elevation_gain_m" = elevation_gain,
    "hrss" = {
      if (!is.null(user_max_hr) && !is.null(user_resting_hr) &&
        is.numeric(user_max_hr) && is.numeric(user_resting_hr) &&
        user_max_hr > user_resting_hr &&
        avg_hr > user_resting_hr && avg_hr <= user_max_hr) {
        hr_reserve <- user_max_hr - user_resting_hr
        avg_hr_rel <- (avg_hr - user_resting_hr) / hr_reserve
        (duration_sec / 60) * avg_hr_rel
      } else {
        0
      }
    },
    "tss" = {
      if (!is.null(user_ftp) && is.numeric(user_ftp) &&
        user_ftp > 0 && np_proxy > 0) {
        intensity_factor <- np_proxy / user_ftp
        (duration_sec * np_proxy * intensity_factor) / (user_ftp * 3600) * 100
      } else {
        0
      }
    },

    # Default case
    0
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
