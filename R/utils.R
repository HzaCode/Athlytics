# R/utils.R

# Internal helper function for English month-year labels
english_month_year <- function(dates) {
  months_en <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  paste(months_en[lubridate::month(dates)], lubridate::year(dates))
}

athlytics_is_verbose <- function() {
  isTRUE(getOption("Athlytics.verbose", FALSE))
}

athlytics_message <- function(..., .verbose = athlytics_is_verbose()) {
  if (isTRUE(.verbose)) {
    message(...)
  }
}

#' Internal: Time-Weighted Stream Coverage
#'
#' Computes the fraction of total recording time for which a given column is
#' present and valid. Prior implementations used row-count coverage, which
#' overweights densely-sampled sections and cannot detect streams where a
#' sensor silently dropped out during large time gaps.
#'
#' @param stream_df A data frame with a numeric `time` column (seconds since
#'   activity start) and the value column named in `col`.
#' @param col Name of the value column whose coverage is being measured.
#' @param valid_fn A predicate taking the value vector and returning a logical
#'   vector of the same length indicating "valid" samples. Default rejects
#'   NA and non-positive values (suitable for HR / watts / velocity).
#'
#' @return A single numeric in \[0, 1\] giving the time-weighted coverage. If
#'   the stream has fewer than 2 rows or zero duration, row-fraction coverage
#'   is returned as a fallback so short streams still have meaningful coverage.
#'
#' @keywords internal
#' @noRd
time_weighted_coverage <- function(stream_df, col,
                                   valid_fn = function(x) !is.na(x) & x > 0) {
  if (!is.data.frame(stream_df) || nrow(stream_df) == 0 || !(col %in% colnames(stream_df))) {
    return(1.0)
  }

  valid <- valid_fn(stream_df[[col]])

  if (nrow(stream_df) < 2 || !"time" %in% colnames(stream_df)) {
    return(sum(valid, na.rm = TRUE) / nrow(stream_df))
  }

  time_vec <- as.numeric(stream_df$time)
  # sort by time so diff() reflects actual sampling intervals
  ord <- order(time_vec)
  time_vec <- time_vec[ord]
  valid <- valid[ord]

  dt <- diff(time_vec)
  # dt[i] is the time weight attributed to row i (the segment from t_i to t_{i+1}).
  # A valid segment requires both endpoints to have a valid sample, which is
  # stricter (and more realistic) than valid[i] alone.
  segment_valid <- valid[-length(valid)] & valid[-1]

  total_time <- sum(dt, na.rm = TRUE)
  if (!is.finite(total_time) || total_time <= 0) {
    return(sum(valid, na.rm = TRUE) / length(valid))
  }

  sum(dt[segment_valid], na.rm = TRUE) / total_time
}

#' Internal: Estimate the median sampling interval (seconds) of a stream
#'
#' Many downstream heuristics (rolling CV windows, steady-state detection,
#' HR/power jump rate checks) were historically written assuming 1 Hz
#' sampling. Smart-recording Garmin watches, low-power Wahoo units and some
#' Strava TCX exports are not 1 Hz, which silently rescales those heuristics
#' by an unknown factor. This helper reports the observed sampling interval
#' so callers can convert row-based window sizes into time-based ones and
#' compute per-second rates from per-sample diffs.
#'
#' @param stream_df A data frame with a numeric `time` column (seconds since
#'   activity start, POSIXct is also accepted because it is coerced via
#'   `as.numeric()`).
#' @param default Fallback returned when the stream is too short or the time
#'   column is missing / non-numeric. Defaults to 1 second (the pre-fix
#'   implicit assumption).
#'
#' @return A single positive numeric. Always finite.
#'
#' @keywords internal
#' @noRd
estimate_sampling_interval <- function(stream_df, default = 1) {
  if (!is.data.frame(stream_df) || nrow(stream_df) < 2 ||
    !"time" %in% colnames(stream_df)) {
    return(as.numeric(default))
  }

  time_num <- suppressWarnings(as.numeric(stream_df$time))
  if (!is.numeric(time_num) || all(is.na(time_num))) {
    return(as.numeric(default))
  }

  time_num <- sort(time_num[!is.na(time_num)])
  if (length(time_num) < 2) {
    return(as.numeric(default))
  }

  dt <- diff(time_num)
  dt <- dt[is.finite(dt) & dt > 0]
  if (length(dt) == 0) {
    return(as.numeric(default))
  }

  med <- stats::median(dt)
  if (!is.finite(med) || med <= 0) {
    return(as.numeric(default))
  }
  as.numeric(med)
}

#' Internal: Convert a target time window to a row count
#'
#' Wraps `estimate_sampling_interval()` to produce a row-count window size
#' that targets a given number of seconds of wall-clock data regardless of
#' sampling frequency, capped by a supplied row-count ceiling and floor.
#'
#' @param stream_df A data frame (or any object accepted by
#'   `estimate_sampling_interval()`).
#' @param window_seconds Target window width in seconds (e.g. 300 for a
#'   5-minute rolling window).
#' @param min_rows Minimum number of rows; even on dense streams the rolling
#'   window must see at least this many samples to be meaningful.
#' @param max_rows Optional cap on window size (e.g. `nrow(stream) %/% 4`).
#'
#' @return An integer >= `min_rows` and <= `max_rows` (if provided).
#'
#' @keywords internal
#' @noRd
time_based_window_size <- function(stream_df, window_seconds,
                                   min_rows = 60, max_rows = NULL) {
  dt <- estimate_sampling_interval(stream_df, default = 1)
  ws <- ceiling(window_seconds / dt)
  if (!is.null(max_rows) && is.finite(max_rows) && max_rows > 0) {
    ws <- min(ws, as.integer(max_rows))
  }
  ws <- max(ws, as.integer(min_rows))
  as.integer(ws)
}

#' Internal: Parse an analysis window endpoint with a visible fallback
#'
#' Wraps `lubridate::as_date()` so that malformed `start_date` / `end_date`
#' arguments produce a one-shot `warning()` instead of being silently
#' replaced by the default. Prior to this helper every public entry point
#' swallowed parse failures via `tryCatch(..., error = function(e) Sys.Date())`,
#' which masked typos in user code (e.g. `start_date = "2024/13/01"`).
#'
#' @param x The value the user supplied (may be `NULL`, a string, a Date,
#'   POSIXct, or something unparseable).
#' @param default A Date to fall back to when `x` is `NULL`, `NA`, or
#'   unparseable.
#' @param arg_name Name of the argument for diagnostic messages.
#'
#' @return A single `Date`.
#'
#' @keywords internal
#' @noRd
parse_analysis_date <- function(x, default, arg_name) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    return(as.Date(default))
  }

  parsed <- suppressWarnings(tryCatch(
    lubridate::as_date(x),
    error = function(e) NA
  ))

  if (length(parsed) != 1L || is.na(parsed)) {
    warning(sprintf(
      "Could not parse `%s` = %s as a date; falling back to %s.",
      arg_name,
      paste(utils::head(format(x), 1), collapse = ""),
      format(as.Date(default))
    ), call. = FALSE)
    return(as.Date(default))
  }

  as.Date(parsed)
}
