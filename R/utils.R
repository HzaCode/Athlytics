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
