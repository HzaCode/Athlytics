# R/data.R

#' Sample ACWR Data for Athlytics
#'
#' A dataset containing pre-calculated Acute:Chronic Workload Ratio (ACWR)
#' and related metrics, derived from simulated Strava data. Used in examples and tests.
#'
#' @format A tibble with X rows and 5 variables:
#' \describe{
#'   \item{date}{Date of the metrics, as a Date object.}
#'   \item{atl}{Acute Training Load, as a numeric value.}
#'   \item{ctl}{Chronic Training Load, as a numeric value.}
#'   \item{acwr}{Acute:Chronic Workload Ratio, as a numeric value.}
#'   \item{acwr_smooth}{Smoothed ACWR, as a numeric value.}
#' }
#' @source Simulated data generated for package examples.
"athlytics_sample_acwr"

#' Sample Aerobic Decoupling Data for Athlytics
#'
#' A dataset containing pre-calculated aerobic decoupling percentages,
#' derived from simulated Strava data. Used in examples and tests.
#'
#' @format A tibble with X rows and 2 variables:
#' \describe{
#'   \item{date}{Date of the activity, as a Date object.}
#'   \item{decoupling}{Calculated decoupling percentage, as a numeric value.}
#' }
#' @source Simulated data generated for package examples.
"athlytics_sample_decoupling"

#' Sample Efficiency Factor (EF) Data for Athlytics
#'
#' A dataset containing pre-calculated Efficiency Factor (EF) values,
#' derived from simulated Strava data. Used in examples and tests.
#'
#' @format A data.frame with X rows and 3 variables:
#' \describe{
#'   \item{date}{Date of the activity, as a Date object.}
#'   \item{activity_type}{Type of activity (e.g., "Run", "Ride"), as a character string.}
#'   \item{ef_value}{Calculated Efficiency Factor, as a numeric value.}
#' }
#' @source Simulated data generated for package examples.
"athlytics_sample_ef"

#' Sample Training Load Exposure Data for Athlytics
#'
#' This dataset contains daily training load, ATL, CTL, and ACWR, derived from
#' simulated Strava data. Used in examples and tests, particularly for `plot_exposure`.
#'
#' @format A tibble with X rows and 5 variables:
#' \describe{
#'   \item{date}{Date of the metrics, as a Date object.}
#'   \item{daily_load}{Calculated daily training load, as a numeric value.}
#'   \item{ctl}{Chronic Training Load, as a numeric value.}
#'   \item{atl}{Acute Training Load, as a numeric value.}
#'   \item{acwr}{Acute:Chronic Workload Ratio, as a numeric value.}
#' }
#' @source Simulated data generated for package examples.
"athlytics_sample_exposure"

#' Sample Personal Bests (PBs) Data for Athlytics
#'
#' A dataset containing pre-calculated Personal Best (PB) times for various distances,
#' derived from simulated Strava data. Used in examples and tests.
#'
#' @format A tibble with X rows and 10 variables:
#' \describe{
#'   \item{activity_id}{ID of the activity where the effort occurred, as a character string.}
#'   \item{activity_date}{Date and time of the activity, as a POSIXct object.}
#'   \item{distance}{Target distance in meters for the best effort, as a numeric value.}
#'   \item{elapsed_time}{Elapsed time for the effort in seconds, as a numeric value.}
#'   \item{moving_time}{Moving time for the effort in seconds, as a numeric value.}
#'   \item{time_seconds}{Typically the same as elapsed_time for best efforts, in seconds, as a numeric value.}
#'   \item{cumulative_pb_seconds}{The personal best time for that distance up to that date, in seconds, as a numeric value.}
#'   \item{is_pb}{Logical, TRUE if this effort set a new personal best.}
#'   \item{distance_label}{Factor representing the distance (e.g., "1k", "5k").}
#'   \item{time_period}{Formatted time of the effort, as a Period object from lubridate.}
#' }
#' @source Simulated data generated for package examples.
"athlytics_sample_pbs" 