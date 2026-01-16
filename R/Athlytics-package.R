#' @keywords internal
"_PACKAGE"

#' Athlytics: Academic R Package for Sports Physiology Analysis
#'
#' @description
#' Athlytics is an open-source computational framework for longitudinal analysis
#' of exercise physiology metrics using local Strava data exports. Designed for
#' personal analysis and sports science applications, this package provides
#' standardized functions to calculate and visualize key physiological indicators.
#'
#' @section Main Functions:
#' **Data Loading:**
#' \itemize{
#'   \item \code{\link{load_local_activities}}: Load activities from Strava export ZIP or directory
#'   \item \code{\link{parse_activity_file}}: Parse individual FIT/TCX/GPX files
#' }
#'
#' **Training Load Analysis:**
#' \itemize{
#'   \item \code{\link{calculate_acwr}}: Calculate Acute:Chronic Workload Ratio
#'   \item \code{\link{calculate_acwr_ewma}}: ACWR using exponentially weighted moving averages
#'   \item \code{\link{calculate_exposure}}: Calculate training load exposure metrics
#' }
#'
#' **Physiological Metrics:**
#' \itemize{
#'   \item \code{\link{calculate_ef}}: Calculate Efficiency Factor (EF)
#'   \item \code{\link{calculate_decoupling}}: Calculate cardiovascular decoupling
#'   \item \code{\link{calculate_pbs}}: Calculate personal bests
#' }
#'
#' **Visualization:**
#' \itemize{
#'   \item \code{\link{plot_acwr}}, \code{\link{plot_acwr_enhanced}}: Plot ACWR trends
#'   \item \code{\link{plot_ef}}: Plot Efficiency Factor trends
#'   \item \code{\link{plot_decoupling}}: Plot decoupling analysis
#'   \item \code{\link{plot_exposure}}: Plot training load exposure
#'   \item \code{\link{plot_pbs}}: Plot personal bests progression
#' }
#'
#' **Quality Control & Cohort Analysis:**
#' \itemize{
#'   \item \code{\link{flag_quality}}: Flag activities based on quality criteria
#'   \item \code{\link{summarize_quality}}: Summarize stream quality flags
#'   \item \code{\link{calculate_cohort_reference}}: Generate cohort reference bands
#' }
#'
#' @section Sample Datasets:
#' The package includes simulated datasets for examples and testing:
#' \itemize{
#'   \item \code{\link{sample_acwr}}: Sample ACWR data
#'   \item \code{\link{sample_ef}}: Sample Efficiency Factor data
#'   \item \code{\link{sample_decoupling}}: Sample decoupling data
#'   \item \code{\link{sample_exposure}}: Sample exposure data
#'   \item \code{\link{sample_pbs}}: Sample personal bests data
#' }
#'
#' @section Getting Started:
#' ```
#' library(Athlytics)
#'
#' # Load your Strava export
#' activities <- load_local_activities("path/to/strava_export.zip")
#'
#' # Calculate ACWR
#' acwr_data <- calculate_acwr(activities, activity_type = "Run")
#'
#' # Visualize

#' plot_acwr(acwr_data)
#' ```
#'
#' @seealso
#' \itemize{
#'   \item Package website: \url{https://hzacode.github.io/Athlytics/}
#'   \item GitHub repository: \url{https://github.com/HzaCode/Athlytics}
#'   \item Strava: \url{https://www.strava.com/}
#' }
#'
#' @importFrom dplyr mutate filter select group_by ungroup summarise arrange
#' @importFrom dplyr left_join bind_rows n lag lead across any_of all_of
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon theme_minimal
#' @importFrom ggplot2 labs scale_x_date scale_y_continuous theme element_text
#' @importFrom lubridate as_date ymd hms seconds_to_period
#' @importFrom rlang .data := sym
#' @importFrom stats na.omit sd median quantile
#' @importFrom utils head tail
#'
#' @name Athlytics-package
#' @aliases Athlytics
NULL
