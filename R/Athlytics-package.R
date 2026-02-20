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
#' - [load_local_activities()]: Load activities from Strava export ZIP or directory
#' - [parse_activity_file()]: Parse individual FIT/TCX/GPX files
#'
#' **Training Load Analysis:**
#' - [calculate_acwr()]: Calculate Acute:Chronic Workload Ratio
#' - [calculate_acwr_ewma()]: ACWR using exponentially weighted moving averages
#' - [calculate_exposure()]: Calculate training load exposure metrics
#'
#' **Physiological Metrics:**
#' - [calculate_ef()]: Calculate Efficiency Factor (EF)
#' - [calculate_decoupling()]: Calculate cardiovascular decoupling
#' - [calculate_pbs()]: Calculate personal bests
#'
#' **Visualization:**
#' - [plot_acwr()], [plot_acwr_enhanced()]: Plot ACWR trends
#' - [plot_ef()]: Plot Efficiency Factor trends
#' - [plot_decoupling()]: Plot decoupling analysis
#' - [plot_exposure()]: Plot training load exposure
#' - [plot_pbs()]: Plot personal bests progression
#'
#' **Quality Control & Cohort Analysis:**
#' - [flag_quality()]: Flag activities based on quality criteria
#' - [summarize_quality()]: Summarize stream quality flags
#' - [calculate_cohort_reference()]: Generate cohort reference bands
#'
#' @section Sample Datasets:
#' The package includes simulated datasets for examples and testing:
#' - [sample_acwr]: Sample ACWR data
#' - [sample_ef]: Sample Efficiency Factor data
#' - [sample_decoupling]: Sample decoupling data
#' - [sample_exposure]: Sample exposure data
#' - [sample_pbs]: Sample personal bests data
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
#' - Package website: <https://docs.ropensci.org/Athlytics/>
#' - GitHub repository: <https://github.com/ropensci/Athlytics>
#' - Strava: <https://www.strava.com/>
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data %||%
#' @importFrom stats sd median quantile
#'
#' @name Athlytics-package
#' @aliases Athlytics
NULL
