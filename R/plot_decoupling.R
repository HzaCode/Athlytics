# R/plot_decoupling.R

#' Plot Aerobic Decoupling Trend
#'
#' Visualizes the trend of aerobic decoupling over time.
#'
#' Plots the aerobic decoupling trend over time. **Recommended workflow: Use local data via `decoupling_df`.**
#'
#' @param data **Recommended: Pass pre-calculated data via `decoupling_df` (local export preferred).**
#'   A data frame from `calculate_decoupling()` or activities data from `load_local_activities()`.
#' @param activity_type Type(s) of activities to analyze (e.g., "Run", "Ride").
#' @param decouple_metric Metric basis: "pace_hr" or "power_hr".
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to ~1 year ago.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#' @param min_duration_mins Minimum activity duration (minutes) to include. Default 45.
#' @param add_trend_line Add a smoothed trend line (`geom_smooth`)? Default `TRUE`.
#' @param smoothing_method Smoothing method for trend line (e.g., "loess", "lm"). Default "loess".
#' @param decoupling_df **Recommended.** A pre-calculated data frame from `calculate_decoupling()`.
#'   When provided, analysis uses local data only (no API calls).
#'   Must contain 'date' and 'decoupling' columns.

#' @return A ggplot object showing the decoupling trend.
#'
#' @details Plots decoupling percentage ((EF_1st_half - EF_2nd_half) / EF_1st_half * 100).
#'   Positive values mean HR drifted relative to output. A 5\\% threshold line is often
#'   used as reference. **Best practice: Use `load_local_activities()` + `calculate_decoupling()` + this function.**

#' @importFrom dplyr filter select mutate arrange %>% rename left_join case_when group_by summarise pull first last tibble
#' @importFrom lubridate as_date
#' @importFrom lubridate date
#' @importFrom lubridate days
#' @importFrom lubridate ymd
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate as_datetime
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal scale_x_date theme element_text geom_hline
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example using pre-calculated sample data
#' data("athlytics_sample_decoupling", package = "Athlytics")
#' p <- plot_decoupling(decoupling_df = athlytics_sample_decoupling)
#' print(p)
#'
#' \dontrun{
#' # Example using local Strava export data
#' activities <- load_local_activities("strava_export_data/activities.csv")
#' 
#' # Example 1: Plot Decoupling trend for Runs (last 6 months)
#' decoupling_runs_6mo <- calculate_decoupling(
#'     activities_data = activities,
#'     export_dir = "strava_export_data",
#'     activity_type = "Run",
#'     decouple_metric = "pace_hr",
#'     start_date = Sys.Date() - months(6)
#' )
#' plot_decoupling(decoupling_runs_6mo)
#'
#' # Example 2: Plot Decoupling trend for Rides
#' decoupling_rides <- calculate_decoupling(
#'   activities_data = activities,
#'   export_dir = "strava_export_data",
#'   activity_type = "Ride",
#'   decouple_metric = "power_hr"
#' )
#' plot_decoupling(decoupling_rides)
#'
#' # Example 3: Plot Decoupling trend for multiple Run types (no trend line)
#' decoupling_multi_run <- calculate_decoupling(
#'   activities_data = activities,
#'   export_dir = "strava_export_data",
#'   activity_type = c("Run", "VirtualRun"),
#'   decouple_metric = "pace_hr"
#' )
#' plot_decoupling(decoupling_multi_run, add_trend_line = FALSE)
#' }
plot_decoupling <- function(data,
                            activity_type = c("Run", "Ride"), # Default to both if not specified
                            decouple_metric = c("pace_hr", "power_hr"),
                            start_date = NULL,
                            end_date = NULL,
                            min_duration_mins = 45,
                            add_trend_line = TRUE,
                            smoothing_method = "loess",
                            decoupling_df = NULL) {

  # Match arg for decouple_metric to ensure only one is used internally if multiple are provided
  decouple_metric_label <- match.arg(decouple_metric)

  # --- Get Data --- 
  # If decoupling_df is not provided, calculate it
  if (is.null(decoupling_df)) {
      # Ensure data is provided if decoupling_df is not
      if (missing(data)) stop("Either 'data' or 'decoupling_df' must be provided.")
      
      message("No pre-calculated decoupling_df provided. Calculating data now... (This may take a while)")
      # Call the calculation function
      decoupling_df <- calculate_decoupling(
          activities_data = data,
          activity_type = activity_type, # Can be a vector
          decouple_metric = decouple_metric_label, # Use the matched, single metric
          start_date = start_date,
          end_date = end_date,
          min_duration_mins = min_duration_mins
      )
  }

  # Check if decoupling_df is empty or invalid
  if (!is.data.frame(decoupling_df) || nrow(decoupling_df) == 0 || !all(c("date", "decoupling") %in% names(decoupling_df))) {
      warning("No valid decoupling data available to plot (or missing required columns).")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No decoupling data to plot"))
  }

  # Rename for clarity
  plot_data <- decoupling_df
  
  # --- Generate Dynamic Title ---
  # Determine title based on activity_type and data
  if ("activity_type" %in% colnames(plot_data)) {
    unique_types <- unique(plot_data$activity_type)
    if (length(unique_types) == 1) {
      plot_title <- paste("Trend for", unique_types[1])
    } else {
      plot_title <- "Trend for Selected Activities"
    }
  } else {
    if (length(activity_type) == 1) {
      plot_title <- paste("Trend for", activity_type[1])
    } else {
      plot_title <- "Trend for Selected Activities"
    }
  }
  
  # --- Plotting ---
  message("Generating plot...")
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$decoupling)) +
    ggplot2::geom_point(alpha = 0.7, size = 2, color = "#E64B35") +
    ggplot2::scale_x_date(labels = english_month_year, date_breaks = "3 months") +
    ggplot2::labs(
      title = plot_title,
      subtitle = paste("Metric:", decouple_metric_label),
      x = "Date",
      y = "Decoupling (%)",
      caption = "Positive values indicate HR drift relative to output"
    )
  
  # Add 5% threshold line
  p <- p + ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "red", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5)
  
  if (add_trend_line && nrow(plot_data) >= 2) {
    p <- p + ggplot2::geom_smooth(method = smoothing_method, se = FALSE, color = "blue", linewidth = 0.8)
  }
  
  p <- p +
    theme_athlytics()

  return(p)
}