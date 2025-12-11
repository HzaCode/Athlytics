# R/plot_ef.R

#' Plot Efficiency Factor (EF) Trend
#'
#' Visualizes the trend of Efficiency Factor (EF) over time.
#'
#' Plots the Efficiency Factor (EF) trend over time. **Recommended workflow: Use local data via `ef_df`.**
#'
#' @param data **Recommended: Pass pre-calculated data via `ef_df` (local export preferred).**
#'   A data frame from `calculate_ef()` or activities data from `load_local_activities()`.
#' @param activity_type Type(s) of activities to analyze (e.g., "Run", "Ride").
#' @param ef_metric Metric to calculate: "pace_hr" (Speed/HR) or "power_hr" (Power/HR).
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to ~1 year ago.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#' @param min_duration_mins Minimum activity duration (minutes) to include. Default 20.
#' @param add_trend_line Add a smoothed trend line (`geom_smooth`)? Default `TRUE`.
#' @param smoothing_method Smoothing method for trend line (e.g., "loess", "lm"). Default "loess".
#' @param ef_df **Recommended.** A pre-calculated data frame from `calculate_ef()`.
#'   When provided, analysis uses local data only (no API calls).
#' @param group_var Optional. Column name for grouping/faceting (e.g., "athlete_id").
#' @param group_colors Optional. Named vector of colors for groups.
#'
#' @return A ggplot object showing the EF trend.
#'
#' @details Plots EF (output/HR based on activity averages). An upward trend
#'   often indicates improved aerobic fitness. Points colored by activity type.
#'   **Best practice: Use `load_local_activities()` + `calculate_ef()` + this function.**
#'
#' 
#' @importFrom dplyr filter select mutate arrange %>% rename left_join case_when pull
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal scale_x_date theme element_text scale_color_viridis_d
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example using pre-calculated sample data
#' data("athlytics_sample_ef", package = "Athlytics")
#' p <- plot_ef(athlytics_sample_ef)
#' print(p)
#'
#' \dontrun{
#' # Example using local Strava export data
#' activities <- load_local_activities("strava_export_data/activities.csv")
#' 
#' # Plot Pace/HR EF trend for Runs (last 6 months)
#' plot_ef(data = activities,
#'         activity_type = "Run",
#'         ef_metric = "pace_hr",
#'         start_date = Sys.Date() - months(6))
#'
#' # Plot Power/HR EF trend for Rides
#' plot_ef(data = activities,
#'         activity_type = "Ride",
#'         ef_metric = "power_hr")
#'
#' # Plot Pace/HR EF trend for multiple Run types (no trend line)
#' plot_ef(data = activities,
#'         activity_type = c("Run", "VirtualRun"),
#'         ef_metric = "pace_hr",
#'         add_trend_line = FALSE)
#' }
plot_ef <- function(data,
                    activity_type = c("Run", "Ride"),
                    ef_metric = c("pace_hr", "power_hr"),
                    start_date = NULL,
                    end_date = NULL,
                    min_duration_mins = 20,
                    add_trend_line = TRUE,
                    smoothing_method = "loess",
                    ef_df = NULL,
                    group_var = NULL,
                    group_colors = NULL) {

  # Match arg here as it's needed for plot labels
  ef_metric_label <- match.arg(ef_metric)

  # --- Check if first argument is already EF data frame ---
  if (is.data.frame(data) && all(c("date", "ef_value") %in% colnames(data))) {
    ef_df <- data
  }

  # --- Get Data --- 
  # If ef_df is not provided, calculate it
  if (is.null(ef_df)) {
      # Check if data provided when ef_df is not
      if (missing(data)) stop("Either provide EF data frame from calculate_ef() as first argument, or provide activities_data.")
      
      ef_df <- calculate_ef(
          activities_data = data,
          activity_type = activity_type,
          ef_metric = ef_metric_label,
          start_date = start_date,
          end_date = end_date,
          min_duration_mins = min_duration_mins
      )
  }

  # Check if ef_df is empty or invalid
  if (!is.data.frame(ef_df) || nrow(ef_df) == 0 || !all(c("date", "ef_value", "activity_type") %in% names(ef_df))) {
      warning("No valid EF data available to plot (or missing required columns).")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No EF data available"))
  }

  # Rename for clarity
  plot_data <- ef_df
  
  # --- Check for group variable ---
  has_groups <- !is.null(group_var) && group_var %in% colnames(plot_data)

  # --- Plotting ---
  message("Generating plot...")
  y_label <- switch(ef_metric_label,
                    "pace_hr" = "Efficiency Factor (Speed [m/s] / HR)",
                    "power_hr" = "Efficiency Factor (Power [W] / HR)")

  if (has_groups) {
    # Multi-group plotting
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$ef_value, 
                                                  color = .data[[group_var]])) +
      ggplot2::geom_point(alpha = 0.7, size = 2.5) +
      ggplot2::scale_x_date(labels = english_month_year, date_breaks = "3 months")
    
    # Apply custom colors if provided
    if (!is.null(group_colors)) {
      p <- p + ggplot2::scale_color_manual(values = group_colors, name = group_var)
    } else {
      # Use default Nature palette colors
      p <- p + ggplot2::scale_color_manual(
        values = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4"),
        name = group_var
      )
    }
    
    p <- p +
      ggplot2::labs(
        title = "Efficiency Factor (EF) Trend",
        subtitle = paste("Metric:", ef_metric_label, "| Grouped by:", group_var),
        x = "Date",
        y = y_label
      )
    
    if (add_trend_line) {
      p <- p + ggplot2::geom_smooth(ggplot2::aes(group = .data[[group_var]]), 
                                    method = smoothing_method, se = FALSE, linewidth = 0.8)
    }
    
  } else {
    # Single group plotting (original logic)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$ef_value)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$activity_type), alpha = 0.7, size = 2) +
      ggplot2::scale_x_date(labels = english_month_year, date_breaks = "3 months") +
      ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8) +
      ggplot2::labs(
        title = "Efficiency Factor (EF) Trend",
        subtitle = paste("Metric:", ef_metric_label),
        x = "Date",
        y = y_label,
        color = "Activity Type"
      )
    
    if (add_trend_line) {
      p <- p + ggplot2::geom_smooth(method = smoothing_method, se = FALSE, color = "blue", linewidth = 0.8)
    }
  }
  
  p <- p +
    theme_athlytics() +
    ggplot2::theme(
      legend.position = "bottom"
    )

  return(p)
}

# Helper for null default (copied from plot_pbs)
# `%||%` <- function(x, y) {
#   if (is.null(x) || length(x) == 0) y else x
# } 
