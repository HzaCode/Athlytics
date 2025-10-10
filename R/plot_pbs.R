#' Plot Personal Best (PB) Trends
#'
#' Visualizes the trend of personal best times for specific running distances.
#'
#' Plots the trend of best efforts for specified distances, highlighting new PBs
#' using pre-calculated data from `calculate_pbs`.
#'
#' @param pbs_df A data frame from `calculate_pbs` containing PB data.
#'   Must contain columns: 'activity_date', 'time_seconds', 'distance_label', 'is_pb'.
#' @param activity_type Type(s) of activities to display (e.g., "Run"). Default "Run".
#' @param distance_meters Optional. Numeric vector of distances (meters) to filter.
#'   If NULL, all distances in pbs_df are plotted.
#' @param add_trend_line Logical. Whether to add a trend line to the plot. Default TRUE.
#'
#' @return A ggplot object showing PB trends, faceted by distance if multiple are plotted.
#'
#' @details Visualizes data from `calculate_pbs`. Points show best efforts;
#'   solid points mark new PBs. Y-axis is MM:SS.
#'   Users must first call `calculate_pbs` to generate the required data.
#'
#' 
#' @importFrom dplyr filter select mutate arrange group_by slice bind_rows summarise distinct rename %>% left_join
#' @importFrom purrr map_dfr map_chr possibly quietly
#' @importFrom tidyr unnest pivot_longer
#' @importFrom lubridate as_datetime ymd_hms seconds_to_period parse_date_time
#' @import ggplot2
#' @importFrom viridis scale_color_viridis
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # First calculate PBs using calculate_pbs function
#' # pbs_data <- calculate_pbs(
#' #   activities_data = your_activities,
#' #   activity_type = "Run",
#' #   distances_m = c(1000, 5000, 10000)
#' # )
#' # 
#' # Then plot the PBs
#' # plot_pbs(pbs_data)
#' # 
#' # Plot only specific distances
#' # plot_pbs(pbs_data, distance_meters = c(5000, 10000))
#' # 
#' # Plot without trend line
#' # plot_pbs(pbs_data, add_trend_line = FALSE)
#' }

plot_pbs <- function(pbs_df,
                     activity_type = "Run",
                     distance_meters = NULL,
                     add_trend_line = TRUE) {

  # --- Validate input ---
  if (!is.data.frame(pbs_df)) {
    stop("pbs_df must be a data frame from calculate_pbs()")
  }
  
  required_cols <- c("activity_date", "time_seconds", "distance_label", "is_pb")
  missing_cols <- setdiff(required_cols, names(pbs_df))
  if (length(missing_cols) > 0) {
    stop("pbs_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.data.frame(pbs_df) || nrow(pbs_df) == 0) {
      warning("No PB data available to plot.")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No PB data available")) 
  }
  
  # Filter by distance_meters if provided
  if (!is.null(distance_meters)) {
    pbs_df <- pbs_df[pbs_df$distance %in% distance_meters, ]
    if (nrow(pbs_df) == 0) {
      warning("pbs_df does not contain data for the specified distance_meters after filtering.")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No PB data for specified distances"))
    }
  }

  # --- Plotting ---
  message("Generating plot...")

  # Create the base plot
  p <- ggplot2::ggplot(pbs_df, ggplot2::aes(x = .data$activity_date, y = .data$time_seconds, color = .data$distance_label)) +
    ggplot2::geom_line(alpha = 0.5) + 
    ggplot2::geom_point(ggplot2::aes(shape = .data$is_pb), size = 2.5) + 
    ggplot2::scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1), 
                       name = "Personal Best", labels = c("TRUE" = "Yes", "FALSE" = "No")) +
    ggplot2::scale_x_date(labels = english_month_year, date_breaks = "3 months") +
    ggplot2::scale_y_continuous(labels = function(x) sprintf('%02d:%02d', floor(x/60), floor(x) %% 60)) + 
    viridis::scale_color_viridis(discrete = TRUE, option = "C", name = "Distance") + 
    ggplot2::labs(
      title = "Personal Best Running Times Trend",
      subtitle = "Showing best efforts for specified distances over time",
      x = "Activity Date",
      y = "Best Time (MM:SS)",
      caption = "Data sourced from Strava via rStrava"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 3)))

  # Add trend line if requested (though typically not used for PB plots)
  if (add_trend_line) {
    # Trend line might not be meaningful for PBs, but included for consistency
    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, aes(group = .data$distance_label), linewidth = 0.7)
  }

  if (length(unique(pbs_df$distance_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ .data$distance_label, scales = "free_y", ncol = 1) +
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }

  return(p)
}

# Helper for null default (from purrr example) - avoids direct dependency if only used here
