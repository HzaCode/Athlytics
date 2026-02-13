# R/plot_pbs.R

#' Plot Personal Best (PB) Trends
#'
#' Visualizes the trend of personal best times for specific running distances.
#'
#' @param data A data frame from `calculate_pbs()`.
#'   Must contain `activity_date`, `distance`, `time_seconds`.
#' @param add_trend_line Logical. Whether to add a trend line to the plot. Default TRUE.
#' @param caption Plot caption. Default NULL (no caption).
#' @param facet_ncol Integer. Number of columns for faceted plots when multiple distances
#'   are shown. Default 2 for better aspect ratio. Set to 1 for vertical stacking.
#' @param title Optional. Custom title for the plot.
#' @param subtitle Optional. Custom subtitle for the plot.
#' @param ... Additional arguments.
#'   Arguments `activity_type`, `distance_meters`, `max_activities`, `date_range`,
#'   `pbs_df` are deprecated and ignored.
#'
#' @return A ggplot object showing PB trends, faceted by distance if multiple are plotted.
#'
#' @details Visualizes data from `calculate_pbs`. Points show best efforts;
#'   solid points mark new PBs. Y-axis is MM:SS.
#'   **Best practice: Use `calculate_pbs()` first, then pass the result to this function.**
#'
#' @export
#'
#' @examples
#' # Example using the built-in sample data
#' data("sample_pbs", package = "Athlytics")
#'
#' if (!is.null(sample_pbs) && nrow(sample_pbs) > 0) {
#'   # Plot PBs using the package sample data directly
#'   p <- plot_pbs(sample_pbs)
#'   print(p)
#' }
#'
#' \dontrun{
#' # Example using local Strava export data
#' activities <- load_local_activities("strava_export_data/activities.csv")
#'
#' # Calculate PBs first
#' pb_data_run <- calculate_pbs(
#'   activities_data = activities,
#'   activity_type = "Run",
#'   distances_m = c(1000, 5000, 10000)
#' )
#'
#' if (nrow(pb_data_run) > 0) {
#'   plot_pbs(pb_data_run)
#' }
#' }
#'
plot_pbs <- function(data,
                     add_trend_line = TRUE,
                     caption = NULL,
                     facet_ncol = 2,
                     title = NULL,
                     subtitle = NULL,
                     ...) {
  # Check for deprecated args
  deprecated_args <- list(...)
  if (length(deprecated_args) > 0) {
    analysis_args <- c("activity_type", "distance_meters", "max_activities", "date_range", "pbs_df")
    if (any(names(deprecated_args) %in% analysis_args)) {
      warning(
        "Analysis arguments (e.g. activity_type) are deprecated in plot_pbs(). ",
        "Please use calculate_pbs() first, then pass the result to plot_pbs()."
      )
    }
  }

  # Validate input
  pbs_df <- data
  if (!is.data.frame(pbs_df)) {
    stop("Input 'data' must be a data frame from calculate_pbs().")
  }

  if (!inherits(pbs_df, "athlytics_pbs") && !all(c("activity_date", "distance", "time_seconds") %in% names(pbs_df))) {
    stop("Input 'data' must be the output of calculate_pbs() or contain 'activity_date', 'distance', 'time_seconds' columns.")
  }

  if (nrow(pbs_df) == 0) {
    stop("No PB data available to plot.")
  }

  if (!all(c("activity_date", "distance", "time_seconds") %in% names(pbs_df))) {
    stop("Input data must contain 'activity_date', 'distance', 'time_seconds'.")
  }

  # Retrieve params for labeling
  params <- attr(pbs_df, "params")

  # --- Plotting ---

  # Ensure activity_date is Date type
  pbs_df$activity_date <- as.Date(pbs_df$activity_date)

  # Create distance_label if it doesn't exist
  if (!"distance_label" %in% names(pbs_df)) {
    pbs_df$distance_label <- dplyr::case_when(
      pbs_df$distance == 1000 ~ "1k",
      pbs_df$distance == 5000 ~ "5k",
      pbs_df$distance == 10000 ~ "10k",
      pbs_df$distance == 21097.5 ~ "21.1k",
      pbs_df$distance == 42195 ~ "Marathon",
      TRUE ~ paste0(round(pbs_df$distance), "m")
    )
    # Build levels dynamically: standard distances in canonical order,
    # then any non-standard distances sorted by numeric value
    standard_levels <- c("1k", "5k", "10k", "21.1k", "Marathon")
    present_standard <- standard_levels[standard_levels %in% unique(pbs_df$distance_label)]
    non_standard <- setdiff(unique(pbs_df$distance_label), standard_levels)
    # Sort non-standard labels by their numeric distance value
    if (length(non_standard) > 0) {
      ns_distances <- pbs_df$distance[match(non_standard, pbs_df$distance_label)]
      non_standard <- non_standard[order(ns_distances)]
    }
    all_levels <- c(present_standard, non_standard)
    pbs_df$distance_label <- factor(pbs_df$distance_label, levels = all_levels)
  }

  # Ensure required columns exist
  if (!"time_seconds" %in% names(pbs_df)) {
    if ("elapsed_time" %in% names(pbs_df)) {
      pbs_df$time_seconds <- pbs_df$elapsed_time
    } else {
      stop("pbs_df must contain either 'time_seconds' or 'elapsed_time' column")
    }
  }

  if (!"is_pb" %in% names(pbs_df)) {
    if ("is_new_pb" %in% names(pbs_df)) {
      pbs_df$is_pb <- pbs_df$is_new_pb
    } else {
      pbs_df$is_pb <- TRUE
    }
  }

  # Sort by distance_label and date for proper line drawing
  pbs_df <- pbs_df[order(pbs_df$distance_label, pbs_df$activity_date), ]

  # Create the base plot with Athlytics theme

  if (is.null(title)) title <- "Personal Best Running Times Trend"
  if (is.null(subtitle)) subtitle <- "Showing best efforts for specified distances over time"

  p <- ggplot2::ggplot(pbs_df, ggplot2::aes(
    x = .data$activity_date, y = .data$time_seconds,
    color = .data$distance_label, group = .data$distance_label
  )) +
    ggplot2::geom_line(linewidth = 1.8, alpha = 0.85) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$is_pb), size = 4.5, alpha = 0.95, stroke = 1.2) +
    ggplot2::scale_shape_manual(
      values = c("TRUE" = 19, "FALSE" = 21),
      name = "Personal Best", labels = c("TRUE" = "Yes", "FALSE" = "No")
    ) +
    ggplot2::scale_x_date(labels = english_month_year) +
    ggplot2::scale_y_continuous(
      labels = function(x) sprintf("%02d:%02d", as.integer(floor(x / 60)), as.integer(round(x %% 60))),
      breaks = function(x) pretty(x, n = 6),
      minor_breaks = NULL
    ) +
    ggplot2::scale_color_manual(values = athlytics_palette_vibrant(), name = "Distance") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Activity Date",
      y = "Best Time (MM:SS)",
      caption = caption
    ) +
    theme_athlytics() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 4)))

  # Add trend line if requested (though typically not used for PB plots)
  if (add_trend_line) {
    # Use linear model (lm) instead of loess to avoid errors with sparse data
    # and to better represent the overall improvement trend
    p <- p + ggplot2::geom_smooth(
      method = "lm", se = TRUE, ggplot2::aes(group = .data$distance_label),
      linewidth = 1.0, alpha = 0.15, linetype = "dashed"
    )
  }

  if (length(unique(pbs_df$distance_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ .data$distance_label, scales = "free", ncol = facet_ncol) +
      ggplot2::theme(
        panel.spacing = ggplot2::unit(1.5, "lines"),
        aspect.ratio = 0.75, # Wider facets (height/width = 0.75) for balanced layout
        strip.text = ggplot2::element_text(size = 10, face = "bold", color = "#2c3e50")
      )
  }

  return(p)
}
