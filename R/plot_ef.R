# R/plot_ef.R

#' Plot Efficiency Factor (EF) Trend
#'
#' Visualizes the trend of Efficiency Factor (EF) over time.
#'
#' @param data A data frame from `calculate_ef()`.
#'   Must contain `date`, `ef_value`, and `activity_type` columns.
#' @param add_trend_line Add a smoothed trend line (`geom_smooth`)? Default `TRUE`.
#' @param smoothing_method Smoothing method for trend line (e.g., "loess", "lm"). Default "loess".
#' @param smooth_per_activity_type Logical. If `TRUE` and `add_trend_line = TRUE`, draws separate
#'   trend lines for each activity type. Default `FALSE` (single trend line for all data).
#'   Note: this parameter only applies when `group_var = NULL`. When `group_var` is set,
#'   smoothing is always done per group and this parameter is ignored with a warning.
#' @param group_var Optional. Column name for grouping/faceting (e.g., "athlete_id").
#' @param group_colors Optional. Named vector of colors for groups.
#' @param title Optional. Custom title for the plot.
#' @param subtitle Optional. Custom subtitle for the plot.
#' @param ... Additional arguments.
#'   Arguments `activity_type`, `ef_metric`, `start_date`, `end_date`,
#'   `min_duration_mins`, `ef_df` are deprecated and ignored.
#'
#' @return A ggplot object showing the EF trend.
#'
#' @details Plots EF (output/HR based on activity averages).
#'   **Best practice: Use `calculate_ef()` first, then pass the result to this function.**
#'
#' @export
#'
#' @examples
#' # Example using pre-calculated sample data
#' data("sample_ef", package = "Athlytics")
#' p <- plot_ef(sample_ef)
#' print(p)
#'
plot_ef <- function(data,
                    add_trend_line = TRUE,
                    smoothing_method = "loess",
                    smooth_per_activity_type = FALSE,
                    group_var = NULL,
                    group_colors = NULL,
                    title = NULL,
                    subtitle = NULL,
                    ...) {
  # Check for deprecated args
  deprecated_args <- list(...)
  if (length(deprecated_args) > 0) {
    analysis_args <- c(
      "activity_type", "ef_metric", "start_date", "end_date",
      "min_duration_mins", "ef_df"
    )
    if (any(names(deprecated_args) %in% analysis_args)) {
      warning(
        "Analysis arguments (e.g. activity_type) are deprecated in plot_ef(). ",
        "Please use calculate_ef() first, then pass the result to plot_ef()."
      )
    }
  }

  # Validate input
  ef_df <- data
  if (!is.data.frame(ef_df)) {
    stop("Input 'data' must be a data frame from calculate_ef().")
  }

  if (!inherits(ef_df, "athlytics_ef") && !all(c("date", "ef_value", "activity_type") %in% names(ef_df))) {
    stop("Input 'data' must be the output of calculate_ef() or contain 'date', 'ef_value', and 'activity_type' columns.")
  }

  if (nrow(ef_df) == 0) {
    stop("Input data frame is empty.")
  }

  if (!all(c("date", "ef_value", "activity_type") %in% names(ef_df))) {
    stop("Input data must contain 'date', 'ef_value', and 'activity_type' columns.")
  }

  # Rename for clarity
  plot_data <- ef_df

  # --- Check for group variable ---
  has_groups <- !is.null(group_var) && group_var %in% colnames(plot_data)

  # Retrieve params for labeling
  params <- attr(ef_df, "params")
  ef_metric_label <- params$ef_metric
  if (is.null(ef_metric_label)) ef_metric_label <- "Unknown Metric"

  # --- Plotting ---
  y_label <- switch(ef_metric_label,
    "speed_hr" = "Efficiency Factor (Speed [m/s] / HR)",
    "pace_hr" = "Efficiency Factor (Speed [m/s] / HR)", # legacy alias
    "power_hr" = "Efficiency Factor (Power [W] / HR)",
    "gap_hr" = "Efficiency Factor (GAP [m/s] / HR)",
    "Efficiency Factor"
  )

  if (is.null(subtitle)) {
    if (!is.null(params)) {
      subtitle <- paste0(
        "Metric: ", ef_metric_label,
        ", Activity: ", paste(params$activity_type, collapse = ", ")
      )
    } else {
      subtitle <- paste("Metric:", ef_metric_label)
    }
  }

  if (is.null(title)) title <- "Efficiency Factor (EF) Trend"

  if (has_groups) {
    # Warn if smooth_per_activity_type is set but will be ignored
    if (isTRUE(smooth_per_activity_type)) {
      warning(
        "'smooth_per_activity_type' is ignored when 'group_var' is set. ",
        "Smoothing is done per group ('", group_var, "') instead."
      )
    }

    # Multi-group plotting
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = .data$date, y = .data$ef_value,
      color = .data[[group_var]]
    )) +
      ggplot2::geom_point(alpha = 0.7, size = 2.5) +
      ggplot2::scale_x_date(labels = english_month_year)

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
        title = title,
        subtitle = subtitle,
        x = "Date",
        y = y_label
      )

    if (add_trend_line) {
      p <- p + ggplot2::geom_smooth(ggplot2::aes(group = .data[[group_var]]),
        method = smoothing_method, se = FALSE, linewidth = 0.8
      )
    }
  } else {
    # Single group plotting (original logic)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$ef_value)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$activity_type), alpha = 0.7, size = 2) +
      ggplot2::scale_x_date(labels = english_month_year) +
      ggplot2::scale_color_manual(
        values = athlytics_palette_nature(),
        name = "Activity Type"
      ) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        x = "Date",
        y = y_label,
        color = "Activity Type"
      )

    if (add_trend_line) {
      if (isTRUE(smooth_per_activity_type)) {
        # Smooth separately for each activity type
        p <- p + ggplot2::geom_smooth(
          ggplot2::aes(group = .data$activity_type, color = .data$activity_type),
          method = smoothing_method, se = FALSE, linewidth = 0.8
        )
      } else {
        # Single trend line for all data
        p <- p + ggplot2::geom_smooth(method = smoothing_method, se = FALSE, color = "blue", linewidth = 0.8)
      }
    }
  }

  p <- p +
    theme_athlytics() +
    ggplot2::theme(
      legend.position = "bottom"
    )

  return(p)
}
