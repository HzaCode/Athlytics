# R/plot_acwr.R

#' Plot ACWR Trend
#'
#' Visualizes the Acute:Chronic Workload Ratio (ACWR) trend over time.
#'
#' @param data A data frame from `calculate_acwr()` or `calculate_acwr_ewma()`.
#'   Must contain `date` and `acwr_smooth` columns.
#' @param highlight_zones Logical, whether to highlight descriptive ACWR bands
#'   (e.g., reference band, high ACWR) on the plot. Default `TRUE`.
#' @param sweet_spot_min Lower bound for the reference ACWR band. Default 0.8.
#' @param sweet_spot_max Upper bound for the reference ACWR band. Default 1.3.
#' @param high_risk_min Lower bound for the high-ACWR band. Default 1.5.
#' @param group_var Optional. Column name for grouping/faceting (e.g., "athlete_id").
#' @param group_colors Optional. Named vector of colors for groups.
#' @param title Optional. Custom title for the plot.
#' @param subtitle Optional. Custom subtitle for the plot.
#' @param ... Additional arguments.
#'   Arguments `activity_type`, `load_metric`, `acute_period`, `chronic_period`,
#'   `start_date`, `end_date`, `user_ftp`, `user_max_hr`, `user_resting_hr`,
#'   `smoothing_period`, `acwr_df` are deprecated and ignored.
#'
#' @return A ggplot object showing the ACWR trend.
#'
#' @details Plots the ACWR trend over time.
#'   **Best practice: Use `calculate_acwr()` first, then pass the result to this function.**
#'   ACWR is calculated as acute load / chronic load. The default 0.8-1.3
#'   band is a commonly used reference band.
#'
#'   When `highlight_zones = TRUE`, all zone labels (High ACWR, Elevated ACWR,
#'   Reference Band, Low ACWR) are **always displayed** regardless of whether
#'   data falls within each zone.
#'   The y-axis is automatically extended to ensure all zone annotations remain visible.
#'   Zone boundaries can be customised via `sweet_spot_min`, `sweet_spot_max`, and
#'   `high_risk_min`.
#'
#'   **Note:** The predictive value of ACWR for injury outcomes is debated in the
#'   literature (Impellizzeri et al., 2020). Zone labels should be interpreted
#'   as descriptive workload heuristics, not validated injury predictors. See
#'   `calculate_acwr()` documentation for full references.
#'
#' @export
#'
#' @examples
#' # Example using pre-calculated sample data
#' data("sample_acwr", package = "Athlytics")
#' p <- plot_acwr(sample_acwr)
#' print(p)
#'
plot_acwr <- function(data,
                      highlight_zones = TRUE,
                      sweet_spot_min = 0.8,
                      sweet_spot_max = 1.3,
                      high_risk_min = 1.5,
                      group_var = NULL,
                      group_colors = NULL,
                      title = NULL,
                      subtitle = NULL,
                      ...) {
  # Check for deprecated args
  deprecated_args <- list(...)
  if (length(deprecated_args) > 0) {
    analysis_args <- c(
      "activity_type", "load_metric", "acute_period", "chronic_period",
      "start_date", "end_date", "user_ftp", "user_max_hr",
      "user_resting_hr", "smoothing_period", "acwr_df"
    )

    if (any(names(deprecated_args) %in% analysis_args)) {
      warning(
        "Analysis arguments (e.g. activity_type, load_metric) are deprecated in plot_acwr(). ",
        "Please use calculate_acwr() first, then pass the result to plot_acwr()."
      )
    }
  }

  # Validate input
  acwr_df <- data
  if (!is.data.frame(acwr_df)) {
    stop("Input 'data' must be a data frame from calculate_acwr().")
  }

  if (!inherits(acwr_df, "athlytics_acwr") && !all(c("date", "acwr_smooth") %in% names(acwr_df))) {
    if (!all(c("date") %in% names(acwr_df)) || !any(c("acwr_smooth", "acwr") %in% names(acwr_df))) {
      stop("Input 'data' must be the output of calculate_acwr() or contain 'date' and 'acwr_smooth' columns.")
    }
  }

  if (nrow(acwr_df) == 0) {
    stop("Input data frame is empty.")
  }

  if (!"acwr_smooth" %in% colnames(acwr_df)) {
    # Check for legacy raw ACWR
    if ("acwr" %in% colnames(acwr_df)) {
      warning("Column 'acwr_smooth' not found, using 'acwr'. Consider using smoothing in calculate_acwr().")
      acwr_df$acwr_smooth <- acwr_df$acwr
    } else {
      stop("Input data must contain 'acwr_smooth' (or 'acwr') column.")
    }
  }

  # Drop rows where smoothed ACWR is NA for plotting purposes
  plot_data <- acwr_df %>% dplyr::filter(!is.na(.data$acwr_smooth))

  if (nrow(plot_data) == 0) {
    stop("No valid smoothed ACWR data available for plotting after removing NAs.")
  }

  # --- Check for group variable ---
  has_groups <- !is.null(group_var) && group_var %in% colnames(plot_data)

  # --- Plotting ---

  if (has_groups) {
    # Multi-group plotting
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = .data$date, y = .data$acwr_smooth,
      color = .data[[group_var]],
      group = .data[[group_var]]
    ))
  } else {
    # Single group plotting
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$acwr_smooth))
  }

  # --- Compute y-axis limits first (needed for finite ribbon bounds) ---
  if (highlight_zones) {
    if (!is.numeric(sweet_spot_min) || length(sweet_spot_min) != 1) stop("`sweet_spot_min` must be a single numeric value.")
    if (!is.numeric(sweet_spot_max) || length(sweet_spot_max) != 1) stop("`sweet_spot_max` must be a single numeric value.")
    if (!is.numeric(high_risk_min) || length(high_risk_min) != 1) stop("`high_risk_min` must be a single numeric value.")
    if (sweet_spot_min >= sweet_spot_max) stop("`sweet_spot_min` must be less than `sweet_spot_max`.")
    if (high_risk_min <= sweet_spot_max) stop("`high_risk_min` must be greater than `sweet_spot_max`.")

    # Ensure y-axis extends enough to show all zone annotations with margin
    zone_y_ref <- high_risk_min + 0.5
  } else {
    zone_y_ref <- 1.5
  }
  y_max_limit <- max(plot_data$acwr_smooth, zone_y_ref, na.rm = TRUE)
  y_breaks <- seq(0, ceiling(y_max_limit * 5) / 5, by = 0.2)
  date_bounds <- if (highlight_zones) padded_date_range(plot_data$date) else NULL

  # --- Add ACWR zone shading (using annotate to avoid inheriting group aes) ---
  if (highlight_zones) {
    date_xmin <- date_bounds[1]
    date_xmax <- date_bounds[2]

    p <- p +
      # High ACWR zone (e.g., > 1.5)
      ggplot2::annotate("rect", xmin = date_xmin, xmax = date_xmax,
        ymin = high_risk_min, ymax = Inf, fill = "#E64B35", alpha = 0.06) +
      # Elevated ACWR band (e.g., 1.3 - 1.5)
      ggplot2::annotate("rect", xmin = date_xmin, xmax = date_xmax,
        ymin = sweet_spot_max, ymax = high_risk_min, fill = "#F39B7F", alpha = 0.06) +
      # Reference band (e.g., 0.8 - 1.3)
      ggplot2::annotate("rect", xmin = date_xmin, xmax = date_xmax,
        ymin = sweet_spot_min, ymax = sweet_spot_max, fill = "#00A087", alpha = 0.06) +
      # Low ACWR band (e.g., < 0.8)
      ggplot2::annotate("rect", xmin = date_xmin, xmax = date_xmax,
        ymin = -Inf, ymax = sweet_spot_min, fill = "#4DBBD5", alpha = 0.06)

    # Always show all ACWR zone annotations regardless of data range
    plot_date_range <- range(plot_data$date, na.rm = TRUE)
    annotation_x_pos <- plot_date_range[2] - lubridate::days(round(as.numeric(diff(plot_date_range)) * 0.02))

    p <- p +
      ggplot2::annotate("text",
        x = annotation_x_pos, y = high_risk_min + 0.15,
        label = "High ACWR", hjust = 1, vjust = 0.5, size = 2.8, color = "#E64B35", alpha = 0.7, fontface = "italic"
      ) +
      ggplot2::annotate("text",
        x = annotation_x_pos, y = (sweet_spot_max + high_risk_min) / 2,
        label = "Elevated ACWR", hjust = 1, vjust = 0.5, size = 2.8, color = "#F39B7F", alpha = 0.7, fontface = "italic"
      ) +
      ggplot2::annotate("text",
        x = annotation_x_pos, y = (sweet_spot_min + sweet_spot_max) / 2,
        label = "Reference Band", hjust = 1, vjust = 0.5, size = 2.8, color = "#00A087", alpha = 0.7, fontface = "italic"
      ) +
      ggplot2::annotate("text",
        x = annotation_x_pos, y = sweet_spot_min / 2,
        label = "Low ACWR", hjust = 1, vjust = 0.5, size = 2.8, color = "#4DBBD5", alpha = 0.7, fontface = "italic"
      )

    # Zone boundary reference lines
    p <- p +
      ggplot2::geom_hline(yintercept = sweet_spot_min, linetype = "dotted", color = "grey40") +
      ggplot2::geom_hline(yintercept = sweet_spot_max, linetype = "dotted", color = "grey40") +
      ggplot2::geom_hline(yintercept = high_risk_min, linetype = "dotted", color = "grey40")
  }

  # --- Add ACWR line(s) ---
  if (has_groups) {
    p <- p + plot_lines(linewidth = 0.9, alpha = 0.8)

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
  } else {
    p <- p + plot_lines(mapping = ggplot2::aes(x = .data$date, y = .data$acwr_smooth), color = "#c00000", linewidth = 1)
  }

  # Retrieve params for labeling
  params <- attr(acwr_df, "params")

  # Construct subtitle if not provided
  if (is.null(subtitle)) {
    if (!is.null(params)) {
      subtitle_parts <- c()
      if (!is.null(params$load_metric)) subtitle_parts <- c(subtitle_parts, paste0("Metric: ", params$load_metric))
      if (!is.null(params$activity_type)) subtitle_parts <- c(subtitle_parts, paste0("Activity: ", paste(params$activity_type, collapse = ", ")))
      if (!is.null(params$acute_period) && !is.null(params$chronic_period)) {
        subtitle_parts <- c(subtitle_parts, paste0(params$acute_period, "/", params$chronic_period, "d"))
      }
      subtitle <- paste(subtitle_parts, collapse = " | ")
    } else {
      subtitle <- "ACWR Trend"
    }
  }

  if (is.null(title)) title <- "Acute:Chronic Workload Ratio"
  x_scale <- if (highlight_zones) {
    ggplot2::scale_x_date(
      labels = english_month_year,
      limits = date_bounds,
      expand = ggplot2::expansion(mult = 0)
    )
  } else {
    ggplot2::scale_x_date(labels = english_month_year)
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Date",
      y = "ACWR"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, y_max_limit), breaks = y_breaks) +
    x_scale +
    theme_athlytics() +
    ggplot2::theme(
      legend.position = if (has_groups) "bottom" else "none"
    )

  return(p)
}
