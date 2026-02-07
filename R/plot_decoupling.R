# R/plot_decoupling.R

#' Plot Aerobic Decoupling Trend
#'
#' Visualizes the trend of aerobic decoupling over time.
#'
#' @param data A data frame from `calculate_decoupling()`.
#'   Must contain 'date' and 'decoupling' columns.
#' @param add_trend_line Add a smoothed trend line (`geom_smooth`)? Default `TRUE`.
#' @param smoothing_method Smoothing method for trend line (e.g., "loess", "lm"). Default "loess".
#' @param caption Plot caption. Default NULL (no caption).
#' @param title Optional. Custom title for the plot.
#' @param subtitle Optional. Custom subtitle for the plot.
#' @param ... Additional arguments.
#'   Arguments `activity_type`, `decouple_metric`, `start_date`, `end_date`,
#'   `min_duration_mins`, `decoupling_df` are deprecated and ignored.
#'
#' @return A ggplot object showing the decoupling trend.
#'
#' @details Plots decoupling percentage ((EF_1st_half - EF_2nd_half) / EF_1st_half * 100).
#'   Positive values mean HR drifted relative to output. A 5\\% threshold line is often
#'   used as reference. **Best practice: Use `calculate_decoupling()` first, then pass the result to this function.**
#'
#' @export
#'
#' @examples
#' # Example using pre-calculated sample data
#' data("sample_decoupling", package = "Athlytics")
#' p <- plot_decoupling(sample_decoupling)
#' print(p)
#'
#' # Runnable example with a manually created decoupling data frame:
#' decoupling_df <- data.frame(
#'   date = seq(Sys.Date() - 29, Sys.Date(), by = "day"),
#'   decoupling = rnorm(30, mean = 5, sd = 2)
#' )
#' plot_decoupling(data = decoupling_df)
#'
plot_decoupling <- function(data,
                            add_trend_line = TRUE,
                            smoothing_method = "loess",
                            caption = NULL,
                            title = NULL,
                            subtitle = NULL,
                            ...) {
  # Check for deprecated args
  deprecated_args <- list(...)
  if (length(deprecated_args) > 0) {
    analysis_args <- c(
      "activity_type", "decouple_metric", "start_date", "end_date",
      "min_duration_mins", "decoupling_df"
    )
    if (any(names(deprecated_args) %in% analysis_args)) {
      warning(
        "Analysis arguments (e.g. activity_type) are deprecated in plot_decoupling(). ",
        "Please use calculate_decoupling() first, then pass the result to plot_decoupling()."
      )
    }
  }

  # Validate input
  decoupling_df <- data
  if (!is.data.frame(decoupling_df)) {
    stop("Input 'data' must be a data frame from calculate_decoupling().")
  }

  if (!inherits(decoupling_df, "athlytics_decoupling") && !all(c("date", "decoupling") %in% names(decoupling_df))) {
    stop("Input 'data' must be the output of calculate_decoupling() or contain 'date' and 'decoupling' columns.")
  }

  if (nrow(decoupling_df) == 0) {
    stop("Input data frame is empty.")
  }

  if (!all(c("date", "decoupling") %in% names(decoupling_df))) {
    stop("Input data must contain 'date' and 'decoupling' columns.")
  }

  # Rename for clarity
  plot_data <- decoupling_df

  # Retrieve params for labeling
  params <- attr(decoupling_df, "params")

  # Determine title/subtitle
  if (is.null(title)) title <- "Aerobic Decoupling Trend"

  if (is.null(subtitle)) {
    if (!is.null(params)) {
      subtitle <- paste0("Metric: ", params$decouple_metric)
      if (!is.null(params$activity_type)) {
        subtitle <- paste0(subtitle, " | Activity: ", paste(params$activity_type, collapse = ", "))
      }
    } else {
      subtitle <- "Decoupling (%)"
    }
  }

  y_label <- "Decoupling (%)"

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$decoupling)) +
    ggplot2::geom_point(alpha = 0.7, size = 2, color = "#E64B35") +
    ggplot2::scale_x_date(labels = english_month_year) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Date",
      y = y_label,
      caption = caption
    )

  # Add 5% threshold line
  p <- p + ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "red", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5)

  if (add_trend_line && nrow(plot_data) >= 2) {
    p <- p + ggplot2::geom_smooth(method = smoothing_method, se = FALSE, color = "blue", linewidth = 0.8)
  }

  p <- p +
    theme_athlytics() +
    ggplot2::theme(
      legend.position = "bottom"
    )

  return(p)
}
