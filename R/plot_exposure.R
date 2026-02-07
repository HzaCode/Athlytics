# R/plot_exposure.R

#' Plot Training Load Exposure (ATL vs CTL)
#'
#' Visualizes the relationship between Acute and Chronic Training Load.
#'
#' @param data A data frame from `calculate_exposure()`.
#'   Must contain `date`, `atl`, and `ctl` columns.
#' @param risk_zones Add background shading for typical ACWR risk zones? Default `TRUE`.
#' @param show_date_color Logical. Whether to color points by date (gradient). Default `TRUE`.
#'   The date gradient helps visualize the training trajectory over time: lighter colors
#'   represent earlier dates and darker colors represent more recent dates, so you can
#'   trace how training state has evolved across a season.
#'   Set to `FALSE` for a simpler single-color plot (useful when the temporal ordering
#'   is less important than the overall distribution).
#' @param caption Plot caption. Default NULL (no caption).
#' @param axis_limit Optional. Numeric value to set both x and y axis limits (0 to axis_limit).
#'   Useful when plotting risk zones without data or with sparse data. Default NULL (auto-scale).
#' @param title Optional. Custom title for the plot.
#' @param subtitle Optional. Custom subtitle for the plot.
#' @param ... Additional arguments.
#'   Arguments `activity_type`, `load_metric`, `acute_period`, `chronic_period`,
#'   `user_ftp`, `user_max_hr`, `user_resting_hr`, `end_date`, `exposure_df` are deprecated and ignored.
#'
#' @return A ggplot object showing ATL vs CTL.
#'
#' @details Visualizes training state by plotting ATL vs CTL (related to PMC charts).
#'   Points are colored by date, latest point is highlighted (red triangle).
#'   Optional risk zones (based on ACWR thresholds ~0.8, 1.3, 1.5) can be shaded.
#'   **Best practice: Use `calculate_exposure()` first, then pass the result to this function.**
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(sample_exposure)
#' # Ensure exposure_df is named and other necessary parameters like activity_type are provided
#' p <- plot_exposure(sample_exposure)
#' print(p)
#'
#' # Runnable example with dummy data:
#' end <- Sys.Date()
#' dates <- seq(end - 59, end, by = "day")
#' dummy_activities <- data.frame(
#'   date = dates,
#'   type = "Run",
#'   moving_time = rep(3600, length(dates)), # 1 hour
#'   distance = rep(10000, length(dates)), # 10 km
#'   average_heartrate = rep(140, length(dates)),
#'   suffer_score = rep(50, length(dates)),
#'   tss = rep(50, length(dates)),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Calculate Exposure (ATL/CTL)
#' exposure_result <- calculate_exposure(
#'   activities_data = dummy_activities,
#'   activity_type = "Run",
#'   load_metric = "distance_km",
#'   acute_period = 7,
#'   chronic_period = 28,
#'   end_date = end
#' )
#' plot_exposure(exposure_result)
#'
plot_exposure <- function(data,
                          risk_zones = TRUE,
                          show_date_color = TRUE,
                          caption = NULL,
                          axis_limit = NULL,
                          title = NULL,
                          subtitle = NULL,
                          ...) {
  # Check for deprecated args
  deprecated_args <- list(...)
  if (length(deprecated_args) > 0) {
    analysis_args <- c(
      "activity_type", "load_metric", "acute_period", "chronic_period",
      "user_ftp", "user_max_hr", "user_resting_hr", "end_date", "exposure_df"
    )
    if (any(names(deprecated_args) %in% analysis_args)) {
      warning(
        "Analysis arguments (e.g. activity_type) are deprecated in plot_exposure(). ",
        "Please use calculate_exposure() first, then pass the result to plot_exposure()."
      )
    }
  }

  # Validate input
  exposure_df <- data
  if (!is.data.frame(exposure_df)) {
    stop("Input 'data' must be a data frame from calculate_exposure().")
  }

  if (!inherits(exposure_df, "athlytics_exposure") && !all(c("date", "atl", "ctl") %in% names(exposure_df))) {
    stop("Input 'data' must be the output of calculate_exposure() or contain 'date', 'atl', and 'ctl' columns.")
  }

  if (nrow(exposure_df) == 0) {
    stop("Input data frame is empty.")
  }

  if (!all(c("date", "atl", "ctl") %in% names(exposure_df))) {
    stop("Input data must contain 'date', 'atl', and 'ctl' columns.")
  }

  load_ts <- exposure_df

  # Retrieve params for labeling
  params <- attr(exposure_df, "params")

  # --- Plotting ---
  plot_end_date <- max(load_ts$date)

  if (is.null(subtitle)) {
    if (!is.null(params)) {
      subtitle <- sprintf(
        "Metric: %s | Acute: %d days, Chronic: %d days | End Date: %s",
        params$load_metric, params$acute_period, params$chronic_period, plot_end_date
      )
    } else {
      subtitle <- paste("End Date:", plot_end_date)
    }
  }

  if (is.null(title)) title <- "Training Load Exposure (ATL vs CTL)"

  x_label <- "Chronic Training Load (CTL)"
  y_label <- "Acute Training Load (ATL)"

  latest_point <- load_ts %>% dplyr::filter(.data$date == plot_end_date)

  p <- ggplot2::ggplot(load_ts, ggplot2::aes(x = .data$ctl, y = .data$atl))

  if (isTRUE(show_date_color)) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = .data$date), alpha = 0.7, size = 2.5) +
      ggplot2::scale_color_gradient(
        low = athlytics_palette_nature()[1],
        high = athlytics_palette_nature()[5],
        name = "Date",
        labels = function(x) {
          d <- as.Date(x, origin = "1970-01-01")
          date_range <- diff(range(d))
          months_en <- c(
            "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
          )
          if (date_range < 180) {
            # Short range: show day + month
            paste(lubridate::day(d), months_en[lubridate::month(d)])
          } else {
            # Long range: show month + year
            paste(months_en[lubridate::month(d)], lubridate::year(d))
          }
        },
        breaks = function(lim) {
          # Use exactly 3 evenly spaced breaks (start, middle, end)
          seq(lim[1], lim[2], length.out = 3)
        },
        guide = ggplot2::guide_colorbar(
          direction = "horizontal",
          title.position = "top",
          barwidth = ggplot2::unit(8, "cm"),
          barheight = ggplot2::unit(0.4, "cm")
        )
      )
  } else {
    p <- p +
      ggplot2::geom_point(color = athlytics_palette_nature()[3], alpha = 0.7, size = 2.5)
  }

  p <- p +
    ggplot2::geom_point(data = latest_point, ggplot2::aes(x = .data$ctl, y = .data$atl), color = "#E64B35", size = 5, shape = 17) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label,
      caption = caption
    ) +
    theme_athlytics() +
    ggplot2::theme(
      legend.position = "bottom"
    )

  if (risk_zones) {
    sweet_spot_lower <- 0.8
    sweet_spot_upper <- 1.3
    danger_zone_upper <- 1.5

    # Use axis_limit if provided, otherwise calculate from data
    if (!is.null(axis_limit) && is.numeric(axis_limit) && axis_limit > 0) {
      max_ctl_limit <- axis_limit
      max_atl_limit <- axis_limit
    } else {
      max_ctl_limit <- max(0, load_ts$ctl, na.rm = TRUE) * 1.1
      max_atl_limit <- max(0, load_ts$atl, na.rm = TRUE) * 1.1
      if (max_ctl_limit == 0) max_ctl_limit <- 100 # Default for empty data
      if (max_atl_limit == 0) max_atl_limit <- 100
    }

    p <- p +
      ggplot2::geom_abline(intercept = 0, slope = sweet_spot_lower, linetype = "dotted", color = "blue") +
      ggplot2::geom_abline(intercept = 0, slope = sweet_spot_upper, linetype = "dotted", color = "orange") +
      ggplot2::geom_abline(intercept = 0, slope = danger_zone_upper, linetype = "dotted", color = "red") +
      ggplot2::coord_cartesian(xlim = c(0, max_ctl_limit), ylim = c(0, max_atl_limit), expand = FALSE)

    p <- p + ggplot2::annotate("text", x = max_ctl_limit * 0.05, y = max_atl_limit * 0.95, label = sprintf("High Risk (>%.1f)", danger_zone_upper), hjust = 0, vjust = 1, color = "red", size = 3, alpha = 0.8)
    p <- p + ggplot2::annotate("text", x = max_ctl_limit * 0.2, y = max_atl_limit * 0.7, label = sprintf("Caution (%.1f-%.1f)", sweet_spot_upper, danger_zone_upper), hjust = 0, vjust = 1, color = "orange", size = 3, alpha = 0.8)
    p <- p + ggplot2::annotate("text", x = max_ctl_limit * 0.5, y = max_atl_limit * 0.5, label = sprintf("Sweet Spot (%.1f-%.1f)", sweet_spot_lower, sweet_spot_upper), hjust = 0, vjust = 1, color = "darkgreen", size = 3, alpha = 0.8)
    p <- p + ggplot2::annotate("text", x = max_ctl_limit * 0.7, y = max_atl_limit * 0.2, label = sprintf("Low Load (<%.1f)", sweet_spot_lower), hjust = 0, vjust = 0, color = "blue", size = 3, alpha = 0.8)
  }

  return(p)
}
