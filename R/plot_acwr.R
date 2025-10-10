# R/plot_acwr.R

#' Plot ACWR Trend
#'
#' Visualizes the Acute:Chronic Workload Ratio (ACWR) trend over time.
#'
#' @param data **Recommended: Pass pre-calculated data via `acwr_df` (local export preferred).**
#'   A data frame from `calculate_acwr()` or activities data from `load_local_activities()`.
#' @param activity_type Type(s) of activities to analyze (e.g., "Run", "Ride").
#' @param load_metric Method for calculating daily load (e.g., "duration_mins", "distance_km", "tss", "hrss").
#' @param acute_period Days for the acute load window (e.g., 7).
#' @param chronic_period Days for the chronic load window (e.g., 28). Must be greater than `acute_period`.
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to ~1 year ago.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#' @param user_ftp Required if `load_metric = "tss"` and `acwr_df` is not provided. Your Functional Threshold Power.
#' @param user_max_hr Required if `load_metric = "hrss"` and `acwr_df` is not provided. Your maximum heart rate.
#' @param user_resting_hr Required if `load_metric = "hrss"` and `acwr_df` is not provided. Your resting heart rate.
#' @param smoothing_period Days for smoothing the ACWR using a rolling mean (e.g., 7). Default 7.
#' @param highlight_zones Logical, whether to highlight different ACWR zones (e.g., sweet spot, high risk) on the plot. Default `TRUE`.
#' @param acwr_df **Recommended.** A pre-calculated data frame from `calculate_acwr()` or `calculate_acwr_ewma()`.
#'   When provided, analysis uses local data only (no API calls).
#' @param group_var Optional. Column name for grouping/faceting (e.g., "athlete_id").
#' @param group_colors Optional. Named vector of colors for groups.
#'
#' @return A ggplot object showing the ACWR trend.
#'
#' @details Plots the ACWR trend over time. **Best practice: Use `load_local_activities()` + `calculate_acwr()` + this function.**
#'   ACWR is calculated as acute load / chronic load. A ratio of 0.8-1.3 is often considered the "sweet spot".
#'
#' 
#' @importFrom dplyr filter select mutate group_by summarise arrange %>% left_join coalesce case_when ungroup
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom zoo rollmean
#' @importFrom tidyr drop_na
#' @importFrom rlang .data %||%
#' @export
#'
#' @examples
#' # Example using pre-calculated sample data
#' data("athlytics_sample_acwr", package = "Athlytics")
#' p <- plot_acwr(athlytics_sample_acwr)
#' print(p)
#'
#' \dontrun{
#' # Example using local Strava export data
#' activities <- load_local_activities("strava_export_data/activities.csv")
#' 
#' # Plot ACWR trend for Runs (using duration as load metric)
#' plot_acwr(data = activities,
#'           activity_type = "Run",
#'           load_metric = "duration_mins",
#'           acute_period = 7,
#'           chronic_period = 28)
#'
#' # Plot ACWR trend for Rides (using TSS as load metric)
#' plot_acwr(data = activities,
#'           activity_type = "Ride",
#'           load_metric = "tss",
#'           user_ftp = 280)  # FTP value is required
#' }
plot_acwr <- function(data,
                      activity_type = NULL,
                      load_metric = "duration_mins",
                      acute_period = 7,
                      chronic_period = 28,
                      start_date = NULL,
                      end_date = NULL,
                      user_ftp = NULL,
                      user_max_hr = NULL,
                      user_resting_hr = NULL,
                      smoothing_period = 7,
                      highlight_zones = TRUE,
                      acwr_df = NULL,
                      group_var = NULL,
                      group_colors = NULL) {
  
  # --- Check if first argument is already ACWR data frame ---
  # This allows backward compatibility: plot_acwr(acwr_result)
  if (is.data.frame(data) && "acwr_smooth" %in% colnames(data)) {
    acwr_df <- data
  }
  
  # --- Get Data --- 
  # If acwr_df is not provided, calculate it
  if (is.null(acwr_df)) {
      # Check if data provided when acwr_df is not
      if (missing(data)) stop("Either provide ACWR data frame from calculate_acwr() as first argument, or provide activities_data.")
      
      # data should be activities_data in new usage
      acwr_df <- calculate_acwr(
          activities_data = data,
          activity_type = activity_type,
          load_metric = load_metric,
          acute_period = acute_period,
          chronic_period = chronic_period,
          start_date = start_date,
          end_date = end_date,
          user_ftp = user_ftp,
          user_max_hr = user_max_hr,
          user_resting_hr = user_resting_hr,
          smoothing_period = smoothing_period
      )
  } 
  
  # Check if acwr_df is empty or invalid after potentially calculating it
  # Check if required 'acwr_smooth' column exists
  if (!is.data.frame(acwr_df) || nrow(acwr_df) == 0 || !"acwr_smooth" %in% colnames(acwr_df)) {
      warning("No valid ACWR data available to plot (or missing 'acwr_smooth' column).")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No ACWR data available")) 
  }
  
  # Drop rows where smoothed ACWR is NA for plotting purposes
  plot_data <- acwr_df %>% tidyr::drop_na("acwr_smooth")
  
  if (nrow(plot_data) == 0) {
    # It's possible all rows were NA after smoothing
    warning("No valid smoothed ACWR data available for plotting after removing NAs.")
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No smoothed ACWR data available"))
  }

  # --- Check for group variable ---
  has_groups <- !is.null(group_var) && group_var %in% colnames(plot_data)
  
  # --- Plotting ---
  message("Generating plot...")
  
  if (has_groups) {
    # Multi-group plotting
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$acwr_smooth, 
                                                  color = .data[[group_var]], 
                                                  group = .data[[group_var]]))
  } else {
    # Single group plotting
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$acwr_smooth))
  }

  # Add risk zone shading
  if (highlight_zones) {
    # Define zones - these are common but can be adjusted
    sweet_spot_min <- 0.8
    sweet_spot_max <- 1.3
    high_risk_min <- 1.5

    p <- p +
      # High Risk Zone (e.g., > 1.5)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = high_risk_min, ymax = Inf), fill = "#E64B35", alpha = 0.15) +
      # Caution Zone (e.g., 1.3 - 1.5)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = sweet_spot_max, ymax = high_risk_min), fill = "#F39B7F", alpha = 0.15) +
      # Sweet Spot (e.g., 0.8 - 1.3)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = sweet_spot_min, ymax = sweet_spot_max), fill = "#00A087", alpha = 0.15) +
       # Low Load / Undertraining Zone (e.g., < 0.8)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = -Inf, ymax = sweet_spot_min), fill = "#4DBBD5", alpha = 0.15)
      
      # Add annotations only if there's enough space/range
      plot_date_range <- range(plot_data$date)
      plot_y_range <- range(plot_data$acwr_smooth)
      annotation_x_pos <- plot_date_range[1] + lubridate::days(round(as.numeric(diff(plot_date_range)) * 0.05))
      
      if(plot_y_range[2] > high_risk_min) {
        p <- p + ggplot2::annotate("text", x = annotation_x_pos, y = min(plot_y_range[2], high_risk_min + 0.2), label = "High Risk", hjust = 0, vjust = 1, size = 3, color = "#E64B35", alpha = 0.8, fontface = "bold")
      }
       if(plot_y_range[2] > sweet_spot_max) {
        p <- p + ggplot2::annotate("text", x = annotation_x_pos, y = min(plot_y_range[2], (sweet_spot_max + high_risk_min)/2), label = "Caution", hjust = 0, vjust = 0.5, size = 3, color = "#F39B7F", alpha = 0.8, fontface = "bold")
       }
       p <- p + ggplot2::annotate("text", x = annotation_x_pos, y = (sweet_spot_min + sweet_spot_max) / 2, label = "Sweet Spot", hjust = 0, vjust = 0.5, size = 3, color = "#00A087", alpha = 0.8, fontface = "bold")
       if(plot_y_range[1] < sweet_spot_min) {
        p <- p + ggplot2::annotate("text", x = annotation_x_pos, y = max(plot_y_range[1], sweet_spot_min - 0.1), label = "Low Load", hjust = 0, vjust = 0, size = 3, color = "#4DBBD5", alpha = 0.8, fontface = "bold")
      }

  }

  # Add ACWR line(s)
  if (has_groups) {
    p <- p + ggplot2::geom_line(linewidth = 1.2, alpha = 0.8)
    
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
    p <- p + ggplot2::geom_line(color = "#3C5488", linewidth = 1.2)
  }

  # Define y-limits and breaks for better scaling
  y_max_limit <- max(plot_data$acwr_smooth, ifelse(highlight_zones, high_risk_min + 0.2, 1.5), na.rm = TRUE)
  y_breaks <- seq(0, ceiling(y_max_limit * 5) / 5, by = 0.2) # Breaks every 0.2

  # Add reference lines (optional)
  if(highlight_zones) {
    p <- p + 
        ggplot2::geom_hline(yintercept = sweet_spot_min, linetype = "dotted", color = "grey40") +
        ggplot2::geom_hline(yintercept = sweet_spot_max, linetype = "dotted", color = "grey40") +
        ggplot2::geom_hline(yintercept = high_risk_min, linetype = "dotted", color = "grey40")
  }

  # Customize plot appearance
  p <- p +
    ggplot2::labs(
      title = "Acute:Chronic Workload Ratio (ACWR) Trend",
      subtitle = paste0("Load Metric: ", load_metric,
                       ", Activity: ", ifelse(is.null(activity_type), "All", paste(activity_type, collapse=", ")), 
                       ", Periods: ", acute_period, "d (Acute) / ", chronic_period, "d (Chronic)",
                       ", Smoothed: ", smoothing_period, "d"),
      x = "Date",
      y = paste0("ACWR (", smoothing_period, "-day Smoothed)")
    ) +
    ggplot2::scale_y_continuous(limits = c(0, y_max_limit), breaks = y_breaks) +
    ggplot2::scale_x_date(labels = english_month_year, date_breaks = "3 months") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 11, face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 14, margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40", margin = ggplot2::margin(b = 15)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.3),
      legend.position = if(has_groups) "bottom" else "none",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9)
    )
                   
  return(p)
}


# 1. Implement the actual Strava data fetching logic (replace placeholder).
#    - Need a helper function `fetch_strava_activities`.
#    - Handle pagination and potential API rate limits.
#    - Consider using `rStrava::get_activity_list` then loop `rStrava::get_activity`? Or is there a bulk way? Check rStrava docs.
# 2. Implement TSS calculation robustly (requires Normalized Power ideally).
# 3. Implement HR TRIMP calculation.
# 4. Add more input validation for parameters.
# 5. Refine error handling and messages.
# 6. Consider options for handling missing data (power, HR).
# 7. Create the helper function e.g. in R/utils.R or R/strava_helpers.R 
