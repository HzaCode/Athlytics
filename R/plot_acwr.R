# R/plot_acwr.R

#' Plot ACWR Trend
#'
#' Visualizes the Acute:Chronic Workload Ratio (ACWR) trend over time.
#'
#' @param stoken A valid Strava token from `rStrava::strava_oauth()`. Required unless `acwr_df` is provided.
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
#' @param acwr_df Optional. A pre-calculated data frame from `calculate_acwr`.
#'   If provided, `stoken` and other calculation parameters are ignored.
#'
#' @return A ggplot object showing the ACWR trend.
#'
#' @details Plots the ACWR trend over time. Uses pre-calculated data or calls `calculate_acwr` (can be slow).
#'   ACWR is calculated as acute load / chronic load. A ratio of 0.8-1.3 is often considered the "sweet spot".
#'   If `acwr_df` is not provided, calls `calculate_acwr` first (can be slow and hit API limits).
#'
#' @importFrom rStrava get_activity_list
#' @importFrom dplyr filter select mutate group_by summarise arrange %>% left_join coalesce case_when ungroup
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom zoo rollmean
#' @importFrom tidyr drop_na
#' @importFrom rlang .data %||%
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(Athlytics_sample_data)
#' if (!is.null(athlytics_sample_acwr)) {
#'   # Ensure acwr_df is named and other necessary parameters are provided if plot_acwr expects them
#'   p <- plot_acwr(acwr_df = athlytics_sample_acwr)
#'   print(p)
#' }
#'
#' \donttest{
#' # Example using real data (requires authentication)
#' # Please replace with your actual Strava application details for this to work.
#' # stoken_example <- rStrava::strava_oauth(
#' #   app_name = "YOUR_APP_NAME_PLACEHOLDER",
#' #   client_id = "YOUR_CLIENT_ID_PLACEHOLDER",
#' #   client_secret = "YOUR_CLIENT_SECRET_PLACEHOLDER",
#' #   cache = TRUE
#' # )
#'
#' # If you have a valid stoken_example, you can then use it:
#' # Plot ACWR trend for Runs (using duration as load metric)
#' # if (exists("stoken_example") && inherits(stoken_example, "Token2.0")) {
#' #   plot_acwr(stoken = stoken_example,
#' #             activity_type = "Run",
#' #             load_metric = "duration_mins",
#' #             acute_period = 7,
#' #             chronic_period = 28)
#' #
#' #   # Plot ACWR trend for Rides (using TSS as load metric)
#' #   plot_acwr(stoken = stoken_example,
#' #             activity_type = "Ride",
#' #             load_metric = "tss",
#' #             user_ftp = 280)  # FTP value is required
#' # } else {
#' #  message("stoken_example not created or invalid. Skipping real data example for plot_acwr.")
#' # }
#' }
plot_acwr <- function(stoken,
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
                      acwr_df = NULL) {
  # --- Get Data --- 
  # If acwr_df is not provided, calculate it
  if (is.null(acwr_df)) {
      # Ensure stoken is provided if acwr_df is not
      if (missing(stoken)) stop("Either 'stoken' or 'acwr_df' must be provided.")
      
      # Call the calculation function
      acwr_df <- calculate_acwr(
          stoken = stoken,
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
  # Ensure the required 'acwr_smooth' column exists
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

  # --- Plotting ---
  message("Generating plot...")
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$acwr_smooth))

  # Add risk zone shading
  if (highlight_zones) {
    # Define zones - these are common but can be adjusted
    sweet_spot_min <- 0.8
    sweet_spot_max <- 1.3
    high_risk_min <- 1.5

    p <- p +
      # High Risk Zone (e.g., > 1.5)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = high_risk_min, ymax = Inf), fill = "red", alpha = 0.1) +
      # Caution Zone (e.g., 1.3 - 1.5)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = sweet_spot_max, ymax = high_risk_min), fill = "orange", alpha = 0.1) +
      # Sweet Spot (e.g., 0.8 - 1.3)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = sweet_spot_min, ymax = sweet_spot_max), fill = "green", alpha = 0.1) +
       # Low Load / Undertraining Zone (e.g., < 0.8)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = -Inf, ymax = sweet_spot_min), fill = "lightblue", alpha = 0.1)
      
      # Add annotations only if there's enough space/range
      plot_date_range <- range(plot_data$date)
      plot_y_range <- range(plot_data$acwr_smooth)
      annotation_x_pos <- plot_date_range[1] + lubridate::days(round(as.numeric(diff(plot_date_range)) * 0.05))
      
      if(plot_y_range[2] > high_risk_min) {
        p <- p + ggplot2::annotate("text", x = annotation_x_pos, y = min(plot_y_range[2], high_risk_min + 0.2), label = "High Risk", hjust = 0, vjust = 1, size = 3, color = "red4", alpha = 0.7)
      }
       if(plot_y_range[2] > sweet_spot_max) {
        p <- p + ggplot2::annotate("text", x = annotation_x_pos, y = min(plot_y_range[2], (sweet_spot_max + high_risk_min)/2), label = "Caution", hjust = 0, vjust = 0.5, size = 3, color = "orange4", alpha = 0.7)
       }
       p <- p + ggplot2::annotate("text", x = annotation_x_pos, y = (sweet_spot_min + sweet_spot_max) / 2, label = "Sweet Spot", hjust = 0, vjust = 0.5, size = 3, color = "green4", alpha = 0.7)
       if(plot_y_range[1] < sweet_spot_min) {
        p <- p + ggplot2::annotate("text", x = annotation_x_pos, y = max(plot_y_range[1], sweet_spot_min - 0.1), label = "Low Load", hjust = 0, vjust = 0, size = 3, color = "blue4", alpha = 0.7)
      }

  }

  # Add ACWR line
  p <- p + ggplot2::geom_line(color = "black", linewidth = 1)

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
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   plot.title = ggplot2::element_text(face = "bold"))
                   
  return(p)
}

# TODO:
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