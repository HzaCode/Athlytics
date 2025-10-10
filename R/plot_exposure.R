#' Plot Training Load Exposure (ATL vs CTL)
#'
#' Visualizes the relationship between Acute and Chronic Training Load.
#'
#' Plots ATL vs CTL, optionally showing risk zones based on ACWR. Uses
#' pre-calculated data or calls `calculate_exposure`.
#'
#' @param stoken A valid Strava token from `rStrava::strava_oauth()`. Required unless `exposure_df` is provided.
#' @param activity_type Type(s) of activities to include (e.g., "Run", "Ride"). Default uses common types.
#' @param load_metric Method for calculating daily load (e.g., "duration_mins", "tss", "hrss"). Default "duration_mins".
#'   See `calculate_exposure` for details on approximate TSS/HRSS calculations.
#' @param acute_period Days for acute load window (e.g., 7).
#' @param chronic_period Days for chronic load window (e.g., 42). Must be > `acute_period`.
#' @param user_ftp Required if `load_metric = "tss"`. Your FTP.
#' @param user_max_hr Required if `load_metric = "hrss"`. Your max HR.
#' @param user_resting_hr Required if `load_metric = "hrss"`. Your resting HR.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#' @param risk_zones Add background shading for typical ACWR risk zones? Default `TRUE`.
#' @param exposure_df Optional. A pre-calculated data frame from `calculate_exposure`.
#'   If provided, `stoken` and other calculation parameters are ignored. Must contain
#'   `date`, `atl`, `ctl` (and `acwr` if `risk_zones = TRUE`).
#'
#' @return A ggplot object showing ATL vs CTL.
#'
#' @details Visualizes training state by plotting ATL vs CTL (related to PMC charts).
#'   Points are colored by date, latest point is highlighted (red triangle).
#'   Optional risk zones (based on ACWR thresholds ~0.8, 1.3, 1.5) can be shaded.
#'   If `exposure_df` is not provided, it calls `calculate_exposure` first.
#'
#' 
#' @importFrom dplyr filter select mutate arrange group_by summarise ungroup lead lag rename recode full_join %>% coalesce
#' @importFrom purrr map_dfr possibly
#' @importFrom lubridate ymd_hms as_date days floor_date ceiling_date interval duration
#' @importFrom zoo rollmean
#' @import ggplot2
#' @importFrom viridis scale_color_viridis
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example using pre-calculated sample data
#' data("athlytics_sample_exposure", package = "Athlytics")
#' p <- plot_exposure(athlytics_sample_exposure)
#' print(p)
#'
#' \dontrun{
#' # Example using real data (requires authentication)
#' # stoken <- rStrava::strava_oauth("YOUR_APP_NAME",
#' #                                "YOUR_APP_CLIENT_ID",
#' #                                "YOUR_APP_SECRET",
#' #                                cache = TRUE)
#'
#' # Plot Exposure trend for Runs (last 6 months)
#' # plot_exposure(stoken = stoken, # Replace stoken with a valid token object
#' #               activity_type = "Run",
#' #               end_date = Sys.Date(), # For internal calculate_exposure: fetches prior data.
#' #               # Note: start_date applies to the internal calculate_exposure call.
#' #               user_ftp = 280) # Example, if load_metric = "tss"
#'
#' # Plot Exposure trend for Rides
#' # plot_exposure(stoken = stoken, # Replace stoken with a valid token object
#' #               activity_type = "Ride",
#' #               user_ftp = 280) # Example, provide if load_metric = "tss"
#'
#' # Plot Exposure trend for multiple Run types (risk_zones = FALSE for this example)
#' # plot_exposure(stoken = stoken, # Replace stoken with a valid token object
#' #               activity_type = c("Run", "VirtualRun"),
#' #               risk_zones = FALSE,
#' #               user_ftp = 280) # Example, provide if load_metric = "tss"
#' }
plot_exposure <- function(stoken,
                          activity_type = c("Run", "Ride", "VirtualRide", "VirtualRun"),
                          load_metric = "duration_mins",
                          acute_period = 7,
                          chronic_period = 42,
                          user_ftp = NULL,
                          user_max_hr = NULL,
                          user_resting_hr = NULL,
                          end_date = NULL,
                          risk_zones = TRUE,
                          exposure_df = NULL) {

  # --- Check if first argument is already exposure data frame ---
  if (is.data.frame(stoken) && all(c("date", "atl", "ctl") %in% colnames(stoken))) {
    exposure_df <- stoken
  }

  # --- Get Data --- 
  if (is.null(exposure_df)) {
      if (missing(stoken)) stop("Either provide exposure data frame from calculate_exposure() as first argument, or provide activities_data.")
      
      exposure_df <- calculate_exposure(
        activities_data = stoken,
        activity_type = activity_type,
        load_metric = load_metric,
        acute_period = acute_period,
        chronic_period = chronic_period,
        user_ftp = user_ftp,
        user_max_hr = user_max_hr,
        user_resting_hr = user_resting_hr,
        end_date = end_date
      )
  }
  
  if (!is.data.frame(exposure_df) || nrow(exposure_df) == 0 || !all(c("date", "atl", "ctl") %in% names(exposure_df))) {
      warning("No valid exposure data available to plot (or missing required columns).")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No exposure data available"))
  }
  
  load_ts <- exposure_df

  # --- Plotting ---
  message("Generating plot...")
  
  plot_end_date <- max(load_ts$date)

  metric_label <- switch(load_metric,
                         "duration_mins" = "Duration (mins)",
                         "distance_km" = "Distance (km)",
                         "tss" = "TSS",
                         "hrss" = "HRSS",
                         "elevation_gain_m" = "Elevation Gain (m)",
                         load_metric 
                         )

  latest_point <- load_ts %>% dplyr::filter(.data$date == plot_end_date)

  p <- ggplot2::ggplot(load_ts, ggplot2::aes(x = .data$ctl, y = .data$atl)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$date), alpha = 0.7, size = 2) + 
    viridis::scale_color_viridis(option = "plasma", name = "Date") +
    ggplot2::geom_point(data = latest_point, ggplot2::aes(x = .data$ctl, y = .data$atl), color = "red", size = 4, shape = 17) + 
    ggplot2::labs(
      title = paste("Training Load Exposure (ATL vs CTL):", metric_label),
      subtitle = sprintf("Acute: %d days, Chronic: %d days | End Date: %s",
                       acute_period, chronic_period, plot_end_date),
      x = sprintf("Chronic Training Load (CTL - %d day avg)", chronic_period),
      y = sprintf("Acute Training Load (ATL - %d day avg)", acute_period),
      caption = "Data sourced from Strava via rStrava. Red triangle is latest data point."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size=10),
        legend.position = "right"
    )

  if (risk_zones) {
      if (!"acwr" %in% colnames(load_ts)) {
          warning("ACWR column not found in calculated data. Cannot add risk zones.")
      } else {
          sweet_spot_lower <- 0.8
          sweet_spot_upper <- 1.3
          danger_zone_upper <- 1.5 
          
          max_ctl_limit <- max(0, load_ts$ctl, na.rm = TRUE) * 1.1
          max_atl_limit <- max(0, load_ts$atl, na.rm = TRUE) * 1.1
          
          if (max_ctl_limit == 0) max_ctl_limit <- 1
          if (max_atl_limit == 0) max_atl_limit <- 1
          
          p <- p + 
              ggplot2::geom_abline(intercept = 0, slope = sweet_spot_lower, linetype="dotted", color="blue") + 
              ggplot2::geom_abline(intercept = 0, slope = sweet_spot_upper, linetype="dotted", color="orange") + 
              ggplot2::geom_abline(intercept = 0, slope = danger_zone_upper, linetype="dotted", color="red") +
              ggplot2::coord_cartesian(xlim = c(0, max_ctl_limit), ylim = c(0, max_atl_limit), expand = FALSE)
          
          p <- p + ggplot2::annotate("text", x = max_ctl_limit * 0.05, y = max_atl_limit * 0.95, label = sprintf("High Risk (>%.1f)", danger_zone_upper), hjust = 0, vjust = 1, color = "red", size = 3, alpha=0.8)
          p <- p + ggplot2::annotate("text", x = max_ctl_limit * 0.2, y = max_atl_limit * 0.7, label = sprintf("Caution (%.1f-%.1f)", sweet_spot_upper, danger_zone_upper), hjust = 0, vjust = 1, color = "orange", size = 3, alpha=0.8)
          p <- p + ggplot2::annotate("text", x = max_ctl_limit * 0.5, y = max_atl_limit * 0.5, label = sprintf("Sweet Spot (%.1f-%.1f)", sweet_spot_lower, sweet_spot_upper), hjust = 0, vjust = 1, color = "darkgreen", size = 3, alpha=0.8)
          p <- p + ggplot2::annotate("text", x = max_ctl_limit * 0.7, y = max_atl_limit * 0.2, label = sprintf("Low Load (<%.1f)", sweet_spot_lower), hjust = 0, vjust = 0, color = "blue", size = 3, alpha=0.8)
       }
  }

  return(p)
}

# Helper for null default (from purrr example) - avoids direct dependency if only used here
# `%||%` <- function(x, y) {
#   if (is.null(x) || length(x) == 0) y else x
# } 
