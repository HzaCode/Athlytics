#' Plot Personal Best (PB) Trends
#'
#' Visualizes the trend of personal best times for specific running distances.
#'
#' Plots the trend of best efforts for specified distances, highlighting new PBs.
#' Uses pre-calculated data or calls `calculate_pbs`.
#'
#' @param stoken A valid Strava token from `rStrava::strava_oauth()`. Required unless `pbs_df` is provided.
#' @param activity_type Type(s) of activities to search (e.g., "Run"). Default "Run".
#' @param distance_meters Numeric vector of distances (meters) to plot PBs for (e.g., `c(1000, 5000)`).
#'   Relies on Strava's `best_efforts` data.
#' @param max_activities Max number of recent activities to check. Default 500. Reduce for speed.
#' @param date_range Optional. Filter activities by date `c("YYYY-MM-DD", "YYYY-MM-DD")`.
#' @param add_trend_line Logical. Whether to add a trend line to the plot. Default TRUE.
#' @param pbs_df Optional. A pre-calculated data frame from `calculate_pbs`.
#'   If provided, `stoken` and other calculation parameters are ignored.
#'
#' @return A ggplot object showing PB trends, faceted by distance if multiple are plotted.
#'
#' @details Visualizes data from `calculate_pbs`. Points show best efforts;
#'   solid points mark new PBs. Y-axis is MM:SS.
#'   If `pbs_df` is not provided, calls `calculate_pbs` first (can be slow).
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
#' # Example using simulated data
#' data(athlytics_sample_data)
#' # athlytics_sample_pbs should contain the PBs to be plotted
#' if (!is.null(athlytics_sample_pbs) && nrow(athlytics_sample_pbs) > 0) {
#'   sample_pbs_for_plot <- athlytics_sample_pbs
#'   
  #'   # Note: date column should be named 'activity_date' and be of Date type
#'   if ("date" %in% names(sample_pbs_for_plot) && !"activity_date" %in% names(sample_pbs_for_plot)) {
#'     names(sample_pbs_for_plot)[names(sample_pbs_for_plot) == "date"] <- "activity_date"
#'   }
#'   if ("activity_date" %in% names(sample_pbs_for_plot)) {
#'     sample_pbs_for_plot$activity_date <- as.Date(sample_pbs_for_plot$activity_date)
#'   } else {
#'     message("Relevant date column not found in sample PBs for example.")
#'   }
#'   
#'   # plot_pbs requires distance_meters. Extract from sample data.
#'   req_dist_meters <- NULL
#'   if ("distance" %in% names(sample_pbs_for_plot)) {
#'     req_dist_meters <- unique(sample_pbs_for_plot$distance)
#'   } else if ("distance_target_m" %in% names(sample_pbs_for_plot)) {
#'     req_dist_meters <- unique(sample_pbs_for_plot$distance_target_m)
#'   }
#'   
#'   can_plot <- "activity_date" %in% names(sample_pbs_for_plot) && 
#'               !is.null(req_dist_meters) && length(req_dist_meters) > 0
#'
#'   if (can_plot) {
#'     p <- plot_pbs(pbs_df = sample_pbs_for_plot, activity_type = "Run", 
#'                   distance_meters = req_dist_meters)
#'     print(p)
#'   } else {
#'     message("Sample PBs data lacks required date or distance info for example.")
#'   }
#' } else {
#'   message("athlytics_sample_pbs is empty or not found, skipping example plot.")
#' }
#'
#' \dontrun{
#' # Example using real data (requires authentication)
#' # Users should first authenticate and obtain a stoken, e.g.:
#' # To authenticate (replace with your details):
#' # stoken <- rStrava::strava_oauth(app_name = "YOUR_APP",
#' #                                client_id = "YOUR_ID",
#' #                                client_secret = "YOUR_SECRET",
#' #                                cache = TRUE)
#'
#' # Plot PBS trend for Runs (last 6 months)
#' # Note: plot_pbs requires distance_meters. 
#' # This example assumes you want to see all available from calculate_pbs.
#' # For a specific plot, ensure calculate_pbs was run for those distances
#' # or specify them here.
#' # pb_data_run <- calculate_pbs(stoken = stoken, activity_type = "Run", 
#' #                              distance_meters = c(1000,5000,10000), 
#' #                              date_range = c(format(Sys.Date() - months(6)),
#' #                                           format(Sys.Date())))
#' # if(nrow(pb_data_run) > 0) {
#' #   plot_pbs(pbs_df = pb_data_run, distance_meters = c(1000,5000,10000))
#' # }
#'
#' # Plot PBS trend for Rides (if applicable, though PBs are mainly for Runs)
  #' # Note: distance_meters should be relevant for Ride PBs if calculate_pbs handles them.
#' # pb_data_ride <- calculate_pbs(stoken = stoken, activity_type = "Ride", 
#' #                                distance_meters = c(10000, 20000))
#' # if(nrow(pb_data_ride) > 0) {
#' #    plot_pbs(pbs_df = pb_data_ride, distance_meters = c(10000, 20000))
#' # }
#'
#' # Plot PBS trend for multiple Run types (no trend line)
  #' # Note: distance_meters should be specified
#' # pb_data_multi <- calculate_pbs(stoken = stoken, 
#' #                                activity_type = c("Run", "VirtualRun"), 
#' #                                distance_meters = c(1000,5000))
#' # if(nrow(pb_data_multi) > 0) {
#' #   plot_pbs(pbs_df = pb_data_multi, distance_meters = c(1000,5000), 
#' #            add_trend_line = FALSE)
#' # }
#' }

plot_pbs <- function(stoken,
                     activity_type = "Run",
                     distance_meters,
                     max_activities = 500,
                     date_range = NULL,
                     add_trend_line = TRUE,
                     pbs_df = NULL) {

  # --- Check if first argument is already PBS data frame ---
  if (is.data.frame(stoken) && any(c("pb_improvement_pct", "distance", "time") %in% colnames(stoken))) {
    pbs_df <- stoken
  }

  # --- Get Data ---
  if (is.null(pbs_df)) {
      if (missing(stoken)) stop("Either provide PBS data frame from calculate_pbs() as first argument, or provide activities_data.")
      
      # Only require distance_meters if we need to calculate
      if (missing(distance_meters)) {
        distance_meters <- c(1000, 5000, 10000)  # Use defaults
        message("Using default distances: 1km, 5km, 10km")
      }
      
      pbs_df <- calculate_pbs(
          activities_data = stoken,
          activity_type = activity_type,
          distances_m = distance_meters,
          start_date = if (!is.null(date_range) && length(date_range) >= 1) date_range[1] else NULL,
          end_date = if (!is.null(date_range) && length(date_range) >= 2) date_range[2] else NULL
      )
  }
  
  if (!is.data.frame(pbs_df) || nrow(pbs_df) == 0) {
      warning("No PB data available to plot.")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No PB data available")) 
  }
  
  # Get distance_meters from pbs_df if passed directly
  # Or ensure they are consistent if pbs_df was calculated
  if(!missing(distance_meters) && !is.null(pbs_df)){
    pbs_df <- pbs_df[pbs_df$distance %in% distance_meters,]
    if(nrow(pbs_df) == 0){
      warning("pbs_df does not contain data for the specified distance_meters after filtering.")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No PB data for specified distances"))
    }
  } else if (is.null(pbs_df) && missing(distance_meters)){
     stop("If pbs_df is not provided, distance_meters must be specified for calculate_pbs call.")
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
