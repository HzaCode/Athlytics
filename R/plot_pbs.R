#' Plot Personal Best (PB) Trends
#'
#' Visualizes the trend of personal best times for specific running distances.
#'
#' Plots the trend of best efforts for specified distances, highlighting new PBs.
#' **Recommended workflow: Use local data via `pbs_df`.**
#'
#' @param data **Recommended: Pass pre-calculated data via `pbs_df` (local export preferred).**
#'   A data frame from `calculate_pbs()` or activities data from `load_local_activities()`.
#' @param activity_type Type(s) of activities to search (e.g., "Run"). Default "Run".
#' @param distance_meters Numeric vector of distances (meters) to plot PBs for (e.g., `c(1000, 5000)`).
#'   Relies on Strava's `best_efforts` data.
#' @param max_activities Max number of recent activities to check. Default 500. Reduce for speed.
#' @param date_range Optional. Filter activities by date `c("YYYY-MM-DD", "YYYY-MM-DD")`.
#' @param add_trend_line Logical. Whether to add a trend line to the plot. Default TRUE.
#' @param pbs_df **Recommended.** A pre-calculated data frame from `calculate_pbs()`.
#'   When provided, analysis uses local data only (no API calls).
#'
#' @return A ggplot object showing PB trends, faceted by distance if multiple are plotted.
#'
#' @details Visualizes data from `calculate_pbs`. Points show best efforts;
#'   solid points mark new PBs. Y-axis is MM:SS.
#'   **Best practice: Use `load_local_activities()` + `calculate_pbs()` + this function.**
#'   Legacy API mode is maintained for backward compatibility only.
#'
#' @importFrom dplyr filter select mutate arrange group_by slice bind_rows summarise distinct rename %>% left_join
#' @importFrom purrr map_dfr map_chr possibly quietly
#' @importFrom tidyr unnest pivot_longer
#' @importFrom lubridate as_datetime ymd_hms seconds_to_period parse_date_time
#' @importFrom scales pretty_breaks
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example using the built-in sample data
#' # This data now contains a simulated history of performance improvements
#' data("athlytics_sample_pbs", package = "Athlytics")
#' 
#' if (!is.null(athlytics_sample_pbs) && nrow(athlytics_sample_pbs) > 0) {
#'   # Plot PBs using the package sample data directly
#'   p <- plot_pbs(pbs_df = athlytics_sample_pbs, activity_type = "Run")
#'   print(p)
#' }
#' 
#' if (FALSE) {
#' # Example using local Strava export data
#' activities <- load_local_activities("strava_export_data/activities.csv")
#' 
#' # Plot PBS trend for Runs (last 6 months)
#' pb_data_run <- calculate_pbs(activities_data = activities, 
#'                              activity_type = "Run", 
#'                              distance_meters = c(1000,5000,10000), 
#'                              date_range = c(format(Sys.Date() - months(6)),
#'                                           format(Sys.Date())))
#' if(nrow(pb_data_run) > 0) {
#'   plot_pbs(pbs_df = pb_data_run, distance_meters = c(1000,5000,10000))
#' }
#' 
#' # Plot PBS trend for Rides (if applicable, though PBs are mainly for Runs)
#' pb_data_ride <- calculate_pbs(activities_data = activities, 
#'                                activity_type = "Ride", 
#'                                distance_meters = c(10000, 20000))
#' if(nrow(pb_data_ride) > 0) {
#'    plot_pbs(pbs_df = pb_data_ride, distance_meters = c(10000, 20000))
#' }
#' 
#' # Plot PBS trend for multiple Run types (no trend line)
#' pb_data_multi <- calculate_pbs(activities_data = activities, 
#'                                activity_type = c("Run", "VirtualRun"), 
#'                                distance_meters = c(1000,5000))
#' if(nrow(pb_data_multi) > 0) {
#'   plot_pbs(pbs_df = pb_data_multi, distance_meters = c(1000,5000), 
#'            add_trend_line = FALSE)
#' }
#' }
#'
#' @importFrom dplyr filter select mutate arrange group_by slice bind_rows summarise distinct rename %>% left_join
#' @importFrom purrr map_dfr map_chr possibly quietly
#' @importFrom tidyr unnest pivot_longer
#' @importFrom lubridate as_datetime ymd_hms seconds_to_period parse_date_time
#' @importFrom scales pretty_breaks
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#'
plot_pbs <- function(data,
                     activity_type = "Run",
                     distance_meters,
                     max_activities = 500,
                     date_range = NULL,
                     add_trend_line = TRUE,
                     pbs_df = NULL) {

  # --- Get Data ---
  if (is.null(pbs_df)) {
      if (missing(data)) stop("Either 'data' or 'pbs_df' must be provided.")
      if (missing(distance_meters)) stop("`distance_meters` must be provided when `pbs_df` is not.")
      
      pbs_df <- calculate_pbs(
          activities_data = data,
          activity_type = activity_type,
          start_date = if (!is.null(date_range)) date_range[1] else NULL,
          end_date = if (!is.null(date_range)) date_range[2] else NULL,
          distances_m = distance_meters
      )
  }
  
  if (!is.data.frame(pbs_df) || nrow(pbs_df) == 0) {
      warning("No PB data available to plot.")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No PB data available")) 
  }
  
  # Ensure distance_meters used for filtering/plotting are derived from pbs_df if it was passed directly
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
  
  # Ensure activity_date is Date type
  pbs_df$activity_date <- as.Date(pbs_df$activity_date)
  
  # Create distance_label if it doesn't exist
  if (!"distance_label" %in% names(pbs_df)) {
    pbs_df$distance_label <- dplyr::case_when(
      pbs_df$distance == 1000 ~ "1k",
      pbs_df$distance == 5000 ~ "5k",
      pbs_df$distance == 10000 ~ "10k",
      pbs_df$distance == 21097.5 ~ "Half Marathon",
      pbs_df$distance == 42195 ~ "Marathon",
      TRUE ~ paste0(round(pbs_df$distance), "m")
    )
    pbs_df$distance_label <- factor(pbs_df$distance_label, 
                                    levels = c("1k", "5k", "10k", "Half Marathon", "Marathon"))
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
  p <- ggplot2::ggplot(pbs_df, ggplot2::aes(x = .data$activity_date, y = .data$time_seconds, 
                                             color = .data$distance_label, group = .data$distance_label)) +
    ggplot2::geom_line(linewidth = 1.8, alpha = 0.85) + 
    ggplot2::geom_point(ggplot2::aes(shape = .data$is_pb), size = 4.5, alpha = 0.95, stroke = 1.2) + 
    ggplot2::scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 21), 
                       name = "Personal Best", labels = c("TRUE" = "Yes", "FALSE" = "No")) +
    ggplot2::scale_x_date(
      date_breaks = "2 months", 
      labels = function(x) {
        months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        paste(months[as.integer(format(x, "%m"))], format(x, "%Y"))
      }
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) sprintf('%02d:%02d', floor(x/60), x %% 60),
      breaks = scales::pretty_breaks(n = 2)
    ) + 
    ggplot2::scale_color_manual(values = athlytics_palette_vibrant(), name = "Distance") + 
    ggplot2::labs(
      title = "Personal Best Running Times Trend",
      subtitle = "Showing best efforts for specified distances over time",
      x = "Activity Date",
      y = "Best Time (MM:SS)",
      caption = "Data from local Strava export"
    ) +
    theme_athlytics() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 4)))

  # Add trend line if requested (though typically not used for PB plots)
  if (add_trend_line) {
    # Use linear model (lm) instead of loess to avoid errors with sparse data
    # and to better represent the overall improvement trend
    p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, aes(group = .data$distance_label), 
                                   linewidth = 1.0, alpha = 0.15, linetype = "dashed")
  }

  if (length(unique(pbs_df$distance_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ .data$distance_label, scales = "free_y", ncol = 1) +
      ggplot2::theme(
        panel.spacing = ggplot2::unit(2, "lines")
      )
  }

  return(p)
}

# Helper for null default (from purrr example) - avoids direct dependency if only used here