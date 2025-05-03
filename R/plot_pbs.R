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
#' @param pbs_df Optional. A pre-calculated data frame from `calculate_pbs`.
#'   If provided, `stoken` and other calculation parameters are ignored.
#'
#' @return A ggplot object showing PB trends, faceted by distance if multiple are plotted.
#'
#' @details Visualizes data from `calculate_pbs`. Points show best efforts;
#'   solid points mark new PBs. Y-axis is MM:SS.
#'   If `pbs_df` is not provided, calls `calculate_pbs` first (can be slow).
#'
#' @importFrom rStrava get_activity_list get_activity
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
#' \dontrun{
#' # Requires authentication first:
#' # stoken <- rStrava::strava_oauth(..., cache = TRUE)
#'
#' # Plot PBs for 1k, 5k, 10k
#' plot_pbs(stoken = stoken, distance_meters = c(1000, 5000, 10000))
#'
#' # Plot PBs for Mile and Half Marathon for 2023
#' plot_pbs(stoken = stoken,
#'          distance_meters = c(1609, 21097),
#'          date_range = c("2023-01-01", "2023-12-31"))
#'
#' # Plot pre-calculated PB data
#' # pb_results <- calculate_pbs(...)
#' # plot_pbs(pbs_df = pb_results)
#' }
plot_pbs <- function(stoken,
                     activity_type = "Run",
                     distance_meters,
                     max_activities = 500,
                     date_range = NULL,
                     pbs_df = NULL) {

  # --- Get Data ---
  if (is.null(pbs_df)) {
      if (missing(stoken)) stop("Either 'stoken' or 'pbs_df' must be provided.")
      
      pbs_df <- calculate_pbs(
          stoken = stoken,
          activity_type = activity_type,
          distance_meters = distance_meters,
          max_activities = max_activities,
          date_range = date_range
      )
  }
  
  if (!is.data.frame(pbs_df) || nrow(pbs_df) == 0) {
      warning("No PB data available to plot.")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No PB data available")) 
  }

  # --- 6. Plotting ---
  message("Generating plot...")

  # Create the base plot
  p <- ggplot2::ggplot(pbs_df, ggplot2::aes(x = .data$activity_date, y = .data$time_seconds, color = .data$distance_label)) +
    ggplot2::geom_line(alpha = 0.5) + # Line connecting all efforts for context
    ggplot2::geom_point(ggplot2::aes(shape = .data$is_pb), size = 2.5) + # Points for all efforts, shape indicates PB
    ggplot2::scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1), # Solid circle for PB, open for others
                       name = "Personal Best", labels = c("TRUE" = "Yes", "FALSE" = "No")) +
    ggplot2::scale_y_continuous(labels = function(x) sprintf('%02d:%02d', floor(x/60), floor(x) %% 60)) + # Format y-axis as MM:SS
    viridis::scale_color_viridis(discrete = TRUE, option = "C", name = "Distance") + # Color by distance
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
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 3))) # Make legend shape clearer

   # Add PB markers specifically (optional, points already do this with shape)
   # p <- p + geom_point(data = filter(pbs_df, is_pb), aes(size = 2), shape = 19, show.legend = FALSE) # Emphasize PBs

  # Add facetting if multiple distances are plotted for clarity
  if (length(unique(pbs_df$distance_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ .data$distance_label, scales = "free_y", ncol = 1) +
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }

  return(p)
}

# Helper for null default (from purrr example) - avoids direct dependency if only used here