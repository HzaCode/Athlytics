# R/plot_decoupling.R

#' Plot Aerobic Decoupling Trend
#'
#' Visualizes the trend of aerobic decoupling over time.
#'
#' Plots the aerobic decoupling trend over time. Uses pre-calculated data
#' or calls `calculate_decoupling` (can be slow).
#'
#' @param stoken A valid Strava token from `rStrava::strava_oauth()`. Required unless `decoupling_df` is provided.
#' @param activity_type Type(s) of activities to analyze (e.g., "Run", "Ride").
#' @param decouple_metric Metric basis: "Pace_HR" or "Power_HR".
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to ~1 year ago.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#' @param min_duration_mins Minimum activity duration (minutes) to include. Default 45.
#' @param max_activities Max number of recent activities to fetch/analyze. Default 50.
#' @param add_trend_line Add a smoothed trend line (`geom_smooth`)? Default `TRUE`.
#' @param smoothing_method Smoothing method for trend line (e.g., "loess", "lm"). Default "loess".
#' @param decoupling_df Optional. A pre-calculated data frame from `calculate_decoupling`.
#'   If provided, `stoken` and other calculation parameters are ignored.
#'
#' @return A ggplot object showing the decoupling trend.
#'
#' @details Plots decoupling percentage ((EF_1st_half - EF_2nd_half) / EF_1st_half * 100).
#'   Positive values mean HR drifted relative to output. A 5% threshold line is often
#'   used as reference. If `decoupling_df` is not provided, calls `calculate_decoupling` first
#'   (can be slow and hit API limits).
#'
#' @importFrom rStrava get_activity_list get_activity_streams
#' @importFrom dplyr filter select mutate arrange %>% rename left_join case_when group_by summarise pull first last tibble
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal scale_x_date theme element_text scale_y_continuous annotate geom_hline
#' @importFrom rlang .data
#' @importFrom stats median na.omit
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires authentication first:
#' # stoken <- rStrava::strava_oauth(..., cache = TRUE)
#'
#' # Plot Pace/HR decoupling for recent Runs (limit to 30)
#' plot_decoupling(stoken = stoken,
#'                 activity_type = "Run",
#'                 decouple_metric = "Pace_HR",
#'                 max_activities = 30)
#'
#' # Plot Power/HR decoupling for recent Rides
#' plot_decoupling(stoken = stoken,
#'                 activity_type = "Ride",
#'                 decouple_metric = "Power_HR")
#'
#' # Plot pre-calculated decoupling data
#' # decoupling_results <- calculate_decoupling(...)
#' # plot_decoupling(decoupling_df = decoupling_results)
#' }
plot_decoupling <- function(stoken,
                            activity_type = c("Run", "Ride"),
                            decouple_metric = c("Pace_HR", "Power_HR"),
                            start_date = NULL,
                            end_date = NULL,
                            min_duration_mins = 45,
                            max_activities = 50,
                            add_trend_line = TRUE,
                            smoothing_method = "loess",
                            decoupling_df = NULL) {

  # Match arg here for labels, needs to be done before data check
  decouple_metric_label <- match.arg(decouple_metric)

  # --- Get Data --- 
  # If decoupling_df is not provided, calculate it
  if (is.null(decoupling_df)) {
      # Ensure stoken is provided if decoupling_df is not
      if (missing(stoken)) stop("Either 'stoken' or 'decoupling_df' must be provided.")
      
      # Call the calculation function
      decoupling_df <- calculate_decoupling(
          stoken = stoken,
          activity_type = activity_type,
          # Pass the matched argument
          decouple_metric = decouple_metric_label, 
          start_date = start_date,
          end_date = end_date,
          min_duration_mins = min_duration_mins,
          max_activities = max_activities
      )
  }

  # Check if decoupling_df is empty or invalid
  if (!is.data.frame(decoupling_df) || nrow(decoupling_df) == 0 || !all(c("date", "decoupling") %in% names(decoupling_df))) {
      warning("No valid decoupling data available to plot (or missing 'date'/'decoupling' columns).")
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No decoupling data available"))
  }

  # Rename for clarity in plot code
  plot_data <- decoupling_df
  
  # --- Plotting ---
  message("Generating plot...")
  y_label <- paste("Decoupling (", gsub("_", "/", decouple_metric_label), ") [%]")

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$decoupling)) +
    ggplot2::geom_point(alpha = 0.8, size = 2.5, color = "dodgerblue") +
    ggplot2::geom_hline(yintercept = 5, linetype="dashed", color="grey70") + # Often cited threshold
    ggplot2::annotate("text", x = min(plot_data$date), y = 5.5, label="5% threshold", hjust=0, vjust=0, size=3, color="grey50") +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::labs(
      title = "Aerobic Decoupling Trend",
      subtitle = paste("Metric:", decouple_metric_label, "(Lower is generally better)"),
      x = "Date",
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   plot.title = ggplot2::element_text(face = "bold"))

  if (add_trend_line) {
    p <- p + ggplot2::geom_smooth(method = smoothing_method, se = FALSE, color = "firebrick", linewidth = 0.8)
  }

  return(p)
} 