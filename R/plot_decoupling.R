# R/plot_decoupling.R

# Helper function for explicit English month-year labels, avoiding locale issues
explicit_english_month_year <- function(date_obj) {
  # Handle NA inputs: return NA_character_ for each NA in date_obj
  if (all(is.na(date_obj))) { # If all are NA (covers single NA and vector of all NAs)
    return(rep(NA_character_, length(date_obj)))
  }

  m <- lubridate::month(date_obj)
  y <- lubridate::year(date_obj)
  eng_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # Initial paste might produce "NA NA" for NA dates
  formatted_dates <- paste(eng_months[m], y)
  
  # Correctly set NA_character_ for positions where original date_obj was NA
  formatted_dates[is.na(date_obj)] <- NA_character_
  
  return(formatted_dates)
}

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
#' @param max_activities Max number of recent activities to fetch/analyze when `stoken` is used. Default 50.
#' @param add_trend_line Add a smoothed trend line (`geom_smooth`)? Default `TRUE`.
#' @param smoothing_method Smoothing method for trend line (e.g., "loess", "lm"). Default "loess".
#' @param decoupling_df Optional. A pre-calculated data frame from `calculate_decoupling`.
#'   If provided, `stoken` and other calculation parameters are ignored.
#'   Must contain 'date' and 'decoupling' columns.
#'
#' @return A ggplot object showing the decoupling trend.
#'
#' @details Plots decoupling percentage ((EF_1st_half - EF_2nd_half) / EF_1st_half * 100).
#'   Positive values mean HR drifted relative to output. A 5% threshold line is often
#'   used as reference. If `decoupling_df` is not provided, calls `calculate_decoupling` first
#'   (can be slow and hit API limits).
#'
#' 
#' @importFrom dplyr filter select mutate arrange %>% rename left_join case_when group_by summarise pull first last tibble
#' @importFrom lubridate as_date
#' @importFrom lubridate date
#' @importFrom lubridate days
#' @importFrom lubridate ymd
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate as_datetime
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal scale_x_date theme element_text scale_y_continuous annotate geom_hline theme_void ggtitle
#' @importFrom rlang .data
#' @importFrom stats median na.omit
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(athlytics_sample_data)
#' # Explicitly name decoupling_df and provide activity_type
#' if (!is.null(athlytics_sample_decoupling) && nrow(athlytics_sample_decoupling) > 0) {
#'   p <- plot_decoupling(decoupling_df = athlytics_sample_decoupling, activity_type = "Run")
#'   print(p)
#' }
#'
#' \dontrun{
#' # Example using real data (requires authentication)
#' # NOTE: The following rStrava::strava_oauth call is a placeholder.
#' # You MUST replace placeholders with your actual Strava API credentials.
#' tryCatch({
#'   stoken_example <- rStrava::strava_oauth(
#'     app_name = "YOUR_APP_NAME_PLACEHOLDER",
#'     app_client_id = "YOUR_CLIENT_ID_PLACEHOLDER",     # CORRECTED
#'     app_secret = "YOUR_CLIENT_SECRET_PLACEHOLDER",  # CORRECTED
#'     cache = TRUE,
#'     app_scope = "activity:read_all" # Recommended scope
#'   )
#'
#'   if (inherits(stoken_example, "Token2.0")) {
#'     message("Placeholder stoken_example created for athlytics examples.")
#'
#'     # Example 1: Plot Decoupling trend for Runs (last 6 months)
#'     # This first calculates the data, then plots it.
#'     message("Calculating decoupling for Runs (last 6 months) - may take a moment...")
#'     decoupling_runs_6mo <- tryCatch({
#'         calculate_decoupling(
#'             stoken = stoken_example,
#'             activity_type = "Run",
#'             decouple_metric = "Pace_HR",
#'             date_range = c(format(Sys.Date() - lubridate::months(6), "%Y-%m-%d"), 
#'                            format(Sys.Date(), "%Y-%m-%d")),
#'             max_activities = 5 # Keep low for example
#'         )
#'     }, error = function(e_calc) {
#'         message(paste("Could not calculate decoupling data in example:", e_calc$message))
#'         return(dplyr::tibble()) # Return empty tibble on error
#'     })
#'
#'     if (nrow(decoupling_runs_6mo) > 0 && "decoupling" %in% names(decoupling_runs_6mo)) {
#'       p_runs_6mo <- plot_decoupling(decoupling_df = decoupling_runs_6mo, activity_type = "Run")
#'       print(p_runs_6mo)
#'     } else {
#'       message("No decoupling data for Runs (last 6 months) to plot, or calculation failed.")
#'     }
#'
#'     # Example 2: Plot Decoupling trend for Rides
#'     # decoupling_rides <- calculate_decoupling(
#'     #   stoken = stoken_example,
#'     #   activity_type = "Ride",
#'     #   decouple_metric = "Power_HR",
#'     #   max_activities = 5
#'     # )
#'     # if (nrow(decoupling_rides) > 0 && "decoupling" %in% names(decoupling_rides)) {
#'     #   p_rides <- plot_decoupling(decoupling_df = decoupling_rides, activity_type = "Ride")
#'     #   print(p_rides)
#'     # } else {
#'     #   message("No decoupling data for Rides to plot, or calculation failed.")
#'     # }
#'
#'     # Example 3: Plot Decoupling trend for multiple Run types (no trend line)
#'     # decoupling_multi_run <- calculate_decoupling(
#'     #   stoken = stoken_example,
#'     #   activity_type = c("Run", "VirtualRun"),
#'     #   decouple_metric = "Pace_HR",
#'     #   max_activities = 5
#'     # )
#'     # if (nrow(decoupling_multi_run) > 0 && "decoupling" %in% names(decoupling_multi_run)) {
#'     #   p_multi_run <- plot_decoupling(
#'     #     decoupling_df = decoupling_multi_run,
#'     #     activity_type = c("Run", "VirtualRun"),
#'     #     add_trend_line = FALSE
#'     #   )
#'     #   print(p_multi_run)
#'     # } else {
#'     #   message("No decoupling data for multi-run types to plot, or calculation failed.")
#'     # }
#'
#'   } else {
#'     message("Failed to create placeholder stoken for plot_decoupling examples.")
#'   }
#' }, error = function(e_auth) {
#'   message(paste("Error during rStrava authentication in example:", e_auth$message))
#' })
#' }
plot_decoupling <- function(stoken,
                            activity_type = c("Run", "Ride"), # Default to both if not specified
                            decouple_metric = c("Pace_HR", "Power_HR"),
                            start_date = NULL,
                            end_date = NULL,
                            min_duration_mins = 45,
                            max_activities = 50,
                            add_trend_line = TRUE,
                            smoothing_method = "loess",
                            decoupling_df = NULL,
                            group_var = NULL,
                            group_colors = NULL) {

  # Match arg for decouple_metric to ensure only one is used internally if multiple are provided
  decouple_metric_label <- match.arg(decouple_metric)

  # --- Check if first argument is already decoupling data frame ---
  if (is.data.frame(stoken) && any(c("decoupling_pct", "decoupling_percent", "decoupling") %in% colnames(stoken))) {
    decoupling_df <- stoken
  }

  # --- Get Data --- 
  # If decoupling_df is not provided, calculate it
  if (is.null(decoupling_df)) {
      # Check if stoken provided when decoupling_df is not
      if (missing(stoken)) stop("Either provide decoupling data frame from calculate_decoupling() as first argument, or provide activities_data.")
      
      message("No pre-calculated decoupling_df provided. Calculating data now... (This may take a while)")
      # Call the calculation function
      decoupling_df <- calculate_decoupling(
          activities_data = stoken,
          activity_type = activity_type, # Can be a vector
          start_date = start_date,
          end_date = end_date,
          min_duration_mins = min_duration_mins
      )
  }

  # Check if decoupling_df is empty or invalid after potential calculation
  if (!is.data.frame(decoupling_df) || nrow(decoupling_df) == 0 || !all(c("date", "decoupling") %in% names(decoupling_df))) {
      warning("No valid decoupling data available to plot (or missing 'date'/'decoupling' columns).")
      # Return a blank plot with a message
      return(
          ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::ggtitle("No decoupling data to plot") +
          ggplot2::labs(subtitle = "Please check input data or calculation parameters.")
      )
  }

  # Convert date to Date type
  plot_data <- decoupling_df %>%
    dplyr::mutate(date = lubridate::as_date(.data$date))
  
  # --- Check for group variable ---
  has_groups <- !is.null(group_var) && group_var %in% colnames(plot_data)
  
  # --- Plotting ---
  message("Generating plot...")
  y_axis_label <- paste("Decoupling (", gsub("_", "/", decouple_metric_label), ") [%]")
  
  # Determine activity type label for the plot title/subtitle
  plot_activity_type_label <- if (length(activity_type) == 1) {
      activity_type
  } else if ("activity_type" %in% names(plot_data) && length(unique(plot_data$activity_type)) == 1) {
      unique(plot_data$activity_type)
  } else {
      "Selected Activities" # Generic if multiple or not clearly defined
  }

  if (has_groups) {
    # Multi-group plotting
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$decoupling, 
                                                  color = .data[[group_var]])) +
      ggplot2::geom_point(alpha = 0.7, size = 2.5) +
      ggplot2::geom_hline(yintercept = 5, linetype="dashed", color="grey70") +
      ggplot2::annotate("text", x = min(plot_data$date, na.rm = TRUE), y = 5.5, 
                       label="5% threshold", hjust=0, vjust=0, size=3, color="grey50") +
      ggplot2::scale_x_date(labels = explicit_english_month_year, date_breaks = "3 months") +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))
    
    # Apply custom colors if provided
    if (!is.null(group_colors)) {
      p <- p + ggplot2::scale_color_manual(values = group_colors, name = group_var)
    } else {
      p <- p + ggplot2::scale_color_manual(
        values = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4"),
        name = group_var
      )
    }
    
    p <- p +
      ggplot2::labs(
        title = paste("Aerobic Decoupling Trend for", plot_activity_type_label),
        subtitle = paste("Metric:", decouple_metric_label, "| Grouped by:", group_var, "(Lower is better)"),
        x = "Date",
        y = y_axis_label
      )
    
    if (add_trend_line && nrow(plot_data) >= 2) {
      p <- p + ggplot2::geom_smooth(ggplot2::aes(group = .data[[group_var]]), 
                                    method = smoothing_method, se = FALSE, linewidth = 0.8)
    }
    
  } else {
    # Single group plotting (original logic)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$decoupling)) +
      ggplot2::geom_point(alpha = 0.8, size = 2.5, color = "dodgerblue") +
      ggplot2::geom_hline(yintercept = 5, linetype="dashed", color="grey70") +
      ggplot2::annotate("text", x = min(plot_data$date, na.rm = TRUE), y = 5.5, 
                       label="5% threshold", hjust=0, vjust=0, size=3, color="grey50") +
      ggplot2::scale_x_date(labels = explicit_english_month_year, date_breaks = "3 months") +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
      ggplot2::labs(
        title = paste("Aerobic Decoupling Trend for", plot_activity_type_label),
        subtitle = paste("Metric:", decouple_metric_label, "(Lower is generally better)"),
        x = "Date",
        y = y_axis_label
      )
    
    if (add_trend_line && nrow(plot_data) >= 2) {
      p <- p + ggplot2::geom_smooth(method = smoothing_method, se = FALSE, color = "firebrick", linewidth = 0.8)
    }
  }
  
  p <- p +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 11, face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 14, margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40", margin = ggplot2::margin(b = 15)),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = if(has_groups) "bottom" else "none",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9)
    )

  return(p)
} 

# Helper for date labels if not available globally (e.g. from another file)
# This function might be needed if `english_month_year` was used previously and is not defined
# For simplicity, I've replaced `english_month_year` with `scales::label_date_short()`
# If you need a specific "Month Year" format, you can define:
# english_month_year <- function(x) format(x, "%b %Y")
# And ensure `scales` is in Imports if using its label functions.
# Since ggplot2 is imported, scales is usually available as a dependency.
