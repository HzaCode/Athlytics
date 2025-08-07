# R/calculate_decoupling.R

#' Calculate Aerobic Decoupling
#'
#' Calculates aerobic decoupling for Strava activities.
#'
#' Calculates aerobic decoupling (HR drift relative to pace/power) using detailed
#' Strava activity streams. Fetching streams via API can be slow.
#'
#' @param stoken A valid Strava token from `rStrava::strava_oauth()`.
#' @param activity_type Type(s) of activities to analyze (e.g., "Run", "Ride").
#' @param decouple_metric Basis for calculation: "Pace_HR" or "Power_HR".
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to one year ago.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#' @param min_duration_mins Minimum activity duration (minutes) to include. Default 45.
#' @param max_activities Maximum number of recent activities to analyze. Default 50.
#' @param stream_df Optional. A pre-fetched data frame for a *single* activity's stream.
#'   If provided, calculates decoupling for this data directly, ignoring `stoken` and
#'   other fetching parameters. Must include columns: `time`, `heartrate`, and either
#'   `velocity_smooth`/`distance` (for Pace_HR) or `watts` (for Power_HR).
#'
#' @return Returns a data frame with `date` and `decoupling` [%] columns, OR
#'   a single numeric decoupling value if `stream_df` is provided.
#'
#' @details Provides data for `plot_decoupling`. Compares output/HR efficiency
#'   between first and second halves of activities. Positive values indicate
#'   HR drift. Fetching streams via API using `httr` is slow and subject to rate limits.
#'
#' @importFrom rStrava get_activity_list get_activity_streams
#' @importFrom dplyr filter select mutate arrange %>% rename left_join case_when group_by summarise pull first last tibble slice_head lead lag
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom stats median na.omit
#' @importFrom rlang .data %||%
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom httr GET add_headers content stop_for_status
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(athlytics_sample_data)
#' print(head(athlytics_sample_decoupling))
#'
#' \dontrun{
#' # Example using real data (requires authentication)
#' # To authenticate (replace with your details):
#' # stoken <- rStrava::strava_oauth(app_name = "YOUR_APP",
#' #                                client_id = "YOUR_ID",
#' #                                client_secret = "YOUR_SECRET",
#' #                                cache = TRUE)
#'
#' # Calculate Pace/HR decoupling for recent Runs (limit to 10 activities for speed)
  #' # Note: stoken should be defined and valid
#' # run_decoupling <- calculate_decoupling(
#' #     stoken = stoken,
#' #     activity_type = "Run",
#' #     decouple_metric = "Pace_HR",
#' #     max_activities = 10
#' # )
#' # print(tail(run_decoupling))
#' }
calculate_decoupling <- function(stoken,
                                 activity_type = c("Run", "Ride"),
                                 decouple_metric = c("Pace_HR", "Power_HR"),
                                 start_date = NULL,
                                 end_date = NULL,
                                 min_duration_mins = 45,
                                 max_activities = 50,
                                 stream_df = NULL) {
  # --- Input Validation ---
  decouple_metric <- match.arg(decouple_metric)
  if (is.null(stream_df)) {
    # Only validate stoken and other params if not using stream_df
    if (!inherits(stoken, "Token2.0")) {
      stop("`stoken` must be a valid Token2.0 object from rStrava::strava_oauth().")
    }
    if (!is.numeric(min_duration_mins) || min_duration_mins <= 0) stop("`min_duration_mins` must be a positive number.")
    if (!is.numeric(max_activities) || max_activities <= 0) stop("`max_activities` must be a positive integer.")
  } else {
      # Validate stream_df structure if provided
      required_cols <- c("time", "heartrate")
      metric_col <- if(decouple_metric == "Pace_HR") c("velocity_smooth", "distance") else "watts"
      required_cols <- c(required_cols, metric_col)
      # Need at least one of the speed/dist/power columns
      if (!is.data.frame(stream_df) || !("time" %in% names(stream_df) && "heartrate" %in% names(stream_df)) || 
          !any(metric_col %in% names(stream_df))) {
          stop("Provided `stream_df` is invalid or missing required columns (time, heartrate, and velocity_smooth/distance or watts).")
      }
      # Check for enough rows
      if (nrow(stream_df) < 10) stop("Provided `stream_df` has too few rows (< 10) to calculate decoupling.")
  }

  # --- Calculate Decoupling Directly if stream_df is provided ---
  if (!is.null(stream_df)) {
    message("Calculating decoupling from provided stream_df...")
    
    # Clean NAs
    stream_df <- stats::na.omit(stream_df)
    if(nrow(stream_df) < 10) stop("Not enough valid data points in stream_df after removing NAs.")
    
    # Calculate speed if missing
    if(decouple_metric == "Pace_HR" && !"velocity_smooth" %in% names(stream_df) && "distance" %in% names(stream_df)) {
      stream_df <- stream_df %>%
        dplyr::mutate(time_diff = c(0, diff(.data$time)), dist_diff = c(0, diff(.data$distance))) %>%
        dplyr::mutate(speed = ifelse(.data$time_diff > 0, .data$dist_diff / .data$time_diff, 0)) %>%
        dplyr::filter(.data$speed >= 0)
    } else if (decouple_metric == "Pace_HR" && "velocity_smooth" %in% names(stream_df)) {
        stream_df <- dplyr::rename(stream_df, speed = "velocity_smooth")
    }

    metric_col <- if(decouple_metric == "Pace_HR") "speed" else "watts"
    if (!metric_col %in% names(stream_df) || nrow(stream_df) < 10) {
        stop(paste("Missing or insufficient data for metric column:", metric_col))
    }

    mid_point_index <- floor(nrow(stream_df) / 2)
    if (mid_point_index < 5) stop("Not enough data points for reliable split-half analysis (< 5 in first half).")

    first_half <- stream_df[1:mid_point_index, ]
    second_half <- stream_df[(mid_point_index + 1):nrow(stream_df), ]

    mean_hr1 <- mean(first_half$heartrate, na.rm = TRUE)
    mean_hr2 <- mean(second_half$heartrate, na.rm = TRUE)
    mean_output1 <- mean(first_half[[metric_col]], na.rm = TRUE)
    mean_output2 <- mean(second_half[[metric_col]], na.rm = TRUE)

    ef1 <- if(mean_hr1 > 0) mean_output1 / mean_hr1 else 0
    ef2 <- if(mean_hr2 > 0) mean_output2 / mean_hr2 else 0

    decoupling_pct <- if (ef1 > 0 && mean_hr1 > 0 && mean_hr2 > 0) { (ef1 - ef2) / ef1 * 100 } else { NA }

    message("Decoupling calculation from stream_df complete.")
    return(decoupling_pct) # Return single value when using stream_df
  }

  # --- Continue with API fetching if stream_df is NULL ---
  # --- Date Handling ---
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), error = function(e) analysis_end_date - lubridate::days(365))
  if (analysis_start_date >= analysis_end_date) stop("start_date must be before end_date.")

  message(sprintf("Calculating Decoupling data from %s to %s.", analysis_start_date, analysis_end_date))
  message(sprintf("Metric: %s, Activity types: %s", decouple_metric, paste(activity_type, collapse=", ")))
  message(sprintf("Minimum duration: %d mins. Processing max %d recent activities.", min_duration_mins, max_activities))
  message("\n*** WARNING: Fetching detailed streams can be slow. Please be patient. ***\n")

  # --- Fetch Activity List ---
  message("Fetching activity list...")
  activities_list <- tryCatch({
      # Call get_activity_list once without pagination arguments
      # Returns list of recent activities
      rStrava::get_activity_list(stoken)
  }, error = function(e) {
      message("Error fetching activity list: ", e$message)
      NULL
  })

  if (is.null(activities_list) || length(activities_list) == 0) stop("Could not fetch any activities.")

  # --- Filter Activities (from the original list) ---
  message("Filtering activity list...")
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  safe_as_numeric <- function(x) { as.numeric(x %||% 0) }

  # Define filtering function
  filter_activity <- function(act) {
    act_date = lubridate::as_date(lubridate::ymd_hms(act$start_date_local %||% NA))
    duration_sec = safe_as_numeric(act$moving_time)
    type = act$type %||% "Unknown"
    
    if (is.na(act_date) || act_date < analysis_start_date || act_date > analysis_end_date) return(FALSE)
    if (!type %in% activity_type) return(FALSE)
    if (duration_sec < (min_duration_mins * 60)) return(FALSE)
    return(TRUE)
  }
  
  # Apply filter and sort by date (descending)
  # Assuming activities_list is already somewhat sorted recent first
  # If strict chronological processing is needed, sort activities_list by date first
  filtered_activities_list <- Filter(filter_activity, activities_list)
  
  # Sort by date descending (newest first) if not already
  filtered_activities_list <- filtered_activities_list[order(sapply(filtered_activities_list, function(x) lubridate::ymd_hms(x$start_date_local)), decreasing = TRUE)]

  # Apply max_activities limit
  if (length(filtered_activities_list) > max_activities) {
      message(sprintf("Applying max_activities limit: Keeping latest %d out of %d filtered activities.", max_activities, length(filtered_activities_list)))
      activities_to_process_list <- filtered_activities_list[1:max_activities]
  } else {
      activities_to_process_list <- filtered_activities_list
  }

  if (length(activities_to_process_list) == 0) stop("No activities met the specified criteria (type, duration, date range).")
  message(sprintf("Found %d activities meeting criteria. Fetching streams...", length(activities_to_process_list)))

  # --- Fetch Streams and Calculate Decoupling ---
  decoupling_results <- list()
  successful_calculations <- 0 # Initialize counter for successful calculations
  target_successful_calculations <- 10 # Set target for successful calculations

  pb <- utils::txtProgressBar(min = 0, max = length(activities_to_process_list), style = 3)

  for (i in 1:length(activities_to_process_list)) {
    current_activity_element <- activities_to_process_list[[i]]
    act_id <- as.character(current_activity_element$id %||% NA)
    act_date <- lubridate::as_date(lubridate::ymd_hms(current_activity_element$start_date_local %||% NA))
    
    if(is.na(act_id) || is.na(act_date)) {
        message(sprintf("\n[%d/%d] Skipping activity with missing ID or date.", i, length(activities_to_process_list)))
        utils::setTxtProgressBar(pb, i)
        next
    }
    
    message(sprintf("\n[%d/%d] Processing Activity ID: %s (%s) using direct API call", i, length(activities_to_process_list), act_id, act_date))
    Sys.sleep(1) # API rate limit

    message("  Fetching streams...")
    
    # Define required_streams INSIDE the tryCatch block to ensure scope
    required_streams_local <- c("time", "heartrate")
    if (decouple_metric == "Pace_HR") { 
        required_streams_local <- c(required_streams_local, "distance") 
    } else { 
        required_streams_local <- c(required_streams_local, "watts") 
    }
    message(sprintf("  Requesting stream types: %s", paste(required_streams_local, collapse=", "))) 

    # Correct usage based on author's example:
    # Pass the full list to act_data and specify index with acts=i
    streams <- tryCatch({ 
        rStrava::get_activity_streams(
          act_data = activities_to_process_list, 
          acts = i, 
          stoken = stoken, 
          types = required_streams_local, # Use the locally defined variable
          resolution = "medium"
      )}, 
      error = function(e) { message(sprintf("  ERROR fetching streams for %s: %s", act_id, e$message)); NULL })

    utils::setTxtProgressBar(pb, i)

    if(is.null(streams)) {
        message("  Failed to create stream_df from fetched streams. Skipping.")
        next
    }
    message(sprintf("  Initial stream_df created via API with %d rows. Columns: %s", nrow(streams), paste(names(streams), collapse=", ")))

    # Apply na.omit (important after manual creation)
    streams <- tryCatch(stats::na.omit(streams), error = function(e) { message(sprintf("  ERROR during na.omit: %s", e$message)); NULL })

    if(is.null(streams) || nrow(streams) < 10) {
        message(sprintf("  SKIPPING activity %s: Not enough valid rows (< 10) after na.omit or error during omit.", act_id))
        next
    }
    message(sprintf("  stream_df has %d rows after na.omit.", nrow(streams)))

    # Calculate speed if missing (this block should now correctly follow manual processing)
    if(decouple_metric == "Pace_HR" && !"speed" %in% names(streams) && "distance" %in% names(streams)) {
      message("  Calculating speed from distance...")
      streams <- streams %>% 
        dplyr::mutate(time_diff = c(0, diff(.data$time)), dist_diff = c(0, diff(.data$distance))) %>% 
        dplyr::mutate(speed = ifelse(.data$time_diff > 0, .data$dist_diff / .data$time_diff, 0)) %>% 
        dplyr::filter(.data$speed >= 0)
      message(sprintf("  Calculated speed. stream_df now has %d rows.", nrow(streams)))
    }
    
    metric_col <- if(decouple_metric == "Pace_HR") "speed" else "power"
    if (!metric_col %in% names(streams) || nrow(streams) < 10) {
        message(sprintf("  SKIPPING activity %s: Metric column '%s' missing or insufficient rows (< 10) after processing.", act_id, metric_col))
        next
    }
    
    # --- DEBUGGING: Print stream summary before internal calculation ---
    message("---- DEBUG: Stream Summary Before Internal Calc ----")
    message(sprintf("    Activity ID: %s, Date: %s", act_id, act_date))
    message(sprintf("    Dimensions: %d rows, %d cols", nrow(streams), ncol(streams)))
    if("heartrate" %in% names(streams)) message(sprintf("    Heartrate Summary: Min=%.1f, Mean=%.1f, Max=%.1f, NAs=%d", min(streams$heartrate, na.rm=T), mean(streams$heartrate, na.rm=T), max(streams$heartrate, na.rm=T), sum(is.na(streams$heartrate)))) else message("    Heartrate column missing!")
    metric_col_for_debug <- if(decouple_metric == "Pace_HR") "speed" else "watts"
    # Need to handle case where speed might not exist yet if only distance was provided
    if(metric_col_for_debug == "speed" && !metric_col_for_debug %in% names(streams) && "velocity_smooth" %in% names(streams)) metric_col_for_debug <- "velocity_smooth"
    if(metric_col_for_debug %in% names(streams)) message(sprintf("    %s Summary: Min=%.2f, Mean=%.2f, Max=%.2f, NAs=%d", metric_col_for_debug, min(streams[[metric_col_for_debug]], na.rm=T), mean(streams[[metric_col_for_debug]], na.rm=T), max(streams[[metric_col_for_debug]], na.rm=T), sum(is.na(streams[[metric_col_for_debug]])))) else message(sprintf("    %s column missing!", metric_col_for_debug))
    message("-------------------------------------------------")
    # --- END DEBUGGING ---

    # Calculate decoupling using the SAME logic as the stream_df path
    # This involves: calculating speed if needed, splitting halves, calculating means, calculating EFs, calculating decoupling_pct
    # ... (省略详细计算代码) ...
    # Assume the result is stored in 'activity_decoupling'
    # Replicate the calculation logic here for clarity in debugging if needed
    # --- Start Internal Calc Logic (Simplified Representation) ---
    internal_stream_df <- streams # Use the potentially modified streams df
    # Calculate speed if missing (replication of logic from stream_df path)
    if(decouple_metric == "Pace_HR" && !"speed" %in% names(internal_stream_df) && !"velocity_smooth" %in% names(internal_stream_df) && "distance" %in% names(internal_stream_df)) {
        message("  DEBUG: Calculating speed internally...")
        internal_stream_df <- internal_stream_df %>%
            dplyr::mutate(time_diff = c(0, diff(.data$time)), dist_diff = c(0, diff(.data$distance))) %>%
            dplyr::mutate(speed = ifelse(.data$time_diff > 0, .data$dist_diff / .data$time_diff, 0)) %>%
            dplyr::filter(.data$speed >= 0)
    } else if (decouple_metric == "Pace_HR" && "velocity_smooth" %in% names(internal_stream_df)) {
        internal_stream_df <- dplyr::rename(internal_stream_df, speed = "velocity_smooth")
    }
    
    internal_metric_col <- if(decouple_metric == "Pace_HR") "speed" else "watts"
    
    activity_decoupling <- NA # Default to NA
    if (internal_metric_col %in% names(internal_stream_df) && nrow(internal_stream_df) >= 10) {
        mid_point_index <- floor(nrow(internal_stream_df) / 2)
        if(mid_point_index >= 5) {
            first_half <- internal_stream_df[1:mid_point_index, ]
            second_half <- internal_stream_df[(mid_point_index + 1):nrow(internal_stream_df), ]
            mean_hr1 <- mean(first_half$heartrate, na.rm = TRUE)
            mean_hr2 <- mean(second_half$heartrate, na.rm = TRUE)
            mean_output1 <- mean(first_half[[internal_metric_col]], na.rm = TRUE)
            mean_output2 <- mean(second_half[[internal_metric_col]], na.rm = TRUE)
            ef1 <- if(mean_hr1 > 0) mean_output1 / mean_hr1 else 0
            ef2 <- if(mean_hr2 > 0) mean_output2 / mean_hr2 else 0
            activity_decoupling <- if (ef1 > 0 && mean_hr1 > 0 && mean_hr2 > 0) { (ef1 - ef2) / ef1 * 100 } else { NA }
        } else { message("  DEBUG: mid_point_index < 5 after speed calc/rename. Skipping calc.") }
    } else { message(sprintf("  DEBUG: Metric column '%s' missing or not enough rows (%d) after speed calc/rename. Skipping calc.", internal_metric_col, nrow(internal_stream_df))) }
    # --- End Internal Calc Logic ---

    if(!is.na(activity_decoupling) && is.finite(activity_decoupling)){ # <--- 检查点 4：计算结果是否为 NA 或 Inf?
        decoupling_results[[act_id]] <- dplyr::tibble(date = act_date, decoupling = activity_decoupling)
        successful_calculations <- successful_calculations + 1 # Increment success counter
        message(sprintf("  Successfully calculated decoupling for %d / %d target activities.", successful_calculations, target_successful_calculations))
        if (successful_calculations >= target_successful_calculations) {
          message(sprintf("  Reached target of %d successful calculations. Stopping stream processing.", target_successful_calculations))
          break # Target reached, exit loop
        }
    }
  }
  close(pb)
  message("\nFinished processing streams.")

  if (length(decoupling_results) == 0) {
    stop("Could not calculate decoupling for any activities after fetching streams.")
  }

  plot_data <- dplyr::bind_rows(decoupling_results) %>% dplyr::arrange(.data$date)

  message(sprintf("Calculation complete. Obtained decoupling data for %d activities.", nrow(plot_data))) # Final update message
  return(plot_data)
}

# Helper needed if not globally available
# `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x 