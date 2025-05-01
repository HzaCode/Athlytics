# R/calculate_decoupling.R

#' Calculate Aerobic Decoupling
#'
#' Calculates aerobic decoupling for Strava activities.
#'
#' Analyzes heart rate and either pace or power data from activity streams
#' to determine the percentage of decoupling, indicating aerobic fitness changes
#' during an activity. Requires fetching detailed stream data via the Strava API,
#' which can be slow.
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
#' @return If `stream_df` is NOT provided: A data frame with columns `date` and
#'   `decoupling` (percentage) for activities meeting the criteria.
#'   If `stream_df` IS provided: A single numeric value representing the
#'   decoupling percentage for that specific stream.
#'
#' @details This function provides the data used by `plot_decoupling`.
#'   It compares the efficiency factor (output/heart rate) between the first
#'   and second halves of each activity. A positive decoupling percentage suggests
#'   a decline in efficiency (i.e., heart rate drift).
#'   Fetching streams from the Strava API can be time-consuming and subject to rate limits.
#'
#' @importFrom rStrava get_activity_list get_activity_streams
#' @importFrom dplyr filter select mutate arrange %>% rename left_join case_when group_by summarise pull first last tibble slice_head lead lag
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom stats median na.omit
#' @importFrom rlang .data %||%
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires authentication first:
#' # stoken <- rStrava::strava_oauth(..., cache = TRUE)
#'
#' # Calculate Pace/HR decoupling for recent Runs (limit to 10 for speed)
#' run_decoupling <- calculate_decoupling(
#'     stoken = stoken,
#'     activity_type = "Run",
#'     decouple_metric = "Pace_HR",
#'     max_activities = 10
#' )
#' print(tail(run_decoupling))
#'
#' # Example with pre-fetched stream data (replace ... with actual stream df)
#' # single_activity_decoupling <- calculate_decoupling(stream_df = ... , decouple_metric = "Power_HR")
#' # print(single_activity_decoupling)
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
      # Ensure enough rows
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
      # It should return a list of recent activities (number determined by rStrava)
      rStrava::get_activity_list(stoken)
  }, error = function(e) {
      message("Error fetching activity list: ", e$message)
      NULL
  })

  if (is.null(activities_list) || length(activities_list) == 0) stop("Could not fetch any activities.")

  # --- Filter Activities ---
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  safe_as_numeric <- function(x) { as.numeric(x %||% 0) }

  activities_df <- purrr::map_dfr(activities_list, ~{
    act_date = lubridate::as_date(lubridate::ymd_hms(.x$start_date_local %||% NA))
    duration_sec = safe_as_numeric(.x$moving_time)
    type = .x$type %||% "Unknown"
    if (is.na(act_date) || act_date < analysis_start_date || act_date > analysis_end_date) return(NULL)
    if (!.data$type %in% activity_type) return(NULL)
    if (duration_sec < (min_duration_mins * 60)) return(NULL)
    data.frame(id = as.character(.x$id %||% NA), date = act_date, type = .data$type, duration_sec = duration_sec, stringsAsFactors = FALSE)
  }) %>%
    dplyr::filter(!is.na(.data$id)) %>%
    dplyr::arrange(dplyr::desc(.data$date)) %>%
    dplyr::slice_head(n = max_activities)

  if (nrow(activities_df) == 0) stop("No activities met the specified criteria (type, duration, date range).")
  message(sprintf("Found %d activities meeting criteria. Fetching streams...", nrow(activities_df)))

  # --- Fetch Streams and Calculate Decoupling ---
  decoupling_results <- list()
  required_streams <- c("time", "heartrate")
  if (decouple_metric == "Pace_HR") { required_streams <- c(required_streams, "velocity_smooth", "distance")
  } else { required_streams <- c(required_streams, "watts") }

  pb <- utils::txtProgressBar(min = 0, max = nrow(activities_df), style = 3)

  for (i in 1:nrow(activities_df)) {
    act_id <- activities_df$id[i]
    act_date <- activities_df$date[i]
    Sys.sleep(1) # API rate limit

    streams <- tryCatch({ rStrava::get_activity_streams(stoken, id = act_id, types = required_streams)},
                       error = function(e) { message(sprintf("\nWarning: Failed to fetch streams for activity %s: %s", act_id, e$message)); NULL })

    utils::setTxtProgressBar(pb, i)
    if (is.null(streams)) next

    # Check if all required core streams were returned
    core_required <- c("time", "heartrate")
    metric_specific_required <- setdiff(required_streams, core_required)
    if(!all(core_required %in% names(streams))) {
        # message(sprintf("\nSkipping activity %s: Missing core streams (time/heartrate).", act_id))
        next
    }
    # Check if at least one required metric stream is present
    if(!any(metric_specific_required %in% names(streams))){
        # message(sprintf("\nSkipping activity %s: Missing required metric streams (%s).", act_id, paste(metric_specific_required, collapse="/")))
        next
    }

    stream_df <- tryCatch({
      df <- dplyr::tibble(time = streams$time$data, hr = streams$heartrate$data)
      # Prioritize velocity_smooth if available
      if ("velocity_smooth" %in% names(streams)) df <- dplyr::mutate(df, speed = streams$velocity_smooth$data)
      if ("distance" %in% names(streams) && !"speed" %in% names(df)) df <- dplyr::mutate(df, distance = streams$distance$data)
      if ("watts" %in% names(streams)) df <- dplyr::mutate(df, power = streams$watts$data)
      stats::na.omit(df)
    }, error = function(e) { NULL })

    if(is.null(stream_df) || nrow(stream_df) < 10) next

    # Calculate speed if missing
    if(decouple_metric == "Pace_HR" && !"speed" %in% names(stream_df) && "distance" %in% names(stream_df)) {
      stream_df <- stream_df %>%
        dplyr::mutate(time_diff = c(0, diff(.data$time)), dist_diff = c(0, diff(.data$distance))) %>%
        dplyr::mutate(speed = ifelse(.data$time_diff > 0, .data$dist_diff / .data$time_diff, 0)) %>%
        dplyr::filter(.data$speed >= 0)
    }

    metric_col <- if(decouple_metric == "Pace_HR") "speed" else "power"
    if (!metric_col %in% names(stream_df) || nrow(stream_df) < 10) next

    # Calculate Decoupling
    mid_point_index <- floor(nrow(stream_df) / 2)
    if (mid_point_index < 5) next

    first_half <- stream_df[1:mid_point_index, ]
    second_half <- stream_df[(mid_point_index + 1):nrow(stream_df), ]

    mean_hr1 <- mean(first_half$hr, na.rm = TRUE)
    mean_hr2 <- mean(second_half$hr, na.rm = TRUE)
    mean_output1 <- mean(first_half[[metric_col]], na.rm = TRUE)
    mean_output2 <- mean(second_half[[metric_col]], na.rm = TRUE)

    ef1 <- if(mean_hr1 > 0) mean_output1 / mean_hr1 else 0
    ef2 <- if(mean_hr2 > 0) mean_output2 / mean_hr2 else 0

    decoupling_pct <- if (ef1 > 0 && mean_hr1 > 0 && mean_hr2 > 0) { (ef1 - ef2) / ef1 * 100 } else { NA }

    if (!is.na(decoupling_pct)) {
      decoupling_results[[act_id]] <- dplyr::tibble(date = act_date, decoupling = decoupling_pct)
    }
  }
  close(pb)
  message("\nFinished processing streams.")

  if (length(decoupling_results) == 0) {
    stop("Could not calculate decoupling for any activities after fetching streams.")
  }

  plot_data <- dplyr::bind_rows(decoupling_results) %>% dplyr::arrange(.data$date)

  message("Calculation complete.")
  return(plot_data)
}

# Helper needed if not globally available
# `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x 