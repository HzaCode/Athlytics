# R/calculate_decoupling.R

#' Calculate Aerobic Decoupling from Local Strava Data
#'
#' Calculates cardiovascular (aerobic) decoupling by analyzing the relationship
#' between normalized power/pace and heart rate. Decoupling indicates cardiovascular
#' drift, where heart rate rises relative to power/pace output during sustained efforts.
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: date, type, filename.
#' @param export_dir Base directory of the Strava export containing the activities folder.
#'   Default is "strava_export_data".
#' @param activity_type Type of activities to analyze (e.g., "Run", "Ride").
#' @param start_date Optional start date for analysis (YYYY-MM-DD). Defaults to NULL (all dates).
#' @param end_date Optional end date for analysis (YYYY-MM-DD). Defaults to NULL (all dates).
#' @param min_duration_mins Minimum activity duration in minutes to include. Default 40.
#'
#' @return A data frame with columns: date, activity_id, decoupling_percent
#'
#' @details
#' Aerobic decoupling is calculated by comparing the first and second halves of an activity:
#' \itemize{
#'   \item Runs: Pace/HR ratio between first and second half
#'   \item Rides: Power/HR ratio between first and second half
#' }
#' A decoupling > 5\% typically indicates cardiovascular drift or inadequate aerobic fitness
#' for the given intensity.
#'
#' **Note**: This function requires detailed activity files (FIT/TCX/GPX) from your
#' Strava export to extract second-by-second heart rate and power/pace data.
#'
#' @importFrom dplyr filter select mutate arrange %>% group_by summarise
#' @importFrom lubridate as_date
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(athlytics_sample_decoupling)
#' print(head(athlytics_sample_decoupling))
#'
#' \dontrun{
#' # Load local activities
#' activities <- load_local_activities("strava_export_data/activities.csv")
#'
#' # Calculate decoupling for long runs
#' decoupling_data <- calculate_decoupling(
#'   activities_data = activities,
#'   export_dir = "strava_export_data",
#'   activity_type = "Run",
#'   min_duration_mins = 60
#' )
#' print(head(decoupling_data))
#' }
calculate_decoupling <- function(activities_data,
                                 export_dir = "strava_export_data",
                                 activity_type = c("Run", "Ride"),
                                 start_date = NULL,
                                 end_date = NULL,
                                 min_duration_mins = 40) {

  # --- Input Validation ---
  if (missing(activities_data) || is.null(activities_data)) {
    stop("`activities_data` must be provided. Use load_local_activities() to load your Strava export data.")
  }

  if (!is.data.frame(activities_data)) {
    stop("`activities_data` must be a data frame.")
  }

  if (!"filename" %in% names(activities_data)) {
    stop("`activities_data` must include 'filename' column. Please use the latest version of load_local_activities().")
  }

  if (!dir.exists(export_dir)) {
    stop("Export directory not found: ", export_dir)
  }

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  # --- Date Handling ---
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), error = function(e) analysis_end_date - lubridate::days(365))

  message(sprintf("Calculating decoupling data from %s to %s.", analysis_start_date, analysis_end_date))

  # --- Filter Activities ---
  filtered_activities <- activities_data %>%
    dplyr::filter(.data$date >= analysis_start_date & .data$date <= analysis_end_date)

  if (!is.null(activity_type)) {
    filtered_activities <- filtered_activities %>%
      dplyr::filter(.data$type %in% activity_type)
  }

  # Filter by minimum duration
  min_duration_sec <- min_duration_mins * 60
  filtered_activities <- filtered_activities %>%
    dplyr::filter(!is.na(.data$moving_time) & .data$moving_time >= min_duration_sec)

  # Filter out activities without files
  filtered_activities <- filtered_activities %>%
    dplyr::filter(!is.na(.data$filename) & .data$filename != "")

  if (nrow(filtered_activities) == 0) {
    warning("No activities meet the criteria for decoupling analysis.")
    return(data.frame(
      date = lubridate::as_date(character(0)),
      activity_id = numeric(0),
      decoupling_percent = numeric(0)
    ))
  }

  message(sprintf("Analyzing %d activities for decoupling...", nrow(filtered_activities)))

  # --- Calculate Decoupling for Each Activity ---
  decoupling_results <- purrr::map_dfr(1:nrow(filtered_activities), function(i) {
    activity <- filtered_activities[i, ]
    
    message(sprintf("Processing activity %d/%d: %s (%s)", 
                    i, nrow(filtered_activities), activity$name, activity$date))

    # Parse activity file
    file_path <- file.path(export_dir, activity$filename)
    stream_data <- parse_activity_file(file_path, export_dir)

    if (is.null(stream_data) || nrow(stream_data) < 100) {
      warning(sprintf("Insufficient data in file for activity %s", activity$id))
      return(NULL)
    }

    # Check for required data
    if (!"heart_rate" %in% names(stream_data) || all(is.na(stream_data$heart_rate))) {
      warning(sprintf("No heart rate data for activity %s", activity$id))
      return(NULL)
    }

    # Calculate decoupling based on activity type
    act_type <- activity$type %||% "Run"
    
    if (act_type %in% c("Run", "VirtualRun")) {
      decoupling <- calculate_pace_hr_decoupling(stream_data)
    } else if (act_type %in% c("Ride", "VirtualRide")) {
      decoupling <- calculate_power_hr_decoupling(stream_data)
    } else {
      warning(sprintf("Unsupported activity type for decoupling: %s", act_type))
      return(NULL)
    }

    if (is.na(decoupling)) {
      return(NULL)
    }

    data.frame(
      date = activity$date,
      activity_id = activity$id,
      activity_name = activity$name,
      decoupling_percent = decoupling,
      stringsAsFactors = FALSE
    )
  })

  if (is.null(decoupling_results) || nrow(decoupling_results) == 0) {
    warning("No valid decoupling data could be calculated.")
    return(data.frame(
      date = lubridate::as_date(character(0)),
      activity_id = numeric(0),
      decoupling_percent = numeric(0)
    ))
  }

  decoupling_results <- decoupling_results %>%
    dplyr::arrange(.data$date)

  message(sprintf("Decoupling analysis complete. Calculated for %d activities.", nrow(decoupling_results)))
  
  return(decoupling_results)
}

#' Calculate Pace/HR Decoupling for Running
#' @keywords internal
calculate_pace_hr_decoupling <- function(stream_data) {
  # Remove rows with missing HR or speed
  valid_data <- stream_data[!is.na(stream_data$heart_rate) & 
                            !is.na(stream_data$speed) & 
                            stream_data$heart_rate > 0 & 
                            stream_data$speed > 0, ]

  if (nrow(valid_data) < 100) {
    return(NA)
  }

  # Split into first and second half
  midpoint <- floor(nrow(valid_data) / 2)
  first_half <- valid_data[1:midpoint, ]
  second_half <- valid_data[(midpoint + 1):nrow(valid_data), ]

  # Calculate average Pace/HR ratio for each half
  # Pace = 1/speed (higher pace = slower)
  first_half_ratio <- mean((1/first_half$speed) / first_half$heart_rate, na.rm = TRUE)
  second_half_ratio <- mean((1/second_half$speed) / second_half$heart_rate, na.rm = TRUE)

  if (is.na(first_half_ratio) || is.na(second_half_ratio) || first_half_ratio == 0) {
    return(NA)
  }

  # Decoupling = (second_half - first_half) / first_half * 100
  decoupling_percent <- ((second_half_ratio - first_half_ratio) / first_half_ratio) * 100

  return(decoupling_percent)
}

#' Calculate Power/HR Decoupling for Cycling
#' @keywords internal
calculate_power_hr_decoupling <- function(stream_data) {
  # Remove rows with missing HR or power
  valid_data <- stream_data[!is.na(stream_data$heart_rate) & 
                            !is.na(stream_data$power) & 
                            stream_data$heart_rate > 0 & 
                            stream_data$power > 0, ]

  if (nrow(valid_data) < 100) {
    return(NA)
  }

  # Split into first and second half
  midpoint <- floor(nrow(valid_data) / 2)
  first_half <- valid_data[1:midpoint, ]
  second_half <- valid_data[(midpoint + 1):nrow(valid_data), ]

  # Calculate average Power/HR ratio for each half
  first_half_ratio <- mean(first_half$power / first_half$heart_rate, na.rm = TRUE)
  second_half_ratio <- mean(second_half$power / second_half$heart_rate, na.rm = TRUE)

  if (is.na(first_half_ratio) || is.na(second_half_ratio) || first_half_ratio == 0) {
    return(NA)
  }

  # Decoupling = (first_half - second_half) / first_half * 100
  # Note: For power, we expect it to decrease (negative decoupling)
  decoupling_percent <- ((first_half_ratio - second_half_ratio) / first_half_ratio) * 100

  return(decoupling_percent)
}