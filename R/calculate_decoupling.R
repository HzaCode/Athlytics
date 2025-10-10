# R/calculate_decoupling.R

#' Calculate Aerobic Decoupling
#'
#' Calculates aerobic decoupling for Strava activities from local export data.
#'
#' Calculates aerobic decoupling (HR drift relative to pace/power) using detailed
#' activity stream data from local FIT/TCX/GPX files.
#'
#' @param activities_data A data frame from `load_local_activities()`. Required unless `stream_df` is provided.
#' @param export_dir Base directory of Strava export containing the activities folder.
#'   Default is "strava_export_data".
#' @param activity_type Type(s) of activities to analyze (e.g., "Run", "Ride").
#' @param decouple_metric Basis for calculation: "pace_hr" or "power_hr" 
#'   (legacy "Pace_HR"/"Power_HR" also supported).
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string or Date). Defaults to one year ago.
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to today.
#' @param min_duration_mins Minimum activity duration (minutes) to include. Default 40.
#' @param min_steady_minutes Minimum duration (minutes) for steady-state segment (default: 40).
#'   Activities shorter than this are automatically rejected for decoupling calculation.
#' @param steady_cv_threshold Coefficient of variation threshold for steady-state (default: 0.08 = 8%).
#'   Activities with higher variability are rejected as non-steady-state.
#' @param min_hr_coverage Minimum HR data coverage threshold (default: 0.9 = 90%).
#'   Activities with lower HR coverage are rejected as insufficient data quality.
#' @param stream_df Optional. A pre-fetched data frame for a *single* activity's stream.
#'   If provided, calculates decoupling for this data directly, ignoring other parameters.
#'   Must include columns: `time`, `heartrate`, and either `velocity_smooth`/`distance` 
#'   (for Pace_HR) or `watts` (for Power_HR).
#' @param stoken **Deprecated.** Legacy Strava token parameter maintained for backward compatibility only.
#'
#' @return Returns a data frame with columns:
#'   \describe{
#'     \item{date}{Activity date (Date class)}
#'     \item{decoupling}{Decoupling percentage (\\%). Positive = HR drift, negative = improved efficiency}
#'     \item{status}{Character. "ok" for successful calculation, "non_steady" if steady-state 
#'       criteria not met, "insufficient_data" if data quality issues}
#'   }
#'   OR a single numeric decoupling value if `stream_df` is provided.
#'
#' @details Provides data for `plot_decoupling`. Compares output/HR efficiency
#'   between first and second halves of activities. Positive values indicate
#'   HR drift (cardiovascular drift).
#'   
#'   **Best practice**: Use `load_local_activities()` to load data, then pass to this function.
#'   
#'   The function parses FIT/TCX/GPX files from your Strava export to extract detailed
#'   stream data (time, heartrate, distance/power). Activities are split into two halves,
#'   and the efficiency factor (output/HR) is compared between halves.
#'
#' @importFrom dplyr filter select mutate arrange %>% rename left_join case_when group_by summarise pull first last tibble slice_head lead lag
#' @importFrom lubridate as_date date days ymd ymd_hms as_datetime
#' @importFrom stats median na.omit
#' @importFrom rlang .data %||%
#' @importFrom purrr map_dfr possibly
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(Athlytics_sample_data)
#' print(head(athlytics_sample_decoupling))
#'
#' \dontrun{
#' # Load local activities
#' activities <- load_local_activities("strava_export_data/activities.csv")
#'
#' # Calculate Pace/HR decoupling for recent runs
#' run_decoupling <- calculate_decoupling(
#'     activities_data = activities,
#'     export_dir = "strava_export_data",
#'     activity_type = "Run",
#'     decouple_metric = "pace_hr",
#'     start_date = "2024-01-01"
#' )
#' print(tail(run_decoupling))
#' 
#' # Calculate for a single activity stream
#' # stream_data <- parse_activity_file("strava_export_data/activities/12345.fit")
#' # single_decoupling <- calculate_decoupling(stream_df = stream_data, decouple_metric = "pace_hr")
#' }
calculate_decoupling <- function(activities_data = NULL,
                                 export_dir = "strava_export_data",
                                 activity_type = c("Run", "Ride"),
                                 decouple_metric = c("pace_hr", "power_hr", "Pace_HR", "Power_HR"),
                                 start_date = NULL,
                                 end_date = NULL,
                                 min_duration_mins = 40,
                                 min_steady_minutes = 40,
                                 steady_cv_threshold = 0.08,
                                 min_hr_coverage = 0.9,
                                 stream_df = NULL,
                                 stoken = NULL) {
  
  # --- Handle deprecated stoken parameter ---
  if (!is.null(stoken)) {
    warning("'stoken' parameter is deprecated. Use local data via 'activities_data' and 'export_dir' instead.",
            "\nAPI-based analysis is no longer supported. Please use local Strava export files.")
    stop("API mode is deprecated. Please use local data: load_local_activities() + calculate_decoupling()")
  }
  
  # --- Input Validation ---
  decouple_metric <- match.arg(decouple_metric)
  
  # Normalize to lowercase (support legacy capitalized names)
  decouple_metric <- tolower(decouple_metric)
  
  # If stream_df provided, calculate for single activity
  if (!is.null(stream_df)) {
    return(calculate_single_decoupling(stream_df, decouple_metric))
  }
  
  # Otherwise, need activities_data
  if (is.null(activities_data) || !is.data.frame(activities_data)) {
    stop("`activities_data` must be provided as a data frame from load_local_activities().")
  }
  
  if (!is.numeric(min_duration_mins) || min_duration_mins <= 0) {
    stop("`min_duration_mins` must be a positive number.")
  }
  if (!is.numeric(min_steady_minutes) || min_steady_minutes <= 0) {
    stop("`min_steady_minutes` must be a positive number.")
  }
  if (!is.numeric(steady_cv_threshold) || steady_cv_threshold <= 0 || steady_cv_threshold > 1) {
    stop("`steady_cv_threshold` must be between 0 and 1.")
  }
  if (!is.numeric(min_hr_coverage) || min_hr_coverage <= 0 || min_hr_coverage > 1) {
    stop("`min_hr_coverage` must be between 0 and 1.")
  }
  
  # --- Date Handling ---
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  analysis_end_date <- tryCatch(lubridate::as_date(end_date %||% Sys.Date()), 
                                 error = function(e) Sys.Date())
  analysis_start_date <- tryCatch(lubridate::as_date(start_date %||% (analysis_end_date - lubridate::days(365))), 
                                   error = function(e) analysis_end_date - lubridate::days(365))
  
  if (analysis_start_date >= analysis_end_date) {
    stop("start_date must be before end_date.")
  }
  
  message(sprintf("Calculating decoupling (%s) from %s to %s.", 
                  decouple_metric, analysis_start_date, analysis_end_date))
  
  # --- Filter Activities ---
  filtered_activities <- activities_data %>%
    dplyr::filter(
      .data$date >= analysis_start_date,
      .data$date <= analysis_end_date,
      .data$type %in% activity_type,
      (.data$moving_time / 60) >= min_duration_mins
    ) %>%
    dplyr::arrange(dplyr::desc(.data$date))
  
  if (nrow(filtered_activities) == 0) {
    stop("No activities found matching the specified criteria.")
  }
  
  message(sprintf("Found %d activities meeting criteria. Processing...", nrow(filtered_activities)))
  
  # --- Process Each Activity ---
  decoupling_results <- purrr::map_dfr(1:nrow(filtered_activities), function(i) {
    activity <- filtered_activities[i, ]
    
    if (is.na(activity$filename) || activity$filename == "") {
      message(sprintf("[%d/%d] Skipping activity %s (no filename)", 
                      i, nrow(filtered_activities), activity$date))
      return(NULL)
    }
    
    # Construct file path
    file_path <- file.path(export_dir, activity$filename)
    
    if (!file.exists(file_path)) {
      message(sprintf("[%d/%d] Skipping activity %s (file not found: %s)", 
                      i, nrow(filtered_activities), activity$date, basename(file_path)))
      return(NULL)
    }
    
    message(sprintf("[%d/%d] Processing %s (%s)", 
                    i, nrow(filtered_activities), activity$date, basename(file_path)))
    
    # Parse activity file
    stream_data <- tryCatch({
      parse_activity_file(file_path)
    }, error = function(e) {
      message(sprintf("  Error parsing file: %s", e$message))
      return(NULL)
    })
    
    if (is.null(stream_data) || nrow(stream_data) == 0) {
      message("  No stream data extracted")
      return(NULL)
    }
    
    # Calculate decoupling for this activity
    decoupling_value <- tryCatch({
      calculate_single_decoupling(stream_data, decouple_metric)
    }, error = function(e) {
      message(sprintf("  Error calculating decoupling: %s", e$message))
      return(NA_real_)
    })
    
    if (is.na(decoupling_value)) {
      return(NULL)
    }
    
    # Return result
    data.frame(
      date = activity$date,
      decoupling = decoupling_value,
      stringsAsFactors = FALSE
    )
  })
  
  if (is.null(decoupling_results) || nrow(decoupling_results) == 0) {
    stop("No decoupling values could be calculated. Check that activity files contain stream data.")
  }
  
  message(sprintf("Successfully calculated decoupling for %d activities.", nrow(decoupling_results)))
  
  return(decoupling_results %>% dplyr::arrange(.data$date))
}


#' Internal: Calculate Decoupling for Single Activity Stream
#' @keywords internal
#' @noRd
calculate_single_decoupling <- function(stream_df, decouple_metric) {
  
  # Validate stream_df structure
  required_cols <- c("time", "heartrate")
  if (decouple_metric == "pace_hr") {
    if (!"distance" %in% colnames(stream_df) && !"velocity_smooth" %in% colnames(stream_df)) {
      stop("For pace_hr decoupling, stream_df must contain 'distance' or 'velocity_smooth' column.")
    }
  } else {  # power_hr
    if (!"watts" %in% colnames(stream_df)) {
      stop("For power_hr decoupling, stream_df must contain 'watts' column.")
    }
  }
  
  missing_cols <- setdiff(required_cols, colnames(stream_df))
  if (length(missing_cols) > 0) {
    stop("stream_df missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Remove NA values
  stream_clean <- stream_df %>%
    dplyr::filter(!is.na(.data$time), !is.na(.data$heartrate))
  
  if (nrow(stream_clean) < 100) {
    stop("Insufficient data points for decoupling calculation (need at least 100 points).")
  }
  
  # Calculate velocity if needed
  if (decouple_metric == "pace_hr") {
    if ("velocity_smooth" %in% colnames(stream_clean)) {
      stream_clean <- stream_clean %>%
        dplyr::mutate(velocity = .data$velocity_smooth)
    } else if ("distance" %in% colnames(stream_clean)) {
      # Calculate velocity from distance
      stream_clean <- stream_clean %>%
        dplyr::arrange(.data$time) %>%
        dplyr::mutate(
          distance_diff = .data$distance - dplyr::lag(.data$distance, default = first(.data$distance)),
          time_diff = .data$time - dplyr::lag(.data$time, default = first(.data$time)),
          velocity = ifelse(.data$time_diff > 0, .data$distance_diff / .data$time_diff, 0)
        )
    }
    
    stream_clean <- stream_clean %>%
      dplyr::filter(!is.na(.data$velocity), .data$velocity > 0, .data$heartrate > 0)
  } else {
    stream_clean <- stream_clean %>%
      dplyr::filter(!is.na(.data$watts), .data$watts > 0, .data$heartrate > 0)
  }
  
  if (nrow(stream_clean) < 100) {
    stop("Insufficient valid data after filtering (need at least 100 points with positive values).")
  }
  
  # Split into two halves
  midpoint <- floor(nrow(stream_clean) / 2)
  first_half <- stream_clean[1:midpoint, ]
  second_half <- stream_clean[(midpoint + 1):nrow(stream_clean), ]
  
  # Calculate efficiency factor for each half
  if (decouple_metric == "Pace_HR") {
    ef_first <- median(first_half$velocity / first_half$heartrate, na.rm = TRUE)
    ef_second <- median(second_half$velocity / second_half$heartrate, na.rm = TRUE)
  } else {
    ef_first <- median(first_half$watts / first_half$heartrate, na.rm = TRUE)
    ef_second <- median(second_half$watts / second_half$heartrate, na.rm = TRUE)
  }
  
  # Calculate decoupling percentage
  if (ef_first > 0) {
    decoupling_pct <- ((ef_first - ef_second) / ef_first) * 100
  } else {
    decoupling_pct <- NA_real_
  }
  
  return(decoupling_pct)
}
