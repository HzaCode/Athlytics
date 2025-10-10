# R/calculate_decoupling.R

#' Calculate Cardiovascular Decoupling
#'
#' Quantifies cardiovascular drift during endurance activities by comparing the
#' efficiency (pace/HR or power/HR) between the first and second halves of an activity.
#' Decoupling is a key indicator of aerobic endurance and durability (Maunder et al., 2021).
#'
#' @description
#' Cardiovascular decoupling (aerobic decoupling) measures the deterioration of
#' efficiency during prolonged exercise. As you fatigue, heart rate tends to rise
#' (cardiac drift) while power/pace may decrease, indicating reduced efficiency.
#' Low decoupling suggests good aerobic fitness and durability at the given intensity.
#'
#' **Decoupling Formula:**
#' 
#' Decoupling\% = ((EF_half1 - EF_half2) / EF_half1) × 100
#' 
#' Where:
#' \itemize{
#'   \item **For Running**: EF = Pace / Heart Rate
#'   \item **For Cycling**: EF = Power / Heart Rate
#' }
#'
#' **Interpretation:**
#' \itemize{
#'   \item **< 5\%**: Excellent aerobic fitness and pacing
#'   \item **5-10\%**: Acceptable for the intensity
#'   \item **> 10\%**: Possible inadequate aerobic base, too fast pace, or environmental stress
#'   \item **Negative values**: Possible warm-up effect or pacing issues
#' }
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: `date`, `type`, `filename`. The `filename` column is used
#'   to locate detailed activity files (FIT/TCX/GPX) in the Strava export.
#' @param export_dir Character string. Base directory of the Strava export containing
#'   the activities folder with detailed activity files. Default: `"strava_export_data"`.
#'   If you loaded from a ZIP, this should be the path where the ZIP was extracted
#'   (handled automatically by `load_local_activities`).
#' @param activity_type Character vector or single string. Activity type(s) to analyze.
#'   Common values: `"Run"`, `"Ride"`. Default: `c("Run", "Ride")`.
#' @param start_date Optional. Analysis start date (YYYY-MM-DD string, Date, or POSIXct).
#'   Defaults to NULL (include all dates).
#' @param end_date Optional. Analysis end date (YYYY-MM-DD string, Date, or POSIXct).
#'   Defaults to NULL (include all dates).
#' @param min_duration_mins Numeric. Minimum activity duration in minutes to include
#'   in analysis (default: 40). Shorter activities may not show meaningful decoupling.
#'   Recommended: 45-60+ minutes for reliable decoupling assessment.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{date}{Activity date (Date class)}
#'   \item{activity_id}{Strava activity ID (character)}
#'   \item{decoupling_percent}{Decoupling percentage (numeric). Positive values indicate drift.}
#' }
#'
#' @details
#' **Algorithm:**
#' 1. Parse detailed activity files (FIT/TCX/GPX) to extract time-series data
#' 2. Split each activity into first and second halves by time
#' 3. Calculate average efficiency for each half:
#'    - Running: avg_pace / avg_HR for each half
#'    - Cycling: avg_power / avg_HR for each half
#' 4. Compute decoupling percentage: ((half1_EF - half2_EF) / half1_EF) × 100
#'
#' **Data Requirements:**
#' \itemize{
#'   \item Requires detailed activity files (FIT/TCX/GPX) from Strava export
#'   \item Must have continuous heart rate data throughout activity
#'   \item Cycling decoupling requires power meter data
#'   \item Activities must be > min_duration_mins (default 40 minutes)
#' }
#'
#' **Best Practices:**
#' \itemize{
#'   \item Use for steady-state aerobic efforts (long runs, endurance rides)
#'   \item Avoid interval workouts (high variability confounds results)
#'   \item Consistent pacing yields more interpretable results
#'   \item Track decoupling trends over time (not individual sessions)
#' }
#'
#' **Factors Affecting Decoupling:**
#' \itemize{
#'   \item **Aerobic Fitness**: Better base = lower decoupling
#'   \item **Intensity**: Higher intensity = more decoupling
#'   \item **Temperature**: Heat increases cardiac drift
#'   \item **Hydration**: Dehydration worsens decoupling
#'   \item **Fatigue**: Accumulated fatigue increases decoupling
#'   \item **Pacing**: Poor pacing (starting too fast) inflates decoupling
#' }
#'
#' **Training Applications:**
#' \itemize{
#'   \item Monitor aerobic base development during base training
#'   \item Assess readiness for race-specific intensity
#'   \item Validate pacing strategy for long events
#'   \item Identify need for more aerobic volume
#' }
#'
#' @note
#' This function requires the optional `FITfileR` package for parsing FIT files.
#' Install with: `install.packages("FITfileR")`. TCX and GPX parsing uses `XML` package.
#'
#' @references
#' Maunder, E., Seiler, S., Mildenhall, M. J., Kilding, A. E., & Plews, D. J. (2021).
#' The Importance of 'Durability' in the Physiological Profiling of Endurance Athletes.
#' *Sports Medicine*, 51(8), 1619-1628.
#'
#' @seealso
#' \code{\link{plot_decoupling}} for visualization,
#' \code{\link{calculate_ef}} for cross-activity efficiency trends,
#' \code{\link{parse_activity_file}} for low-level file parsing,
#' \code{\link{load_local_activities}} for data loading
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