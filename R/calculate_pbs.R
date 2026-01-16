# R/calculate_pbs.R

#' Calculate Personal Bests (PBs) from Local Strava Data
#'
#' Tracks personal best times for standard distances (1k, 5k, 10k, half marathon, marathon)
#' by analyzing detailed activity files from Strava export data.
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: date, type, filename, distance.
#' @param export_dir Base directory of the Strava export containing the activities folder.
#'   Default is "strava_export_data".
#' @param activity_type Type of activities to analyze (typically "Run"). Default "Run".
#' @param start_date Optional start date for analysis (YYYY-MM-DD). Defaults to NULL (all dates).
#' @param end_date Optional end date for analysis (YYYY-MM-DD). Defaults to NULL (all dates).
#' @param distances_m Target distances in meters to track.
#'   Default: c(1000, 5000, 10000, 21097.5, 42195) for 1k, 5k, 10k, half, full marathon.
#'
#' @return A data frame with columns: activity_id, activity_date, distance,
#'   elapsed_time, moving_time, time_seconds, cumulative_pb_seconds, is_pb,
#'   distance_label, time_period
#'
#' @details
#' This function analyzes detailed activity files (FIT/TCX/GPX) to find the fastest
#' efforts at specified distances. It tracks cumulative personal bests over time,
#' showing when new PBs are set.
#'
#' **Note**: Requires detailed activity files from your Strava export. Activities
#' must be long enough to contain the target distance segments.
#'
#' @importFrom dplyr filter select mutate arrange %>% group_by
#' @importFrom lubridate as_date period seconds_to_period
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(sample_pbs)
#' print(head(sample_pbs))
#'
#' \dontrun{
#' # Load local activities
#' activities <- load_local_activities("strava_export_data/activities.csv")
#'
#' # Calculate PBs for standard running distances
#' pbs_data <- calculate_pbs(
#'   activities_data = activities,
#'   export_dir = "strava_export_data",
#'   activity_type = "Run"
#' )
#' print(head(pbs_data))
#' }
calculate_pbs <- function(activities_data,
                          export_dir = "strava_export_data",
                          activity_type = "Run",
                          start_date = NULL,
                          end_date = NULL,
                          distances_m = c(1000, 5000, 10000, 21097.5, 42195)) {
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

  message(sprintf("Calculating PBs from %s to %s.", analysis_start_date, analysis_end_date))
  message(sprintf("Target distances: %s", paste(distances_m, "m", collapse = ", ")))

  # --- Filter Activities ---
  filtered_activities <- activities_data %>%
    dplyr::filter(.data$date >= analysis_start_date & .data$date <= analysis_end_date)

  if (!is.null(activity_type)) {
    filtered_activities <- filtered_activities %>%
      dplyr::filter(.data$type %in% activity_type)
  }

  # Filter out activities without files
  filtered_activities <- filtered_activities %>%
    dplyr::filter(!is.na(.data$filename) & .data$filename != "")

  # Filter activities that are long enough for the smallest target distance
  min_distance <- min(distances_m)
  filtered_activities <- filtered_activities %>%
    dplyr::filter(!is.na(.data$distance) & .data$distance >= min_distance)

  if (nrow(filtered_activities) == 0) {
    warning("No activities meet the criteria for PB analysis.")
    return(data.frame(
      activity_id = numeric(0),
      activity_date = lubridate::as_datetime(character(0)),
      distance = numeric(0),
      elapsed_time = numeric(0),
      moving_time = numeric(0),
      time_seconds = numeric(0),
      cumulative_pb_seconds = numeric(0),
      is_pb = logical(0),
      distance_label = character(0),
      time_period = character(0)
    ))
  }

  message(sprintf("Analyzing %d activities for PBs...", nrow(filtered_activities)))

  # --- Calculate Best Efforts for Each Activity ---
  all_efforts <- purrr::map_dfr(1:nrow(filtered_activities), function(i) {
    activity <- filtered_activities[i, ]

    message(sprintf(
      "Processing activity %d/%d: %s (%s)",
      i, nrow(filtered_activities), activity$name, activity$date
    ))

    # Parse activity file
    file_path <- file.path(export_dir, activity$filename)
    stream_data <- parse_activity_file(file_path, export_dir)

    if (is.null(stream_data) || nrow(stream_data) < 10) {
      warning(sprintf("Insufficient data in file for activity %s", activity$id))
      return(NULL)
    }

    # Check for distance data
    if (!"distance" %in% names(stream_data) || all(is.na(stream_data$distance))) {
      warning(sprintf("No distance data for activity %s", activity$id))
      return(NULL)
    }

    # Calculate best efforts for each target distance
    efforts <- purrr::map_dfr(distances_m, function(target_distance) {
      best_effort <- find_best_effort(stream_data, target_distance)

      if (is.null(best_effort)) {
        return(NULL)
      }

      data.frame(
        activity_id = activity$id,
        activity_date = activity$start_date_local,
        distance = target_distance,
        time_seconds = best_effort$time_seconds,
        start_distance = best_effort$start_distance,
        end_distance = best_effort$end_distance,
        stringsAsFactors = FALSE
      )
    })

    return(efforts)
  })

  if (is.null(all_efforts) || nrow(all_efforts) == 0) {
    warning("No best efforts could be calculated.")
    return(data.frame(
      activity_id = numeric(0),
      activity_date = lubridate::as_datetime(character(0)),
      distance = numeric(0),
      time_seconds = numeric(0),
      cumulative_pb_seconds = numeric(0),
      is_pb = logical(0),
      distance_label = character(0),
      time_period = character(0)
    ))
  }

  # --- Track Personal Bests Over Time ---
  all_efforts <- all_efforts %>%
    dplyr::arrange(.data$activity_date, .data$distance)

  # For each distance, track cumulative PB
  pb_results <- all_efforts %>%
    dplyr::group_by(.data$distance) %>%
    dplyr::arrange(.data$activity_date) %>%
    dplyr::mutate(
      cumulative_pb_seconds = cummin(.data$time_seconds),
      is_pb = .data$time_seconds == .data$cumulative_pb_seconds
    ) %>%
    dplyr::ungroup()

  # Add distance labels
  distance_labels <- c(
    "1000" = "1k",
    "5000" = "5k",
    "10000" = "10k",
    "21097.5" = "Half Marathon",
    "42195" = "Marathon"
  )

  pb_results <- pb_results %>%
    dplyr::mutate(
      distance_label = dplyr::case_when(
        .data$distance == 1000 ~ "1k",
        .data$distance == 5000 ~ "5k",
        .data$distance == 10000 ~ "10k",
        .data$distance == 21097.5 ~ "Half Marathon",
        .data$distance == 42195 ~ "Marathon",
        TRUE ~ paste0(round(.data$distance), "m")
      ),
      distance_label = factor(.data$distance_label,
        levels = c("1k", "5k", "10k", "Half Marathon", "Marathon")
      ),
      time_period = as.character(lubridate::seconds_to_period(.data$time_seconds))
    )

  # Add elapsed_time and moving_time (same as time_seconds for best efforts)
  pb_results <- pb_results %>%
    dplyr::mutate(
      elapsed_time = .data$time_seconds,
      moving_time = .data$time_seconds
    )

  # Select final columns
  pb_results <- pb_results %>%
    dplyr::select(
      .data$activity_id, .data$activity_date, .data$distance,
      .data$elapsed_time, .data$moving_time, .data$time_seconds,
      .data$cumulative_pb_seconds, .data$is_pb,
      .data$distance_label, .data$time_period
    ) %>%
    dplyr::arrange(.data$activity_date, .data$distance)

  message(sprintf(
    "PB analysis complete. Found %d efforts, %d are new PBs.",
    nrow(pb_results), sum(pb_results$is_pb)
  ))

  return(pb_results)
}

#' Find Best Effort for Target Distance
#' @keywords internal
find_best_effort <- function(stream_data, target_distance) {
  # Remove rows with missing distance or time
  valid_data <- stream_data[!is.na(stream_data$distance) & !is.na(stream_data$time), ]

  if (nrow(valid_data) < 10) {
    return(NULL)
  }

  # Check if activity is long enough
  max_distance <- max(valid_data$distance, na.rm = TRUE)
  if (max_distance < target_distance) {
    return(NULL)
  }

  # Use sliding window to find fastest segment of target distance
  # Allow 2% tolerance for distance matching
  tolerance <- target_distance * 0.02

  best_time <- Inf
  best_start_idx <- NA
  best_end_idx <- NA

  # For each starting point, find the point where distance >= target
  for (i in 1:(nrow(valid_data) - 1)) {
    start_dist <- valid_data$distance[i]
    target_dist <- start_dist + target_distance

    # Find first point that reaches or exceeds target distance
    candidates <- which(valid_data$distance >= target_dist)

    if (length(candidates) == 0) {
      break # No more segments possible
    }

    end_idx <- candidates[1]
    actual_dist <- valid_data$distance[end_idx] - start_dist

    # Check if distance is within tolerance
    if (abs(actual_dist - target_distance) <= tolerance) {
      elapsed_time <- as.numeric(difftime(valid_data$time[end_idx],
        valid_data$time[i],
        units = "secs"
      ))

      if (elapsed_time > 0 && elapsed_time < best_time) {
        best_time <- elapsed_time
        best_start_idx <- i
        best_end_idx <- end_idx
      }
    }
  }

  if (is.infinite(best_time) || is.na(best_start_idx)) {
    return(NULL)
  }

  return(list(
    time_seconds = best_time,
    start_distance = valid_data$distance[best_start_idx],
    end_distance = valid_data$distance[best_end_idx]
  ))
}
