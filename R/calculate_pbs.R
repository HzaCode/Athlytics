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
#' @param start_date Optional start date for analysis (YYYY-MM-DD). If NULL, defaults to 365 days before `end_date`.
#' @param end_date End date for analysis (YYYY-MM-DD). Default `Sys.Date()` (today).
#' @param distances_m Target distances in meters to track.
#'   Default: c(1000, 5000, 10000, 21097.5, 42195) for 1k, 5k, 10k, half, full marathon.
#' @param verbose Logical. If TRUE, prints progress messages. Default FALSE.
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
#' Personal best tracking is a standard approach in endurance sport performance
#' monitoring. Systematic PB analysis over multiple distances helps identify
#' fitness improvements, training phase effectiveness, and performance peaks
#' (Matveyev, 1981). The multi-distance approach enables
#' athletes to assess both speed (shorter distances) and endurance (longer
#' distances) progression simultaneously.
#'
#' ## References
#' - Matveyev, L. P. (1981). *Fundamentals of Sports Training*. Moscow: Progress
#'   Publishers.
#'
#' **Note**: Requires detailed activity files from your Strava export. Activities
#' must be long enough to contain the target distance segments.
#'
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
#'
#' # Calculate PBs for custom distances (e.g., 400m, 800m, 1500m for track)
#' track_pbs <- calculate_pbs(
#'   activities_data = activities,
#'   export_dir = "strava_export_data",
#'   activity_type = "Run",
#'   distances_m = c(400, 800, 1500, 3000) # Custom distances in meters
#' )
#' }
calculate_pbs <- function(activities_data,
                          export_dir = "strava_export_data",
                          activity_type = "Run",
                          start_date = NULL,
                          end_date = Sys.Date(),
                          distances_m = c(1000, 5000, 10000, 21097.5, 42195),
                          verbose = FALSE) {
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

  is_zip_export <- is.character(export_dir) &&
    length(export_dir) == 1 &&
    file.exists(export_dir) &&
    tolower(tools::file_ext(export_dir)) == "zip"
  is_dir_export <- is.character(export_dir) &&
    length(export_dir) == 1 &&
    dir.exists(export_dir)

  if (!is_zip_export && !is_dir_export) {
    stop("`export_dir` must be an existing directory or a .zip file: ", export_dir)
  }

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  # --- Date Handling ---
  analysis_end_date <- parse_analysis_date(end_date, default = Sys.Date(), arg_name = "end_date")
  analysis_start_date <- parse_analysis_date(
    start_date,
    default = analysis_end_date - lubridate::days(365),
    arg_name = "start_date"
  )

  verbose_on <- isTRUE(verbose) || athlytics_is_verbose()

  athlytics_message(sprintf("Calculating PBs from %s to %s.", analysis_start_date, analysis_end_date), .verbose = verbose_on)
  athlytics_message(sprintf("Target distances: %s", paste(distances_m, "m", collapse = ", ")), .verbose = verbose_on)

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

  athlytics_message(sprintf("Analyzing %d activities for PBs...", nrow(filtered_activities)), .verbose = verbose_on)

  # --- Calculate Best Efforts for Each Activity ---
  all_efforts <- lapply(1:nrow(filtered_activities), function(i) {
    activity <- filtered_activities[i, ]

    athlytics_message(sprintf(
      "Processing activity %d/%d: %s (%s)",
      i, nrow(filtered_activities), activity$name, activity$date
    ), .verbose = verbose_on)

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
    efforts <- lapply(distances_m, function(target_distance) {
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
    }) |> dplyr::bind_rows()

    return(efforts)
  }) |> dplyr::bind_rows()

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

  # Add distance labels. Build factor levels dynamically from the distances
  # actually present so custom entries (e.g. 3000m, 800m) are preserved rather
  # than silently collapsed to NA by a hardcoded levels whitelist.
  pb_results <- pb_results %>%
    dplyr::mutate(
      distance_label = dplyr::case_when(
        .data$distance == 1000 ~ "1k",
        .data$distance == 5000 ~ "5k",
        .data$distance == 10000 ~ "10k",
        .data$distance == 21097.5 ~ "21.1k",
        .data$distance == 42195 ~ "Marathon",
        TRUE ~ paste0(round(.data$distance), "m")
      ),
      time_period = as.character(lubridate::seconds_to_period(.data$time_seconds))
    )

  label_levels <- pb_results %>%
    dplyr::distinct(.data$distance, .data$distance_label) %>%
    dplyr::arrange(.data$distance) %>%
    dplyr::pull(.data$distance_label)

  pb_results$distance_label <- factor(
    pb_results$distance_label,
    levels = as.character(label_levels)
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

  athlytics_message(sprintf(
    "PB analysis complete. Found %d efforts, %d are new PBs.",
    nrow(pb_results), sum(pb_results$is_pb)
  ), .verbose = verbose_on)

  # Add parameters as attributes
  attr(pb_results, "params") <- list(
    activity_type = activity_type,
    distances_m = distances_m
  )

  # Add S3 class for type identification
  class(pb_results) <- c("athlytics_pbs", class(pb_results))
  return(pb_results)
}

#' Find Best Effort for Target Distance
#'
#' Locates the fastest contiguous sub-segment that covers `target_distance`
#' within a stream. Two robustness improvements over the original
#' nearest-row approach:
#'
#' 1. **Strictly-monotonic-distance filter**: the input is reduced to rows
#'    where cumulative distance and time both strictly increase, so spurious
#'    backward jumps (GPS glitches, laps restarting the distance counter) no
#'    longer contribute fake short intervals.
#' 2. **Linear interpolation at the target distance**: the start and end
#'    points of each candidate window are interpolated between recorded
#'    samples, eliminating a systematic bias toward slower efforts that
#'    nearest-row lookup introduced on low-Hz streams (a ~0.1 Hz Garmin
#'    smart-recording sample could previously inflate a 5 km time by up to
#'    ~10 s on each side).
#' 3. **O(n) two-pointer sweep**: replaces the previous O(n^2) scan across
#'    start indices with a single left-to-right pass, making per-activity
#'    PB extraction tractable on multi-hour ultras.
#'
#' @keywords internal
find_best_effort <- function(stream_data, target_distance) {
  if (!is.data.frame(stream_data) ||
    !all(c("distance", "time") %in% colnames(stream_data))) {
    return(NULL)
  }

  # Drop rows with missing distance/time and coerce time to POSIXct-numeric
  # seconds for arithmetic.
  valid <- !is.na(stream_data$distance) & !is.na(stream_data$time)
  d <- as.numeric(stream_data$distance[valid])
  t <- as.numeric(stream_data$time[valid])

  if (length(d) < 10) {
    return(NULL)
  }

  # Sort by time (not assumed to already be sorted), then enforce strict
  # monotonicity of distance. This rejects GPS bounce-backs, lap resets and
  # paused-then-resumed intervals that would otherwise create pseudo-segments
  # of near-zero elapsed time.
  ord <- order(t)
  t <- t[ord]
  d <- d[ord]

  keep <- rep(TRUE, length(d))
  if (length(d) >= 2) {
    running_max <- d[1]
    running_t <- t[1]
    for (i in 2:length(d)) {
      if (d[i] > running_max && t[i] > running_t) {
        running_max <- d[i]
        running_t <- t[i]
      } else {
        keep[i] <- FALSE
      }
    }
  }
  d <- d[keep]
  t <- t[keep]

  n <- length(d)
  if (n < 10) {
    return(NULL)
  }

  # Total distance must exceed target.
  if ((d[n] - d[1]) < target_distance) {
    return(NULL)
  }

  # Linear interpolation helper: given sample values (x, y) and a target x0
  # strictly inside [x[j-1], x[j]], return y(x0).
  interp <- function(x0, x_lo, x_hi, y_lo, y_hi) {
    if (x_hi == x_lo) {
      return(y_lo)
    }
    y_lo + (y_hi - y_lo) * (x0 - x_lo) / (x_hi - x_lo)
  }

  # Two-pointer sweep:
  #   i is the start index; d_start(i) is the interpolated distance at the
  #   i-th anchor; for each i we advance j until d[j] >= d[i] + target_distance
  #   and interpolate the exact crossing. Because both d and t are strictly
  #   increasing, j never decreases as i advances.
  best_time <- Inf
  best_start_d <- NA_real_
  best_end_d <- NA_real_

  j <- 1L
  for (i in seq_len(n - 1)) {
    target_d <- d[i] + target_distance

    if (j <= i) j <- i + 1L
    while (j <= n && d[j] < target_d) j <- j + 1L
    if (j > n) break

    # Interpolate end-time at the exact target_d crossing between (d[j-1], d[j]).
    t_end <- interp(target_d, d[j - 1L], d[j], t[j - 1L], t[j])
    elapsed <- t_end - t[i]

    if (is.finite(elapsed) && elapsed > 0 && elapsed < best_time) {
      best_time <- elapsed
      best_start_d <- d[i]
      best_end_d <- target_d
    }
  }

  if (!is.finite(best_time)) {
    return(NULL)
  }

  list(
    time_seconds = best_time,
    start_distance = best_start_d,
    end_distance = best_end_d
  )
}
