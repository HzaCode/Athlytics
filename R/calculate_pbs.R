# R/calculate_pbs.R

#' Calculate Personal Bests (PBs) from Local Strava Data
#'
#' Tracks personal best times for standard distances (1k, 5k, 10k, half marathon, marathon)
#' by analyzing detailed activity files from Strava export data.
#'
#' @param activities_data A data frame of activities from `load_local_activities()`.
#'   Must contain columns: date, type, filename, distance.
#'   Optional `id`, `name`, and `start_date_local` columns are used when
#'   available; otherwise row number, filename, and date fallbacks are used.
#' @param export_dir Path to a Strava export directory or ZIP file containing
#'   the activities folder. Default is "strava_export_data".
#' @param activity_type Type of activities to analyze (typically "Run"). Default "Run".
#' @param start_date Optional start date for analysis (YYYY-MM-DD). If NULL, defaults to 365 days before `end_date`.
#' @param end_date End date for analysis (YYYY-MM-DD). Default `Sys.Date()` (today).
#' @param distances_m Target distances in meters to track.
#'   Default: c(1000, 5000, 10000, 21097.5, 42195) for 1k, 5k, 10k, half, full marathon.
#' @param verbose Logical. If TRUE, prints progress messages. Default FALSE.
#'
#' @return A data frame with columns: `activity_id`, `activity_date`,
#'   `distance`, `elapsed_time`, `moving_time`, `time_seconds`,
#'   `cumulative_pb_seconds`, `is_pb`, `distance_label`, `time_period`,
#'   and `time_basis` (always `"moving"` in the current implementation;
#'   see **PB time semantics** below).
#'
#' @section PB time semantics:
#' `find_best_effort()` selects the fastest interval whose cumulative
#' *distance* increases strictly monotonically. It builds a compressed
#' moving-time axis from those increasing-distance samples, so samples where
#' the distance counter plateaus (traffic stops, laps pausing the watch,
#' signal dropouts) are excluded from the candidate window and from the
#' reported duration. That makes the reported times *moving-time* best
#' efforts rather than elapsed-time best efforts.
#'
#' The authoritative field is `time_basis`, which is hard-coded to
#' `"moving"` in the current implementation. For backward compatibility
#' with earlier releases the output still exposes two columns:
#' `elapsed_time` and `moving_time`. Both are populated with the same
#' numeric `time_seconds` value — they are *compatibility columns*, not
#' two independently-computed quantities. Filter on `time_basis` rather
#' than relying on `elapsed_time != moving_time` to tell the two apart,
#' because the current implementation never produces that difference.
#'
#' If you need an elapsed-time PB (i.e. including paused seconds), use the
#' raw stream with a separate tool; the current implementation intentionally
#' does not attempt to reconstruct paused segments from FIT laps.
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
      time_period = character(0),
      time_basis = character(0)
    ))
  }

  athlytics_message(sprintf("Analyzing %d activities for PBs...", nrow(filtered_activities)), .verbose = verbose_on)

  # --- Calculate Best Efforts for Each Activity ---
  all_efforts <- lapply(1:nrow(filtered_activities), function(i) {
    activity <- filtered_activities[i, ]
    activity_id <- if ("id" %in% names(activity) && !is.na(activity$id)) activity$id else i
    activity_name <- if ("name" %in% names(activity) &&
      !is.na(activity$name) &&
      nzchar(as.character(activity$name))) {
      as.character(activity$name)
    } else {
      as.character(activity$filename)
    }
    activity_date <- if ("start_date_local" %in% names(activity) &&
      !is.na(activity$start_date_local)) {
      activity$start_date_local
    } else {
      activity$date
    }

    athlytics_message(sprintf(
      "Processing activity %d/%d: %s (%s)",
      i, nrow(filtered_activities), activity_name, activity$date
    ), .verbose = verbose_on)

    # Parse activity file. parse_activity_file() resolves both directory
    # and .zip export_dir values via its zip-aware logic, so we pass
    # activity$filename and export_dir straight through rather than
    # pre-building a path with file.path() (which the zip resolver then
    # has to strip the export_dir prefix from). This matches the call
    # pattern used by calculate_decoupling().
    stream_data <- parse_activity_file(activity$filename, export_dir)

    if (is.null(stream_data) || nrow(stream_data) < 10) {
      warning(sprintf("Insufficient data in file for activity %s", activity_id))
      return(NULL)
    }

    # Check for distance data
    if (!"distance" %in% names(stream_data) || all(is.na(stream_data$distance))) {
      warning(sprintf("No distance data for activity %s", activity_id))
      return(NULL)
    }

    # Calculate best efforts for each target distance
    efforts <- lapply(distances_m, function(target_distance) {
      best_effort <- find_best_effort(stream_data, target_distance)

      if (is.null(best_effort)) {
        return(NULL)
      }

      data.frame(
        activity_id = activity_id,
        activity_date = activity_date,
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
      elapsed_time = numeric(0),
      moving_time = numeric(0),
      time_seconds = numeric(0),
      cumulative_pb_seconds = numeric(0),
      is_pb = logical(0),
      distance_label = character(0),
      time_period = character(0),
      time_basis = character(0)
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

  # Add elapsed_time and moving_time. find_best_effort() operates on the
  # strictly-monotonic-distance subset, which excludes paused/plateau
  # samples, so the interval duration it returns is moving time, not
  # elapsed time. We set both columns to the same value for backward
  # compatibility with early callers, and surface the semantic choice via
  # an explicit `time_basis` column so newer code can filter on it.
  pb_results <- pb_results %>%
    dplyr::mutate(
      elapsed_time = .data$time_seconds,
      moving_time = .data$time_seconds,
      time_basis = "moving"
    )

  # Select final columns
  pb_results <- pb_results %>%
    dplyr::select(dplyr::all_of(c(
      "activity_id", "activity_date", "distance",
      "elapsed_time", "moving_time", "time_seconds",
      "cumulative_pb_seconds", "is_pb",
      "distance_label", "time_period", "time_basis"
    ))) %>%
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
#' 2. **Linear interpolation at both segment boundaries**: candidate windows
#'    may start or end between recorded samples. Interpolating both boundaries
#'    removes nearest-row bias on low-Hz streams and avoids missing
#'    end-anchored fastest efforts whose true start falls between samples.
#' 3. **Bounded candidate sweep**: candidates are generated from recorded
#'    starts and recorded ends shifted back by `target_distance`, avoiding the
#'    previous O(n^2) scan while preserving exact fixed-distance intervals.
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
  # monotonicity of distance. While doing so, build a compressed moving-time
  # axis: intervals where cumulative distance did not increase are not counted
  # toward PB duration. This rejects GPS bounce-backs, lap resets and pause
  # plateaus without converting stopped time into moving-time best efforts.
  ord <- order(t)
  raw_t <- t[ord]
  raw_d <- d[ord]

  keep_d <- numeric(length(raw_d))
  keep_t <- numeric(length(raw_t))
  keep_count <- 1L
  keep_d[1] <- raw_d[1]
  keep_t[1] <- 0

  running_max <- raw_d[1]
  last_counted_time <- raw_t[1]
  moving_time <- 0
  saw_bounce_since_last_gain <- FALSE

  if (length(raw_d) >= 2) {
    for (idx in 2:length(raw_d)) {
      current_time <- raw_t[idx]
      interval_seconds <- current_time - last_counted_time

      if (is.finite(interval_seconds) &&
        interval_seconds > 0 &&
        raw_d[idx] > running_max) {
        moving_time <- moving_time + interval_seconds
        running_max <- raw_d[idx]
        keep_count <- keep_count + 1L
        keep_d[keep_count] <- running_max
        keep_t[keep_count] <- moving_time
        last_counted_time <- current_time
        saw_bounce_since_last_gain <- FALSE
      } else if (is.finite(interval_seconds) &&
        interval_seconds > 0 &&
        raw_d[idx] == running_max) {
        # True distance plateaus are pause-like for moving-time PBs. GPS
        # bounce-backs below the running max are not treated as pauses; if the
        # cumulative distance later resumes above the prior max, the intervening
        # time remains counted rather than creating a fake short effort.
        if (!saw_bounce_since_last_gain) {
          last_counted_time <- current_time
        }
      } else if (is.finite(interval_seconds) &&
        interval_seconds > 0 &&
        raw_d[idx] < running_max) {
        saw_bounce_since_last_gain <- TRUE
      }
    }
  }

  d <- keep_d[seq_len(keep_count)]
  t <- keep_t[seq_len(keep_count)]

  n <- length(d)
  if (n < 10) {
    return(NULL)
  }

  # Total distance must exceed target.
  if ((d[n] - d[1]) < target_distance) {
    return(NULL)
  }

  # Vectorized linear interpolation helper for the compressed moving-time axis.
  interp_at <- function(x0) {
    out <- rep(NA_real_, length(x0))
    if (length(x0) == 0) {
      return(out)
    }

    at_or_before_start <- x0 <= d[1]
    at_or_after_end <- x0 >= d[n]
    out[at_or_before_start] <- t[1]
    out[at_or_after_end] <- t[n]

    mid <- is.na(out)
    if (any(mid)) {
      x_mid <- x0[mid]
      idx <- findInterval(x_mid, d)
      idx <- pmax(1L, pmin(idx, n - 1L))

      mid_out <- numeric(length(x_mid))
      exact <- d[idx] == x_mid
      if (any(exact)) {
        mid_out[exact] <- t[idx[exact]]
      }
      if (any(!exact)) {
        k <- idx[!exact]
        mid_out[!exact] <- t[k] +
          (t[k + 1L] - t[k]) * (x_mid[!exact] - d[k]) / (d[k + 1L] - d[k])
      }

      out[mid] <- mid_out
    }

    out
  }

  max_start_d <- d[n] - target_distance
  eps <- sqrt(.Machine$double.eps) * max(1, abs(c(d, target_distance)), na.rm = TRUE)

  candidate_starts <- sort(unique(c(
    d[d <= max_start_d + eps],
    d[d >= d[1] + target_distance - eps] - target_distance
  )))
  candidate_starts <- candidate_starts[
    is.finite(candidate_starts) &
      candidate_starts >= d[1] - eps &
      candidate_starts <= max_start_d + eps
  ]
  candidate_starts <- sort(unique(pmin(pmax(candidate_starts, d[1]), max_start_d)))

  if (length(candidate_starts) == 0) {
    return(NULL)
  }

  t_start <- interp_at(candidate_starts)
  candidate_ends <- candidate_starts + target_distance
  t_end <- interp_at(candidate_ends)
  elapsed <- t_end - t_start

  valid_candidates <- is.finite(elapsed) & elapsed > 0
  if (!any(valid_candidates)) {
    return(NULL)
  }

  valid_indices <- which(valid_candidates)
  best_index <- valid_indices[which.min(elapsed[valid_candidates])]

  list(
    time_seconds = elapsed[best_index],
    start_distance = candidate_starts[best_index],
    end_distance = candidate_ends[best_index]
  )
}
