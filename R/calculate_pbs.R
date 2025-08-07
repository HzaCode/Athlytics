# R/calculate_pbs.R

#' Calculate Personal Bests (PBs)
#'
#' Finds personal best times for specified distances from Strava activities.
#'
#' Fetches detailed activity data, extracts Strava's 'best efforts', and calculates
#' cumulative PBs for specified distances.
#'
#' @param stoken A valid Strava token from `rStrava::strava_oauth()`.
#' @param activity_type Type(s) of activities to search for PBs (e.g., "Run").
#'   Note: Current logic relies on Strava's `best_efforts`, primarily available for Runs.
#' @param distance_meters Numeric vector of distances (in meters) to find PBs for (e.g., `c(1000, 5000, 10000)`).
#' @param max_activities Maximum number of recent activities to check. Default 500.
#'   Reducing this can speed up the process and help avoid API rate limits.
#' @param date_range Optional. Filter activities within a date range `c("YYYY-MM-DD", "YYYY-MM-DD")`.
#'
#' @return A data frame containing all found best efforts for the specified distances.
#'   Includes columns: `activity_id`, `activity_date`, `distance`, `time_seconds` (elapsed time),
#'   `cumulative_pb_seconds` (the PB for that distance as of that date), `is_pb` (TRUE if this effort set a new PB),
#'   `distance_label` (e.g., "5k"), and `time_period` (formatted time).
#'
#' @details Provides data for `plot_pbs`. Processes activities chronologically.
#'   Fetching detailed data is slow due to API limits (includes 1s delay per activity).
#'
#' @importFrom rStrava get_activity_list get_activity
#' @importFrom dplyr filter select mutate arrange group_by ungroup bind_rows slice_head lag distinct %>%
#' @importFrom purrr map_dfr possibly
#' @importFrom lubridate as_datetime ymd_hms seconds_to_period ymd
#' @importFrom rlang .data %||%
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#'
#' @examples
#' # Example using simulated data
#' data(athlytics_sample_data)
#' print(head(athlytics_sample_pbs))
#'
#' \dontrun{
#' # Example using real data (requires authentication and YOUR credentials)
#' # NOTE: The following rStrava::strava_oauth call is a placeholder.
#' # You MUST replace "YOUR_APP_NAME", "YOUR_CLIENT_ID", "YOUR_CLIENT_SECRET"
#' # with your actual Strava API credentials for this to work.
#' # This is a placeholder and will likely require user interaction or fail
#' # if not properly configured with actual credentials.
#' # For R CMD check, the main thing is that the syntax is valid.
#' tryCatch({
#'   stoken_example <- rStrava::strava_oauth(
#'     app_name = "YOUR_APP_NAME_PLACEHOLDER",
#'     client_id = "YOUR_CLIENT_ID_PLACEHOLDER",
#'     client_secret = "YOUR_CLIENT_SECRET_PLACEHOLDER",
#'     cache = TRUE
#'   )
#'
#'   # Proceed only if a token object is created
#'   if (inherits(stoken_example, "Token2.0")) {
#'     # Shortened message for line width
#'     message("Placeholder stoken obtained. Real data features may not work.")
#'     # Calculate PBs for 1k, 5k (limit activities for speed in example)
#'     pb_data <- calculate_pbs(stoken = stoken_example,
#'                              activity_type = "Run", 
  #'                              # Note: activity_type should match your intended type
#'                              distance_meters = c(1000, 5000, 10000),
#'                              max_activities = 10) # Reduced for example speed
#'     if (nrow(pb_data) > 0) {
#'       print(head(pb_data))
#'       # Show only new PB records
#'       new_pbs <- pb_data[pb_data$is_pb, ]
#'       print(new_pbs)
#'     } else {
#'       message("calculate_pbs example (placeholder token) returned no PB data.")
#'     }
#'   } else {
#'     message("Placeholder stoken creation failed. Check rStrava setup.")
#'   }
#' }, error = function(e) {
#'   # Shortened error messages as well
#'   message("Error in rStrava::strava_oauth() example: ", e$message)
#'   message("This can happen with placeholder credentials.")
#' })
#' }
calculate_pbs <- function(stoken,
                          activity_type = "Run", # Default to Run as per original logic
                          distance_meters,
                          max_activities = 500,
                          date_range = NULL) {

  # --- Input Validation ---
  # Stoken checks first
  if (missing(stoken)) stop("Strava token 'stoken' is required.")
  if (!inherits(stoken, "Token2.0")) {
    stop(paste0("Assertion on 'stoken' failed: Must inherit from class 'Token2.0', but has class '", class(stoken)[1], "'."))
  }

  # Distance meters checks
  if (missing(distance_meters) || !is.numeric(distance_meters) || length(distance_meters) == 0) {
    stop("'distance_meters' must be a numeric vector of distances (e.g., c(1000, 5000)).")
  }
  if (any(distance_meters <= 0)) {
    stop("All 'distance_meters' must be positive.")
  }

  # Other checks/warnings
  if(any(tolower(activity_type) != "run")) {
      warning("Current implementation primarily supports 'Run' for PB calculation based on best_efforts.")
  }


  message("Fetching activity list...")
  # --- 1. Get Activity List ---
  per_page_limit <- 200 # Your original variable
  num_to_fetch <- min(max_activities, per_page_limit) # Your original variable
  activities_list <- tryCatch({
    rStrava::get_activity_list(stoken)
  }, error = function(e) {
    message("Error fetching activity list: ", e$message)
    return(NULL)
  })

  if (is.null(activities_list) || length(activities_list) == 0) {
    stop("Could not fetch activities or no activities found.")
  }

  # Assuming rlang::%||% is imported via @importFrom rlang .data %||%
  # No local %||% definition needed here.

  activities_df <- purrr::map_dfr(activities_list, ~{
    data.frame(
      id = as.character(.x$id %||% NA), # Uses rlang::%||%
      type = .x$type %||% NA,
      start_date_local = .x$start_date_local %||% NA,
      stringsAsFactors = FALSE
    )
  }) %>%
    dplyr::filter(!is.na(.data$id), !is.na(.data$type), !is.na(.data$start_date_local)) %>%
    dplyr::mutate(start_date_local = lubridate::ymd_hms(.data$start_date_local))

  # --- Date Range Filtering ---
  if (!is.null(date_range) && length(date_range) == 2) {
    start_date_filter <- tryCatch(lubridate::ymd(date_range[1]), warning = function(w) NULL)
    end_date_filter <- tryCatch(lubridate::ymd(date_range[2]), warning = function(w) NULL)
    if(!is.null(start_date_filter) && !is.null(end_date_filter)) {
      activities_df <- activities_df %>%
        dplyr::filter(as.Date(.data$start_date_local) >= start_date_filter & as.Date(.data$start_date_local) <= end_date_filter)
      message(sprintf("Filtering activities between %s and %s.", date_range[1], date_range[2]))
    } else {
      warning("Invalid date_range format. Should be c('YYYY-MM-DD', 'YYYY-MM-DD'). Ignoring date filter.")
    }
  }

  # --- 2. Filter by Activity Type ---
  activities_to_process <- activities_df %>%
    dplyr::filter(.data$type %in% activity_type) %>%
    dplyr::select(.data$id, .data$start_date_local) %>%
    dplyr::arrange(.data$start_date_local)

  if (nrow(activities_to_process) == 0) {
    stop(paste0("No activities of type '", paste(activity_type, collapse=", "), "' found",
                if (!is.null(date_range)) " in the specified date range." else "."))
  }
  message(sprintf("Found %d activities of type '%s' to process. Fetching details...", nrow(activities_to_process), paste(activity_type, collapse=", ")))

  # --- Apply max_activities limit ---
  # This section refers to `slice_head`.
  if (nrow(activities_to_process) > max_activities) {
    message(sprintf("Applying max_activities limit: Processing only the earliest %d activities (after sorting by date ascending) out of %d found.",
                    max_activities, nrow(activities_to_process)))
    activities_to_process <- activities_to_process %>%
      dplyr::slice_head(n = max_activities) # Takes first N after current sort (start_date_local ascending)
  }

  # --- 3. & 4. Loop, Get Details, Extract Best Efforts ---
  safe_get_activity <- purrr::possibly(rStrava::get_activity, otherwise = NULL, quiet = FALSE)
  all_best_efforts <- list()
  pb_counter <- 0
  pb <- utils::txtProgressBar(min = 0, max = nrow(activities_to_process), style = 3)

  for (i in 1:nrow(activities_to_process)) {
    act_id <- activities_to_process$id[i]
    act_date <- activities_to_process$start_date_local[i]
    Sys.sleep(1)

    detailed_activity <- safe_get_activity(id = act_id, stoken = stoken)

    if (!is.null(detailed_activity) && !is.null(detailed_activity$best_efforts)) {
      best_efforts_df <- purrr::map_dfr(detailed_activity$best_efforts, ~{
        effort_dist <- as.numeric(.x$distance %||% NA)
        matched_dist <- distance_meters[abs(effort_dist - distance_meters) <= 50]

        if(length(matched_dist) > 0) {
          data.frame(
            activity_id = act_id,
            activity_date = act_date,
            distance = matched_dist[1],
            elapsed_time = as.numeric(.x$elapsed_time %||% NA), # Uses rlang::%||%
            moving_time = as.numeric(.x$moving_time %||% NA), # Uses rlang::%||%
            stringsAsFactors = FALSE
          )
        } else { NULL }
      })
      if (nrow(best_efforts_df) > 0) {
        all_best_efforts[[as.character(act_id)]] <- best_efforts_df
      }
    }
     pb_counter <- pb_counter + 1
     utils::setTxtProgressBar(pb, pb_counter)
  }
  close(pb)
  message("\nFinished fetching details.")

  # --- 5. Process and Aggregate Results ---
  if (length(all_best_efforts) == 0) {
    stop("No best efforts found for the specified distances in the processed activities.")
  }

  pbs_df <- dplyr::bind_rows(all_best_efforts) %>%
    dplyr::filter(!is.na(.data$elapsed_time), !is.na(.data$distance)) %>%
    dplyr::mutate(time_seconds = .data$elapsed_time)

  if (nrow(pbs_df) == 0) {
    stop("No valid best effort times found after processing details.")
  }

  pbs_df <- pbs_df %>%
    dplyr::arrange(.data$distance, .data$activity_date) %>%
    dplyr::group_by(.data$distance) %>%
    dplyr::mutate(
        cumulative_pb_seconds = cummin(.data$time_seconds),
        is_pb = .data$time_seconds == .data$cumulative_pb_seconds & (is.na(dplyr::lag(.data$cumulative_pb_seconds)) | .data$time_seconds < dplyr::lag(.data$cumulative_pb_seconds))
        ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        distance_label = factor(paste0(.data$distance / 1000, "k"),
                               levels = paste0(sort(unique(.data$distance)) / 1000, "k")),
        time_period = lubridate::seconds_to_period(.data$time_seconds)
     )

  message("Calculation complete.")
  return(pbs_df)
}