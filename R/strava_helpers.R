# R/strava_helpers.R

#' (Internal) Fetch and Compile Strava Activities
#'
#' Helper function to get Strava activity data within a date range.
#'
#' Retrieves a summary list of activities and optionally fetches detailed
#' data for each activity if needed for specific metrics (e.g., power).
#' Intended for internal use within the package.
#'
#' @param stoken A valid Strava token from `rStrava::strava_oauth()`.
#' @param start_date Optional. Start date (YYYY-MM-DD or Date/POSIXct) for fetching. Defaults to ~2009.
#' @param end_date Optional. End date (YYYY-MM-DD or Date/POSIXct) for fetching. Defaults to now.
#' @param fetch_details Fetch detailed data for each activity using `get_activity()`?
#'   Needed for power/HR metrics but much slower and uses more API calls. Default `FALSE`.
#' @param required_cols If `fetch_details = TRUE`, which detailed columns to attempt
#'   to extract (e.g., `c("average_watts", "average_heartrate")`).
#' @param delay Seconds to pause between `get_activity` calls when `fetch_details = TRUE`
#'   to manage API rate limits. Default 1.
#'
#' @return A tibble of activity data. Columns vary based on `fetch_details`.
#'   Includes a `date` column derived from `start_date_local`.
#'
#' @details This function wraps `rStrava::get_activity_list` and
#'   `rStrava::compile_activities`. If `fetch_details` is enabled, it iteratively
#'   calls `rStrava::get_activity` for each activity, which is the main performance
#'   bottleneck and API usage driver.
#'
#' @importFrom rStrava get_activity_list compile_activities get_activity
#' @importFrom dplyr tibble mutate select left_join %>% rowwise ungroup relocate all_of starts_with bind_rows
#' @importFrom lubridate as_datetime ymd_hms is.POSIXct is.Date as_date force_tz with_tz now
#' @importFrom purrr map map_dfr possibly pluck
#' @importFrom rlang .data := 
#' @keywords internal
fetch_strava_activities <- function(stoken,
                                    start_date = NULL,
                                    end_date = NULL,
                                    fetch_details = FALSE,
                                    required_cols = c("average_watts", "average_heartrate"),
                                    delay = 1) {

  # --- Input Validation and Date Handling ---
  if (!inherits(stoken, "Token2.0")) {
    stop("`stoken` must be a valid Token2.0 object from rStrava::strava_oauth().")
  }

  # Default start date (Strava API likely uses Unix epoch or similar if null)
  # Setting a practical default far back, but warn user if not specified
  if (is.null(start_date)) {
      # Strava was founded in 2009
      start_date_ts <- as.numeric(lubridate::as_datetime("2009-01-01 00:00:00", tz = "UTC"))
      # message("No start_date provided, attempting to fetch all activities since 2009. This may be very slow.")
  } else {
      # Parse start_date and explicitly check for NA
      start_date_parsed <- suppressWarnings(tryCatch({
          lubridate::as_datetime(start_date)
      }, error = function(e) NA )) # Return NA on error too
      
      if(is.na(start_date_parsed)){
          stop("Could not parse start_date. Please use YYYY-MM-DD format or a Date/POSIXct object.")
      }
      start_date <- lubridate::with_tz(start_date_parsed, "UTC")
      start_date_ts <- as.numeric(start_date)
  }


  # Default end date to now
  if (is.null(end_date)) {
      end_date_ts <- as.numeric(lubridate::now(tzone = "UTC"))
  } else {
      # Parse end_date and explicitly check for NA
      end_date_parsed <- suppressWarnings(tryCatch({
          dt <- lubridate::as_datetime(end_date)
          if(format(dt, "%H:%M:%S") == "00:00:00"){
             dt <- lubridate::ymd_hms(paste(lubridate::as_date(dt), "23:59:59"), tz = lubridate::tz(dt))
          }
          dt
      }, error = function(e) NA )) # Return NA on error too
      
      if(is.na(end_date_parsed)){
          stop("Could not parse end_date. Please use YYYY-MM-DD format or a Date/POSIXct object.")
      }
      end_date <- lubridate::with_tz(end_date_parsed, "UTC")
      end_date_ts <- as.numeric(end_date)
  }

  # --- Fetch Activity List ---
  message("Fetching activity list from Strava...")
  activity_list <- tryCatch({
      # Need to ensure stoken is passed correctly if not using default httr config
      rStrava::get_activity_list(stoken = stoken, before = end_date_ts, after = start_date_ts)
      # Note: get_activity_list handles pagination internally up to a limit (check rStrava docs/src)
      # If more control is needed, might need to use get_pages directly.
  }, error = function(e) {
      stop("Failed to fetch activity list from Strava: ", e$message)
  })

  if (length(activity_list) == 0) {
    message("No activities found in the specified date range.")
    # Return an empty tibble with expected basic columns from compile_activities
    # Need to know the exact columns compile_activities produces or handle this robustly
    # Return minimal structure
     return(dplyr::tibble(
        id = integer(0),
        name = character(0),
        type = character(0),
        sport_type = character(0),
        start_date = lubridate::as_datetime(character(0)),
        start_date_local = lubridate::as_datetime(character(0)),
        date = lubridate::as_date(character(0)), # Added date column
        distance = numeric(0),
        moving_time = integer(0),
        elapsed_time = integer(0)
        # Add other base columns if known
     ))
  }

  message("Compiling base activity data...")
  activities_base_df <- tryCatch({
      # Pass stoken? compile_activities doesn't seem to need it based on docs
      rStrava::compile_activities(activity_list)
  }, error = function(e) {
      stop("Failed to compile activity list into data frame: ", e$message)
  })
  
  # Check required columns after compile_activities
  # Check types after compile_activities
   if (nrow(activities_base_df) > 0) {
       activities_base_df <- activities_base_df %>% 
           dplyr::mutate(
               # Convert start_date_local to datetime
               start_date_local = lubridate::as_datetime(.data$start_date_local), 
               # start_date might also be useful
               start_date = lubridate::as_datetime(.data$start_date), 
               # Create a simple Date column for daily grouping
               date = lubridate::as_date(.data$start_date_local), 
               distance = as.numeric(.data$distance), 
               moving_time = as.integer(.data$moving_time), 
               elapsed_time = as.integer(.data$elapsed_time)
           ) 
   } else {
        # If compile_activities somehow returned an empty df, ensure base columns exist
        base_cols <- c("id", "name", "type", "sport_type", "start_date", "start_date_local", "date", "distance", "moving_time", "elapsed_time")
        for(col in base_cols){ 
            if(!col %in% names(activities_base_df)){ 
                # Assign correct empty type
                activities_base_df[[col]] <- switch(col,
                    id = integer(0), 
                    name = character(0),
                    type = character(0),
                    sport_type = character(0),
                    start_date = lubridate::as_datetime(character(0)),
                    start_date_local = lubridate::as_datetime(character(0)),
                    date = lubridate::as_date(character(0)),
                    distance = numeric(0),
                    moving_time = integer(0),
                    elapsed_time = integer(0),
                    vector() # Default case
                )
            }
        }
   }

  # --- Fetch Detailed Data (if requested) ---
  if (fetch_details && nrow(activities_base_df) > 0) {
      message(paste("Fetching details for", nrow(activities_base_df), "activities... (This may take time)"))

      # Define a safe version of get_activity that returns NULL on error
      # get_activity needs act_data (compiled list), stoken, and id
      safe_get_activity <- purrr::possibly(rStrava::get_activity, otherwise = NULL, quiet = FALSE) # Set quiet=FALSE to see errors from get_activity

      # Define a function to process one activity
      process_activity_details <- function(activity_id, act_data, stoken, delay, required_cols) {
          # Small delay before each call
          Sys.sleep(delay) 
          
          details_list <- safe_get_activity(act_data = act_data, stoken = stoken, id = activity_id)

          if (is.null(details_list)) {
              warning("Failed to fetch details for activity ID: ", activity_id, call. = FALSE)
              # Create a tibble with NA for required columns for this ID
              res <- dplyr::tibble(id = activity_id) 
              for (col in required_cols) { 
                  res[[col]] <- NA 
              }
              return(res)
          }

          # Extract required columns safely using pluck, provide NA default
          # Use names() on the list to see available fields if needed
          extracted_data <- purrr::map(required_cols, ~purrr::pluck(details_list, .x, .default = NA))
          names(extracted_data) <- required_cols
          
          # Combine ID with extracted data
          dplyr::tibble(id = activity_id, !!!extracted_data)
      }

      # Apply the function to each activity ID using map_dfr
      detailed_data_list <- purrr::map(
            activities_base_df$id,
            ~process_activity_details(
                activity_id = .x,
                # Pass the original compiled data frame as act_data? Check get_activity docs
                # It seems get_activity uses act_data just to find the id, might not be strictly needed if we pass id directly? 
                # Pass for safety 
                act_data = activities_base_df, 
                stoken = stoken,
                delay = delay,
                required_cols = required_cols
            )
       )
       
      # Combine list of tibbles into a single tibble
      detailed_data <- dplyr::bind_rows(detailed_data_list)

      # Add missing required columns
      for (col in required_cols) {
            if (!col %in% names(detailed_data)) {
                # Assign appropriate NA type
                detailed_data[[col]] <- NA
           }
       }

      # Join detailed data back to the base data frame
      # Match id column types for joining
      activities_base_df$id <- as.numeric(activities_base_df$id)
      detailed_data$id <- as.numeric(detailed_data$id)
      
      activities_final_df <- dplyr::left_join(activities_base_df, detailed_data, by = "id")

  } else {
      # If not fetching details or if no base activities, just use the base data frame
      activities_final_df <- activities_base_df
      # Optionally add NA columns for required_cols if they don't exist
      for (col in required_cols) {
          if (!col %in% names(activities_final_df)) {
              # Assign appropriate NA type
              activities_final_df[[col]] <- NA_real_ # Assume numeric details for now
          }
      }
  }

  # Final cleanup and ordering if the dataframe is not empty
  if(nrow(activities_final_df) > 0) {
      # Define expected base cols + required detail cols for selection
      select_cols <- unique(c("id", "name", "type", "sport_type", "start_date_local", "date", "distance", "moving_time", "elapsed_time", required_cols))
      # Check column existence before selecting
      select_cols_exist <- select_cols[select_cols %in% names(activities_final_df)]
      
      activities_final_df <- activities_final_df %>% 
          # Select known/required columns first, then everything else
          dplyr::select(dplyr::all_of(select_cols_exist), dplyr::everything()) %>% 
          dplyr::arrange(.data$start_date_local)
  } 

  message("Strava data fetching and processing complete.")
  return(activities_final_df)
}

 

#' @keywords internal
get_activity_list_stoken_direct <- function(stoken, before = NULL, after = NULL) {

} 
