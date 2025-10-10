# R/load_local_activities.R

#' Load Activities from Local Strava Export
#'
#' Reads and processes activity data from a local Strava export, supporting both
#' direct CSV files and compressed ZIP archives. This function converts Strava export
#' data to a format compatible with all Athlytics analysis functions.
#' Designed to work with Strava's official bulk data export
#' (Settings > My Account > Download or Delete Your Account > Get Started).
#'
#' @param path Path to activities.csv file OR a .zip archive from Strava export.
#'   Supports both CSV and ZIP formats. If a .zip file is provided, the function
#'   will automatically extract and read the activities.csv file from within the archive.
#'   Default is "strava_export_data/activities.csv".
#' @param start_date Optional. Start date (YYYY-MM-DD or Date/POSIXct) for filtering activities.
#'   Defaults to NULL (no filtering).
#' @param end_date Optional. End date (YYYY-MM-DD or Date/POSIXct) for filtering activities.
#'   Defaults to NULL (no filtering).
#' @param activity_types Optional. Character vector of activity types to include
#'   (e.g., c("Run", "Ride")). Defaults to NULL (include all types).
#'
#' @return A tibble of activity data with standardized column names compatible
#'   with Athlytics functions. Key columns include:
#'   \itemize{
#'     \item \code{id}: Activity ID (numeric)
#'     \item \code{name}: Activity name
#'     \item \code{type}: Activity type (Run, Ride, etc.)
#'     \item \code{start_date_local}: Activity start datetime (POSIXct)
#'     \item \code{date}: Activity date (Date)
#'     \item \code{distance}: Distance in meters (numeric)
#'     \item \code{moving_time}: Moving time in seconds (integer)
#'     \item \code{elapsed_time}: Elapsed time in seconds (integer)
#'     \item \code{average_heartrate}: Average heart rate (numeric)
#'     \item \code{average_watts}: Average power in watts (numeric)
#'     \item \code{elevation_gain}: Elevation gain in meters (numeric)
#'   }
#'
#' @details
#' This function reads the activities.csv file from a Strava data export
#' and transforms the data to match the structure expected by Athlytics
#' analysis functions. The transformation includes:
#' \itemize{
#'   \item Converting column names to match API format
#'   \item Parsing dates into POSIXct format
#'   \item Converting distances to meters
#'   \item Converting times to seconds
#'   \item Filtering by date range and activity type if specified
#' }
#'
#' **Privacy Note**: This function processes local export data only and does not
#' connect to the internet. Ensure you have permission to analyze the data and
#' follow applicable privacy regulations when using this data for research purposes.
#'
#' @examples
#' \dontrun{
#' # Load all activities from local CSV
#' activities <- load_local_activities("strava_export_data/activities.csv")
#'
#' # Load directly from ZIP archive (no need to extract manually!)
#' activities <- load_local_activities("export_12345678.zip")
#'
#' # Load only running activities from 2023
#' activities <- load_local_activities(
#'   path = "export_12345678.zip",
#'   start_date = "2023-01-01",
#'   end_date = "2023-12-31",
#'   activity_types = "Run"
#' )
#'
#' # Use with Athlytics functions
#' acwr_data <- calculate_acwr(activities, load_metric = "distance_km")
#' plot_acwr(acwr_data, highlight_zones = TRUE)
#' 
#' # Multi-metric analysis
#' ef_data <- calculate_ef(activities, ef_metric = "Pace_HR")
#' plot_ef(ef_data, add_trend_line = TRUE)
#' }
#'
#' @importFrom dplyr tibble mutate select filter arrange %>% rename
#' @importFrom lubridate parse_date_time as_datetime as_date
#' @importFrom readr read_csv cols
#' @importFrom rlang .data
#' @export
load_local_activities <- function(path = "strava_export_data/activities.csv",
                                 start_date = NULL,
                                 end_date = NULL,
                                 activity_types = NULL) {
  
  # --- Input Validation ---
  if (!file.exists(path)) {
    stop("File not found at: ", path,
         "\nPlease ensure you have downloaded your Strava data export.")
  }
  
  # --- Handle ZIP files ---
  temp_extracted <- FALSE
  original_path <- path
  
  # Check if input is a ZIP file
  if (tolower(tools::file_ext(path)) == "zip") {
    message("Detected ZIP archive. Extracting activities.csv...")
    
    # Create a temporary directory for extraction
    temp_dir <- tempdir()
    
    # List files in the ZIP
    zip_contents <- utils::unzip(path, list = TRUE)
    
    # Find activities.csv (case-insensitive)
    activities_file <- grep("activities\\.csv$", zip_contents$Name, 
                           ignore.case = TRUE, value = TRUE)
    
    if (length(activities_file) == 0) {
      stop("No activities.csv file found in ZIP archive: ", path)
    }
    
    if (length(activities_file) > 1) {
      warning("Multiple activities.csv files found. Using: ", activities_file[1])
      activities_file <- activities_file[1]
    }
    
    # Extract the activities.csv file
    tryCatch({
      utils::unzip(path, files = activities_file, exdir = temp_dir, overwrite = TRUE)
      path <- file.path(temp_dir, activities_file)
      temp_extracted <- TRUE
      message("Successfully extracted to temporary location.")
    }, error = function(e) {
      stop("Failed to extract ZIP archive: ", e$message)
    })
  }
  
  # --- Read CSV ---
  message("Reading activities from: ", basename(original_path))
  
  # Read CSV with appropriate column types
  activities_raw <- tryCatch({
    readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols())
  }, error = function(e) {
    stop("Failed to read CSV file: ", e$message)
  })
  
  if (nrow(activities_raw) == 0) {
    warning("No activities found in CSV file.")
    return(dplyr::tibble(
      id = numeric(0),
      name = character(0),
      type = character(0),
      start_date_local = lubridate::as_datetime(character(0)),
      date = lubridate::as_date(character(0)),
      distance = numeric(0),
      moving_time = integer(0),
      elapsed_time = integer(0),
      average_heartrate = numeric(0),
      average_watts = numeric(0),
      elevation_gain = numeric(0)
    ))
  }
  
  message("Found ", nrow(activities_raw), " activities in CSV file.")
  
  # --- Transform Data ---
  activities_df <- activities_raw %>%
    dplyr::mutate(
      # ID
      id = as.numeric(.data$`Activity ID`),
      
      # Name and Type
      name = as.character(.data$`Activity Name`),
      type = as.character(.data$`Activity Type`),
      sport_type = as.character(.data$`Activity Type`), # Strava export doesn't distinguish
      
      # Parse Date - Strava format: "Feb 17, 2022, 12:18:26 PM"
      start_date_local = lubridate::parse_date_time(
        .data$`Activity Date`,
        orders = c("b d, Y, I:M:S p", "mdy HMS p", "ymd HMS"),
        tz = "UTC"  # Will be converted to local if needed
      ),
      date = lubridate::as_date(.data$start_date_local),
      
      # Distance - CSV has two "Distance" columns, use the second (more detailed)
      # R renames to Distance...7 and Distance...18
      distance = as.numeric(.data$`Distance...18`),
      
      # Times - CSV shows seconds, has duplicate columns
      # Elapsed Time appears at positions 6 and 16 -> Elapsed Time...6 and Elapsed Time...16
      moving_time = as.integer(.data$`Moving Time`),
      elapsed_time = as.integer(.data$`Elapsed Time...16`),
      
      # Heart Rate - Max Heart Rate appears at positions 8 and 31
      average_heartrate = as.numeric(.data$`Average Heart Rate`),
      max_heartrate = as.numeric(.data$`Max Heart Rate...31`),
      
      # Power
      average_watts = as.numeric(.data$`Average Watts`),
      max_watts = as.numeric(.data$`Max Watts`),
      weighted_average_watts = as.numeric(.data$`Weighted Average Power`),
      
      # Elevation
      elevation_gain = as.numeric(.data$`Elevation Gain`),
      elevation_loss = as.numeric(.data$`Elevation Loss`),
      
      # Speed
      average_speed = as.numeric(.data$`Average Speed`),
      max_speed = as.numeric(.data$`Max Speed`),
      
      # Other useful metrics
      calories = as.numeric(.data$Calories),
      relative_effort = as.numeric(.data$`Relative Effort...38`),  # Position 38 is the detailed one
      
      # File path for detailed activity data (for decoupling, pbs analysis)
      filename = as.character(.data$Filename)
    )
  
  # --- Select Key Columns ---
  # Keep columns that are commonly used in Athlytics functions
  key_columns <- c(
    "id", "name", "type", "sport_type",
    "start_date_local", "date",
    "distance", "moving_time", "elapsed_time",
    "average_heartrate", "max_heartrate",
    "average_watts", "max_watts", "weighted_average_watts",
    "elevation_gain", "elevation_loss",
    "average_speed", "max_speed",
    "calories", "relative_effort",
    "filename"
  )
  
  # Select only existing columns
  available_cols <- key_columns[key_columns %in% names(activities_df)]
  activities_df <- activities_df %>%
    dplyr::select(dplyr::all_of(available_cols))
  
  # --- Apply Filters ---
  
  # Filter by date range
  if (!is.null(start_date)) {
    start_date_parsed <- tryCatch({
      lubridate::as_datetime(start_date)
    }, error = function(e) {
      stop("Could not parse start_date. Please use YYYY-MM-DD format or a Date/POSIXct object.")
    })
    
    activities_df <- activities_df %>%
      dplyr::filter(.data$start_date_local >= start_date_parsed)
    
    message("Filtered to activities after ", start_date_parsed)
  }
  
  if (!is.null(end_date)) {
    end_date_parsed <- tryCatch({
      dt <- lubridate::as_datetime(end_date)
      # If time is 00:00:00, extend to end of day
      if (format(dt, "%H:%M:%S") == "00:00:00") {
        dt <- dt + lubridate::hours(23) + lubridate::minutes(59) + lubridate::seconds(59)
      }
      dt
    }, error = function(e) {
      stop("Could not parse end_date. Please use YYYY-MM-DD format or a Date/POSIXct object.")
    })
    
    activities_df <- activities_df %>%
      dplyr::filter(.data$start_date_local <= end_date_parsed)
    
    message("Filtered to activities before ", end_date_parsed)
  }
  
  # Filter by activity type
  if (!is.null(activity_types)) {
    if (!is.character(activity_types)) {
      stop("activity_types must be a character vector (e.g., c('Run', 'Ride'))")
    }
    
    activities_df <- activities_df %>%
      dplyr::filter(.data$type %in% activity_types)
    
    message("Filtered to activity types: ", paste(activity_types, collapse = ", "))
  }
  
  # --- Final Sorting ---
  activities_df <- activities_df %>%
    dplyr::arrange(.data$start_date_local)
  
  message("Data loading complete. ", nrow(activities_df), " activities after filtering.")
  
  return(activities_df)
}


