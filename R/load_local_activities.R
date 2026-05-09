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
#'   - `id`: Activity ID (numeric)
#'   - `name`: Activity name
#'   - `type`: Activity type (Run, Ride, etc.)
#'   - `start_date_local`: Activity start datetime (POSIXct)
#'   - `date`: Activity date (Date)
#'   - `distance`: Distance in meters (numeric)
#'   - `moving_time`: Moving time in seconds (integer)
#'   - `elapsed_time`: Elapsed time in seconds (integer)
#'   - `average_heartrate`: Average heart rate (numeric)
#'   - `average_watts`: Average power in watts (numeric)
#'   - `elevation_gain`: Elevation gain in meters (numeric)
#'
#' @details
#' This function reads the activities.csv file from a Strava data export
#' and transforms the data to match the structure expected by Athlytics
#' analysis functions. The transformation includes:
#' - Standardizing column names for analysis functions
#' - Parsing dates into POSIXct format
#' - Converting distances to meters
#' - Converting times to seconds
#' - Filtering by date range and activity type if specified
#'
#' **Language Note**: Strava export language must be set to **English** for proper
#' CSV parsing. Change this in Strava Settings > Display Preferences > Language
#' before requesting your data export. If `load_local_activities()` reports
#' missing Strava export columns, first check that the export was requested after
#' changing this setting.
#'
#' **Privacy Note**: This function processes local export data only and does not
#' connect to the internet. Ensure you have permission to analyze the data and
#' follow applicable privacy regulations when using this data for research purposes.
#'
#' @examples
#' # Example using built-in sample CSV
#' csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")
#' if (nzchar(csv_path)) {
#'   activities <- load_local_activities(csv_path)
#'   head(activities)
#' }
#'
#' \dontrun{
#' # Load from a local Strava export ZIP archive
#' activities <- load_local_activities("export_12345678.zip")
#'
#' # Filter by date and activity type
#' activities <- load_local_activities(
#'   path = "export_12345678.zip",
#'   start_date = "2023-01-01",
#'   end_date = "2023-12-31",
#'   activity_types = "Run"
#' )
#' }
#'
#' @export
load_local_activities <- function(path = "strava_export_data/activities.csv",
                                  start_date = NULL,
                                  end_date = NULL,
                                  activity_types = NULL) {
  # --- Input Validation ---
  if (!file.exists(path)) {
    stop(
      "File not found at: ", path,
      "\nPlease ensure you have downloaded your Strava data export."
    )
  }

  # --- Handle ZIP files ---
  temp_extracted <- FALSE
  original_path <- path

  # Check if input is a ZIP file
  if (tolower(tools::file_ext(path)) == "zip") {
    athlytics_message("Detected ZIP archive. Extracting activities.csv...")

    # Create a temporary directory for extraction
    temp_dir <- tempdir()

    # List files in the ZIP
    zip_contents <- utils::unzip(path, list = TRUE)

    # Find activities.csv (case-insensitive)
    activities_file <- grep("activities\\.csv$", zip_contents$Name,
      ignore.case = TRUE, value = TRUE
    )

    if (length(activities_file) == 0) {
      stop("No activities.csv file found in ZIP archive: ", path)
    }

    if (length(activities_file) > 1) {
      warning("Multiple activities.csv files found. Using: ", activities_file[1])
      activities_file <- activities_file[1]
    }

    # Extract the activities.csv file
    tryCatch(
      {
        utils::unzip(path, files = activities_file, exdir = temp_dir, overwrite = TRUE)
        path <- file.path(temp_dir, activities_file)
        temp_extracted <- TRUE
        athlytics_message("Successfully extracted to temporary location.")
      },
      error = function(e) {
        stop("Failed to extract ZIP archive: ", e$message)
      }
    )
  }

  # --- Read CSV ---
  athlytics_message("Reading activities from: ", basename(original_path))

  # Read CSV with appropriate column types
  activities_raw <- tryCatch(
    {
      if (requireNamespace("readr", quietly = TRUE)) {
        readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols())
      } else {
        utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      }
    },
    error = function(e) {
      stop("Failed to read CSV file: ", e$message)
    }
  )

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

  athlytics_message("Found ", nrow(activities_raw), " activities in CSV file.")

  # --- Transform Data ---
  # Store column names to avoid using `.` inside mutate
  col_names <- names(activities_raw)

  find_column <- function(aliases) {
    hits <- aliases[aliases %in% col_names]
    if (length(hits) == 0) {
      return(NA_character_)
    }
    hits[1]
  }

  get_column <- function(aliases) {
    column <- find_column(aliases)
    if (is.na(column)) {
      return(rep(NA, nrow(activities_raw)))
    }
    activities_raw[[column]]
  }

  required_columns <- list(
    "Activity ID" = c("Activity ID"),
    "Activity Date" = c("Activity Date"),
    "Activity Type" = c("Activity Type"),
    "Distance" = c("Distance", "Distance.1", "Distance...18"),
    "Elapsed Time" = c("Elapsed Time", "Elapsed.Time.1", "Elapsed Time...16"),
    "Moving Time" = c("Moving Time", "Moving.Time")
  )

  missing_required <- names(required_columns)[
    vapply(required_columns, function(aliases) is.na(find_column(aliases)), logical(1))
  ]

  if (length(missing_required) > 0) {
    stop(
      "activities.csv is missing required Strava export column(s): ",
      paste(missing_required, collapse = ", "),
      "\nAthlytics expects the English Strava bulk-export schema. ",
      "If this is a Strava export, set Strava language to English ",
      "(Settings > Display Preferences > Language) before requesting the export, ",
      "then download a fresh ZIP/CSV.",
      call. = FALSE
    )
  }

  activities_df <- activities_raw %>%
    dplyr::mutate(
      # ID
      id = as.numeric(get_column(c("Activity ID"))),

      # Name and Type
      name = as.character(get_column(c("Activity Name"))),
      type = as.character(get_column(c("Activity Type"))),
      sport_type = as.character(get_column(c("Activity Type"))), # Strava export doesn't distinguish

      # Parse Date - Strava format: "Feb 17, 2022, 12:18:26 PM"
      start_date_local = lubridate::parse_date_time(
        get_column(c("Activity Date")),
        orders = c("b d, Y, I:M:S p", "mdy HMS p", "ymd HMS"),
        tz = "UTC" # Will be converted to local if needed
      ),
      date = lubridate::as_date(.data$start_date_local),

      # Distance - CSV has two "Distance" columns, use the second (more detailed)
      # R renames duplicate columns by appending .1, .2, etc.
      distance = as.numeric(if ("Distance.1" %in% col_names) {
        get_column(c("Distance.1"))
      } else if ("Distance...18" %in% col_names) {
        get_column(c("Distance...18"))
      } else {
        get_column(c("Distance"))
      }),

      # Times - CSV shows seconds, has duplicate columns
      # Handle different possible column names based on how R reads the CSV
      moving_time = as.integer(get_column(c("Moving.Time", "Moving Time"))),
      elapsed_time = as.integer(if ("Elapsed.Time.1" %in% col_names) {
        get_column(c("Elapsed.Time.1"))
      } else if ("Elapsed Time...16" %in% col_names) {
        get_column(c("Elapsed Time...16"))
      } else {
        get_column(c("Elapsed Time"))
      }),

      # Heart Rate - handle duplicate columns
      average_heartrate = as.numeric(get_column(c("Average.Heart.Rate", "Average Heart Rate"))),
      max_heartrate = as.numeric(if ("Max.Heart.Rate.1" %in% col_names) {
        get_column(c("Max.Heart.Rate.1"))
      } else if ("Max Heart Rate...31" %in% col_names) {
        get_column(c("Max Heart Rate...31"))
      } else {
        get_column(c("Max Heart Rate"))
      }),

      # Power
      average_watts = as.numeric(get_column(c("Average.Watts", "Average Watts"))),
      max_watts = as.numeric(get_column(c("Max.Watts", "Max Watts"))),
      weighted_average_watts = as.numeric(get_column(c("Weighted Average Power"))),

      # Elevation
      elevation_gain = as.numeric(get_column(c("Elevation.Gain", "Elevation Gain"))),
      elevation_loss = as.numeric(get_column(c("Elevation.Loss", "Elevation Loss"))),

      # Speed
      average_speed = as.numeric(get_column(c("Average.Speed", "Average Speed"))),
      max_speed = as.numeric(get_column(c("Max.Speed", "Max Speed"))),

      # Grade Adjusted Pace (GAP) - accounts for elevation changes
      # Note: despite the column name "Pace", Strava stores this as speed in m/s
      average_gap = as.numeric(if ("Average.Grade.Adjusted.Pace" %in% col_names) {
        get_column(c("Average.Grade.Adjusted.Pace"))
      } else if ("Average Grade Adjusted Pace" %in% col_names) {
        get_column(c("Average Grade Adjusted Pace"))
      } else {
        NA_real_
      }),

      # Other useful metrics
      calories = as.numeric(get_column(c("Calories"))),
      relative_effort = as.numeric(if ("Relative.Effort.1" %in% col_names) {
        get_column(c("Relative.Effort.1"))
      } else if ("Relative Effort...38" %in% col_names) {
        get_column(c("Relative Effort...38"))
      } else {
        get_column(c("Relative Effort"))
      }),

      # File path for detailed activity data (for decoupling, pbs analysis)
      filename = as.character(get_column(c("Filename")))
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
    "average_speed", "max_speed", "average_gap",
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
    start_date_parsed <- tryCatch(
      {
        lubridate::as_datetime(start_date)
      },
      error = function(e) {
        stop("Could not parse start_date. Please use YYYY-MM-DD format or a Date/POSIXct object.")
      }
    )

    activities_df <- activities_df %>%
      dplyr::filter(.data$start_date_local >= start_date_parsed)

    athlytics_message("Filtered to activities after ", start_date_parsed)
  }

  if (!is.null(end_date)) {
    end_date_parsed <- tryCatch(
      {
        dt <- lubridate::as_datetime(end_date)
        # If time is 00:00:00, extend to end of day
        if (format(dt, "%H:%M:%S") == "00:00:00") {
          dt <- dt + lubridate::hours(23) + lubridate::minutes(59) + lubridate::seconds(59)
        }
        dt
      },
      error = function(e) {
        stop("Could not parse end_date. Please use YYYY-MM-DD format or a Date/POSIXct object.")
      }
    )

    activities_df <- activities_df %>%
      dplyr::filter(.data$start_date_local <= end_date_parsed)

    athlytics_message("Filtered to activities before ", end_date_parsed)
  }

  # Filter by activity type
  if (!is.null(activity_types)) {
    if (!is.character(activity_types)) {
      stop("activity_types must be a character vector (e.g., c('Run', 'Ride'))")
    }

    activities_df <- activities_df %>%
      dplyr::filter(.data$type %in% activity_types)

    athlytics_message("Filtered to activity types: ", paste(activity_types, collapse = ", "))
  }

  # --- Final Sorting ---
  activities_df <- activities_df %>%
    dplyr::arrange(.data$start_date_local)

  athlytics_message("Data loading complete. ", nrow(activities_df), " activities after filtering.")

  return(activities_df)
}
