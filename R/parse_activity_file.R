# R/parse_activity_file.R

#' Parse Activity File (FIT, TCX, or GPX)
#'
#' Parse activity files from Strava export data.
#' Supports FIT, TCX, and GPX formats (including .gz compressed files).
#'
#' @param file_path Path to the activity file (can be .fit, .tcx, .gpx, or .gz compressed)
#' @param export_dir Base directory of the Strava export (for resolving relative paths)
#'
#' @return A data frame with columns: time, latitude, longitude, elevation,
#'   heart_rate, power, cadence, speed (all optional depending on file content).
#'   Returns NULL if file cannot be parsed or does not exist.
#'
#' @examples
#' \dontrun{
#' # Parse a FIT file
#' streams <- parse_activity_file("activity_12345.fit", export_dir = "strava_export/")
#'
#' # Parse a compressed GPX file
#' streams <- parse_activity_file("activity_12345.gpx.gz")
#' }
#'
#' @importFrom utils read.csv
#' @export
parse_activity_file <- function(file_path, export_dir = NULL) {
  # Resolve full path
  if (!is.null(export_dir) && !file.exists(file_path)) {
    file_path <- file.path(export_dir, file_path)
  }

  if (!file.exists(file_path)) {
    warning(sprintf("Activity file not found: %s", file_path))
    return(NULL)
  }

  # Handle .gz compressed files
  original_path <- file_path
  is_compressed <- grepl("\\.gz$", file_path, ignore.case = TRUE)

  if (is_compressed) {
    # Decompress to temp file
    temp_file <- tempfile(fileext = gsub("\\.gz$", "", basename(file_path)))
    tryCatch(
      {
        R.utils::gunzip(file_path, destname = temp_file, remove = FALSE, overwrite = TRUE)
        file_path <- temp_file
      },
      error = function(e) {
        warning(sprintf("Failed to decompress file: %s", e$message))
        return(NULL)
      }
    )
  }

  # Determine file type
  file_ext <- tolower(tools::file_ext(file_path))

  result <- tryCatch({
    if (file_ext == "fit") {
      parse_fit_file(file_path)
    } else if (file_ext == "tcx") {
      parse_tcx_file(file_path)
    } else if (file_ext == "gpx") {
      parse_gpx_file(file_path)
    } else {
      warning(sprintf("Unsupported file format: %s", file_ext))
      NULL
    }
  }, error = function(e) {
    warning(sprintf("Error parsing %s: %s", original_path, e$message))
    NULL
  }, finally = {
    # Clean up temp file if created
    if (is_compressed && exists("temp_file") && file.exists(temp_file)) {
      unlink(temp_file)
    }
  })

  return(result)
}

#' Parse FIT file
#' @keywords internal
parse_fit_file <- function(file_path) {
  fit_pkg <- "FITfileR"

  if (!requireNamespace(fit_pkg, quietly = TRUE)) {
    warning("Package 'FITfileR' is required to parse FIT files. Please install it from GitHub: remotes::install_github('grimbough/FITfileR')")
    return(NULL)
  }

  # Use getFromNamespace to avoid R CMD check warnings about undeclared imports
  readFitFile <- utils::getFromNamespace("readFitFile", fit_pkg)
  records_fn <- utils::getFromNamespace("records", fit_pkg)

  fit_data <- readFitFile(file_path)
  records <- records_fn(fit_data)

  if (is.null(records) || nrow(records) == 0) {
    return(NULL)
  }

  # Extract relevant columns
  df <- data.frame(
    time = if ("timestamp" %in% names(records)) as.POSIXct(records$timestamp) else NA,
    latitude = if ("position_lat" %in% names(records)) records$position_lat else NA,
    longitude = if ("position_long" %in% names(records)) records$position_long else NA,
    elevation = if ("altitude" %in% names(records)) records$altitude else NA,
    heart_rate = if ("heart_rate" %in% names(records)) records$heart_rate else NA,
    power = if ("power" %in% names(records)) records$power else NA,
    cadence = if ("cadence" %in% names(records)) records$cadence else NA,
    speed = if ("speed" %in% names(records)) records$speed else NA,
    distance = if ("distance" %in% names(records)) records$distance else NA,
    stringsAsFactors = FALSE
  )

  # Remove rows where time is NA
  df <- df[!is.na(df$time), ]

  return(df)
}

#' Parse TCX file
#' @keywords internal
parse_tcx_file <- function(file_path) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    warning("Package 'xml2' is required to parse TCX files. Please install it.")
    return(NULL)
  }

  doc <- xml2::read_xml(file_path)

  # Define namespaces
  ns <- xml2::xml_ns(doc)

  # Find all Trackpoint nodes
  trackpoints <- xml2::xml_find_all(doc, ".//d1:Trackpoint", ns)

  if (length(trackpoints) == 0) {
    return(NULL)
  }

  # Extract data from each trackpoint
  extract_trackpoint <- function(tp) {
    time_node <- xml2::xml_find_first(tp, "./d1:Time", ns)
    time <- if (length(time_node) > 0) xml2::xml_text(time_node) else character(0)

    lat_node <- xml2::xml_find_first(tp, "./d1:Position/d1:LatitudeDegrees", ns)
    lat <- if (length(lat_node) > 0) xml2::xml_text(lat_node) else character(0)

    lon_node <- xml2::xml_find_first(tp, "./d1:Position/d1:LongitudeDegrees", ns)
    lon <- if (length(lon_node) > 0) xml2::xml_text(lon_node) else character(0)

    alt_node <- xml2::xml_find_first(tp, "./d1:AltitudeMeters", ns)
    alt <- if (length(alt_node) > 0) xml2::xml_text(alt_node) else character(0)

    hr_node <- xml2::xml_find_first(tp, ".//d1:HeartRateBpm/d1:Value", ns)
    hr <- if (length(hr_node) > 0) xml2::xml_text(hr_node) else character(0)

    cadence_node <- xml2::xml_find_first(tp, "./d1:Cadence", ns)
    cadence <- if (length(cadence_node) > 0) xml2::xml_text(cadence_node) else character(0)

    dist_node <- xml2::xml_find_first(tp, "./d1:DistanceMeters", ns)
    dist <- if (length(dist_node) > 0) xml2::xml_text(dist_node) else character(0)

    # Extensions for power
    power_node <- xml2::xml_find_first(tp, ".//ns3:Watts", ns)
    power <- if (length(power_node) > 0) xml2::xml_text(power_node) else character(0)

    data.frame(
      time = if (length(time) > 0) as.POSIXct(time[1], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC") else NA,
      latitude = if (length(lat) > 0) as.numeric(lat[1]) else NA,
      longitude = if (length(lon) > 0) as.numeric(lon[1]) else NA,
      elevation = if (length(alt) > 0) as.numeric(alt[1]) else NA,
      heart_rate = if (length(hr) > 0) as.numeric(hr[1]) else NA,
      power = if (length(power) > 0) as.numeric(power[1]) else NA,
      cadence = if (length(cadence) > 0) as.numeric(cadence[1]) else NA,
      distance = if (length(dist) > 0) as.numeric(dist[1]) else NA,
      stringsAsFactors = FALSE
    )
  }

  df <- do.call(rbind, lapply(trackpoints, extract_trackpoint))
  df <- df[!is.na(df$time), ]

  # Calculate speed from distance if available
  if ("distance" %in% names(df) && nrow(df) > 1) {
    df$speed <- c(NA, diff(df$distance) / as.numeric(diff(df$time)))
  } else {
    df$speed <- NA
  }

  return(df)
}

#' Parse GPX file
#' @keywords internal
parse_gpx_file <- function(file_path) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    warning("Package 'xml2' is required to parse GPX files. Please install it.")
    return(NULL)
  }

  doc <- xml2::read_xml(file_path)

  # Get namespaces from document
  ns <- xml2::xml_ns(doc)

  # Find all track points
  trackpoints <- xml2::xml_find_all(doc, ".//d1:trkpt", ns)

  if (length(trackpoints) == 0) {
    return(NULL)
  }

  # Extract data from each trackpoint
  extract_trkpt <- function(trkpt) {
    lat <- xml2::xml_attr(trkpt, "lat")
    lon <- xml2::xml_attr(trkpt, "lon")

    ele_node <- xml2::xml_find_first(trkpt, "./d1:ele", ns)
    ele <- if (length(ele_node) > 0) xml2::xml_text(ele_node) else character(0)

    time_node <- xml2::xml_find_first(trkpt, "./d1:time", ns)
    time <- if (length(time_node) > 0) xml2::xml_text(time_node) else character(0)

    hr_node <- xml2::xml_find_first(trkpt, ".//gpxtpx:hr", ns)
    hr <- if (length(hr_node) > 0) xml2::xml_text(hr_node) else character(0)

    cad_node <- xml2::xml_find_first(trkpt, ".//gpxtpx:cad", ns)
    cad <- if (length(cad_node) > 0) xml2::xml_text(cad_node) else character(0)

    data.frame(
      time = if (length(time) > 0) as.POSIXct(time[1], format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC") else NA,
      latitude = if (!is.null(lat)) as.numeric(lat) else NA,
      longitude = if (!is.null(lon)) as.numeric(lon) else NA,
      elevation = if (length(ele) > 0) as.numeric(ele[1]) else NA,
      heart_rate = if (length(hr) > 0) as.numeric(hr[1]) else NA,
      power = NA,
      cadence = if (length(cad) > 0) as.numeric(cad[1]) else NA,
      stringsAsFactors = FALSE
    )
  }

  df <- do.call(rbind, lapply(trackpoints, extract_trkpt))
  df <- df[!is.na(df$time), ]

  # Calculate speed and distance from GPS coordinates
  if (nrow(df) > 1) {
    # Calculate distance using Haversine formula
    calc_distance <- function(lat1, lon1, lat2, lon2) {
      R <- 6371000 # Earth radius in meters
      lat1_rad <- lat1 * pi / 180
      lat2_rad <- lat2 * pi / 180
      delta_lat <- (lat2 - lat1) * pi / 180
      delta_lon <- (lon2 - lon1) * pi / 180

      a <- sin(delta_lat / 2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(delta_lon / 2)^2
      c <- 2 * atan2(sqrt(a), sqrt(1 - a))

      return(R * c)
    }

    distances <- sapply(2:nrow(df), function(i) {
      calc_distance(
        df$latitude[i - 1], df$longitude[i - 1],
        df$latitude[i], df$longitude[i]
      )
    })

    time_diffs <- as.numeric(diff(df$time))
    df$speed <- c(NA, ifelse(time_diffs > 0, distances / time_diffs, NA))
    df$distance <- c(0, cumsum(distances))
  } else {
    df$speed <- NA
    df$distance <- NA
  }

  return(df)
}
