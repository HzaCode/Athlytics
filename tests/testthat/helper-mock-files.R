# tests/testthat/helper-mock-files.R
# Helper functions to create mock Strava export files

library(xml2)

# Create a temporary directory structure that mimics Strava export
create_mock_strava_export <- function(base_dir = tempdir(), n_activities = 5) {
  export_dir <- file.path(base_dir, "mock_strava_export")
  activities_dir <- file.path(export_dir, "activities")
  
  # Create directories
  dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(activities_dir, showWarnings = FALSE)
  
  # Create activities.csv with proper column names (spaces, not dots)
  activities_data <- data.frame(
    `Activity ID` = seq_len(n_activities),
    `Activity Date` = format(seq(Sys.Date() - n_activities + 1, Sys.Date(), by = "day"), "%b %d, %Y, %I:%M:%S %p"),
    `Activity Name` = paste("Morning Run", seq_len(n_activities)),
    `Activity Type` = "Run",
    `Activity Description` = "",
    `Elapsed Time` = round(runif(n_activities, 1800, 3600)),
    Distance = round(runif(n_activities, 5000, 10000)),
    `Max Heart Rate` = round(runif(n_activities, 160, 180)),
    `Relative Effort` = round(runif(n_activities, 50, 100)),
    Commute = "false",
    `Activity Gear` = "",
    Filename = paste0("activities/", seq_len(n_activities), ".tcx"),
    `Athlete Weight` = "",
    `Bike Weight` = "",
    `Elapsed Time.1` = round(runif(n_activities, 1800, 3600)),
    `Moving Time` = round(runif(n_activities, 1700, 3500)),
    Distance.1 = round(runif(n_activities, 5000, 10000)),
    `Max Speed` = round(runif(n_activities, 4, 6), 1),
    `Average Speed` = round(runif(n_activities, 3, 4), 1),
    `Elevation Gain` = round(runif(n_activities, 50, 200)),
    `Elevation Loss` = round(runif(n_activities, 50, 200)),
    `Elevation Low` = round(runif(n_activities, 50, 100)),
    `Elevation High` = round(runif(n_activities, 100, 300)),
    `Max Grade` = round(runif(n_activities, 5, 15)),
    `Average Grade` = round(runif(n_activities, 0, 2), 1),
    `Max Cadence` = round(runif(n_activities, 170, 190)),
    `Average Cadence` = round(runif(n_activities, 160, 180)),
    `Average Heart Rate` = round(runif(n_activities, 140, 160)),
    Calories = round(runif(n_activities, 300, 600)),
    `Max Heart Rate.1` = round(runif(n_activities, 160, 180)),
    `Average Watts` = "",
    `Max Watts` = "",
    `Weighted Average Power` = "",
    `Relative Effort.1` = round(runif(n_activities, 50, 100)),
    `Average Elapsed Speed` = round(runif(n_activities, 2.8, 3.8), 1),
    check.names = FALSE
  )
  
  write.csv(activities_data, file.path(export_dir, "activities.csv"), row.names = FALSE)
  
  # Create mock activity files
  for (i in seq_len(n_activities)) {
    create_mock_tcx_file(
      file.path(activities_dir, paste0(i, ".tcx")),
      activity_date = Sys.Date() - n_activities + i,
      duration_seconds = activities_data$`Elapsed Time`[i],
      distance_meters = activities_data$Distance[i]
    )
  }
  
  return(export_dir)
}

# Create a mock TCX file with realistic data
create_mock_tcx_file <- function(filepath, activity_date, duration_seconds = 3600, distance_meters = 10000) {
  # Generate time points
  n_points <- min(duration_seconds, 360)  # Limit to 360 points (every 10 seconds for 1 hour)
  time_points <- seq(0, duration_seconds, length.out = n_points)
  
  # Generate realistic running data
  base_hr <- 150
  base_speed <- distance_meters / duration_seconds  # m/s
  
  # Add some variation
  hr_data <- round(base_hr + sin(time_points / 300) * 10 + rnorm(n_points, 0, 3))
  hr_data <- pmax(60, pmin(200, hr_data))  # Clamp to reasonable range
  
  distance_data <- cumsum(base_speed + rnorm(n_points, 0, 0.1))
  distance_data <- distance_data * (distance_meters / max(distance_data))  # Scale to target distance
  
  # Create TCX structure
  tcx_content <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<TrainingCenterDatabase xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2">\n',
    '  <Activities>\n',
    '    <Activity Sport="Running">\n',
    '      <Id>', format(activity_date, "%Y-%m-%dT%H:%M:%SZ"), '</Id>\n',
    '      <Lap StartTime="', format(activity_date, "%Y-%m-%dT%H:%M:%SZ"), '">\n',
    '        <TotalTimeSeconds>', duration_seconds, '</TotalTimeSeconds>\n',
    '        <DistanceMeters>', distance_meters, '</DistanceMeters>\n',
    '        <Calories>0</Calories>\n',
    '        <Track>\n'
  )
  
  # Add trackpoints
  for (i in seq(1, n_points, by = 10)) {  # Sample every 10 seconds
    timestamp <- activity_date + time_points[i]
    tcx_content <- paste0(tcx_content,
      '          <Trackpoint>\n',
      '            <Time>', format(timestamp, "%Y-%m-%dT%H:%M:%SZ"), '</Time>\n',
      '            <DistanceMeters>', round(distance_data[i], 2), '</DistanceMeters>\n',
      '            <HeartRateBpm>\n',
      '              <Value>', hr_data[i], '</Value>\n',
      '            </HeartRateBpm>\n',
      '          </Trackpoint>\n'
    )
  }
  
  tcx_content <- paste0(tcx_content,
    '        </Track>\n',
    '      </Lap>\n',
    '    </Activity>\n',
    '  </Activities>\n',
    '</TrainingCenterDatabase>'
  )
  
  writeLines(tcx_content, filepath)
}

# Create a mock FIT file (simplified - just creates a text file with data)
create_mock_fit_file <- function(filepath, activity_date, duration_seconds = 3600, distance_meters = 10000) {
  # For testing purposes, create a simple CSV that parse_activity_file can handle
  n_points <- min(duration_seconds, 1000)  # Limit points for performance
  time_points <- seq(0, duration_seconds, length.out = n_points)
  
  # Generate realistic data
  base_hr <- 150
  base_speed <- distance_meters / duration_seconds
  base_power <- 200
  
  stream_data <- data.frame(
    time = time_points,
    distance = cumsum(rep(base_speed * (duration_seconds / n_points), n_points)),
    heartrate = round(base_hr + sin(time_points / 300) * 10 + rnorm(n_points, 0, 3)),
    watts = round(base_power + sin(time_points / 250) * 30 + rnorm(n_points, 0, 10)),
    cadence = round(rnorm(n_points, 170, 5)),
    velocity_smooth = base_speed + rnorm(n_points, 0, 0.1),
    altitude = 100 + cumsum(rnorm(n_points, 0, 0.1)),
    grade_smooth = rnorm(n_points, 0, 1),
    moving = TRUE
  )
  
  # Ensure positive values
  stream_data$heartrate <- pmax(60, pmin(200, stream_data$heartrate))
  stream_data$watts <- pmax(0, stream_data$watts)
  stream_data$velocity_smooth <- pmax(0.5, stream_data$velocity_smooth)
  
  # Save as CSV (parse_activity_file should handle this)
  write.csv(stream_data, filepath, row.names = FALSE)
}

# Create a mock GPX file
create_mock_gpx_file <- function(filepath, activity_date, duration_seconds = 3600, distance_meters = 10000) {
  n_points <- min(duration_seconds / 10, 360)  # One point every 10 seconds
  
  # Generate GPS coordinates (small variations around a point)
  base_lat <- 40.7128
  base_lon <- -74.0060
  
  gpx_content <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<gpx version="1.1" creator="MockStrava">\n',
    '  <metadata>\n',
    '    <time>', format(activity_date, "%Y-%m-%dT%H:%M:%SZ"), '</time>\n',
    '  </metadata>\n',
    '  <trk>\n',
    '    <name>Morning Run</name>\n',
    '    <type>Run</type>\n',
    '    <trkseg>\n'
  )
  
  for (i in seq_len(n_points)) {
    time_offset <- (i - 1) * 10
    timestamp <- activity_date + time_offset
    lat <- base_lat + rnorm(1, 0, 0.0001)
    lon <- base_lon + rnorm(1, 0, 0.0001)
    ele <- 100 + rnorm(1, 0, 5)
    hr <- round(150 + rnorm(1, 0, 10))
    
    gpx_content <- paste0(gpx_content,
      '      <trkpt lat="', lat, '" lon="', lon, '">\n',
      '        <ele>', ele, '</ele>\n',
      '        <time>', format(timestamp, "%Y-%m-%dT%H:%M:%SZ"), '</time>\n',
      '        <extensions>\n',
      '          <hr>', hr, '</hr>\n',
      '        </extensions>\n',
      '      </trkpt>\n'
    )
  }
  
  gpx_content <- paste0(gpx_content,
    '    </trkseg>\n',
    '  </trk>\n',
    '</gpx>'
  )
  
  writeLines(gpx_content, filepath)
}

# Helper to clean up mock files
cleanup_mock_export <- function(export_dir) {
  if (dir.exists(export_dir)) {
    unlink(export_dir, recursive = TRUE)
  }
}
