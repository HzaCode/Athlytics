# Shared test data and utilities for Athlytics test suite
# testthat automatically sources helper-*.R files before running tests

# --- Package example data paths ---
# Available to all tests; system.file() resolves correctly under devtools::test()
extdata_dir <- system.file("extdata", package = "Athlytics")
extdata_csv <- system.file("extdata", "activities.csv", package = "Athlytics")
extdata_activities_dir <- file.path(extdata_dir, "activities")

# Load the example activities shipped with the package.
# Returns a data.frame (~75 rows: Oct 2024 - Jan 2025, Run + Ride).
load_extdata_activities <- function() {
  load_local_activities(extdata_csv)
}

# Get the date window covered by extdata activities.
# Useful for passing explicit start_date / end_date to analysis functions.
extdata_window <- function(activities) {
  list(
    start_date = min(activities$date, na.rm = TRUE),
    end_date = max(activities$date, na.rm = TRUE)
  )
}

# --- Synthetic stream data generators (for stream-level tests only) ---
# These generate 1Hz time series data that cannot be replaced by CSV files,
# because they test specific data patterns (steady state, spikes, GPS drift).

# Create activity stream data (1Hz time series)
# Used by: test-calculate-functions.R
create_activity_stream <- function(duration_seconds = 3600, steady_state = TRUE) {
  time_points <- seq(0, duration_seconds, by = 1)
  n_points <- length(time_points)

  # Base values
  base_hr <- 150
  base_pace <- 3.0 # m/s
  base_power <- 200 # watts

  if (steady_state) {
    # Steady state: low variation
    hr_variation <- rnorm(n_points, 0, 2)
    pace_variation <- rnorm(n_points, 0, 0.05)
    power_variation <- rnorm(n_points, 0, 5)
  } else {
    # Non-steady state: high variation
    hr_variation <- sin(time_points / 300) * 20 + rnorm(n_points, 0, 5)
    pace_variation <- sin(time_points / 200) * 0.5 + rnorm(n_points, 0, 0.1)
    power_variation <- sin(time_points / 250) * 50 + rnorm(n_points, 0, 10)
  }

  stream_df <- data.frame(
    time = time_points,
    heartrate = round(base_hr + hr_variation),
    velocity_smooth = base_pace + pace_variation,
    distance = cumsum(base_pace + pace_variation),
    watts = round(base_power + power_variation),
    cadence = round(rnorm(n_points, 170, 5)),
    altitude = 100 + cumsum(rnorm(n_points, 0, 0.1)),
    grade_smooth = rnorm(n_points, 0, 1),
    moving = rep(TRUE, n_points),
    stringsAsFactors = FALSE
  )

  # Ensure positive values
  stream_df$heartrate <- pmax(stream_df$heartrate, 60)
  stream_df$heartrate <- pmin(stream_df$heartrate, 200)
  stream_df$velocity_smooth <- pmax(stream_df$velocity_smooth, 0.5)
  stream_df$watts <- pmax(stream_df$watts, 0)

  return(stream_df)
}

# Build a minimal Strava-export ZIP from inst/extdata.
# Returns the path to a temporary .zip containing activities.csv and the
# activities/ directory. Used by tests that exercise the zip-aware gates
# in calculate_pbs() and calculate_decoupling().
make_extdata_zip <- function() {
  src_csv <- system.file("extdata", "activities.csv", package = "Athlytics")
  src_acts <- system.file("extdata", "activities", package = "Athlytics")
  skip_if_not(nzchar(src_csv) && dir.exists(src_acts), "extdata not available")

  zip_path <- tempfile(fileext = ".zip")
  staging <- tempfile(pattern = "athlytics_zip_src_")
  dir.create(file.path(staging, "activities"), recursive = TRUE, showWarnings = FALSE)

  file.copy(src_csv, file.path(staging, "activities.csv"), overwrite = TRUE)
  act_files <- list.files(src_acts, full.names = TRUE)
  file.copy(act_files, file.path(staging, "activities"), overwrite = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(staging)
  utils::zip(zip_path, files = c("activities.csv", "activities"), flags = "-q")

  zip_path
}

# Write a synthetic TCX file with constant pace so PBs are well-defined.
# Used by calculate_pbs() tests that need a deterministic distance-label
# expectation (e.g. the 3000m custom-label regression).
#   total_m        : total distance covered
#   pace_s_per_m   : seconds per metre (inverse of speed in m/s)
#   step_s         : seconds between trackpoints
write_synthetic_tcx <- function(path, total_m = 6000, pace_s_per_m = 0.3, step_s = 1) {
  total_s <- total_m * pace_s_per_m
  n_points <- as.integer(floor(total_s / step_s)) + 1L
  times <- seq(0, by = step_s, length.out = n_points)
  distances <- pmin(total_m, times / pace_s_per_m)
  start_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  timestamps <- format(start_time + times, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  header <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<TrainingCenterDatabase xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2" ',
    'xmlns:ns3="http://www.garmin.com/xmlschemas/ActivityExtension/v2">\n',
    '  <Activities>\n',
    '    <Activity Sport="Running">\n',
    sprintf('      <Id>%s</Id>\n', timestamps[1]),
    sprintf('      <Lap StartTime="%s">\n', timestamps[1]),
    sprintf('        <TotalTimeSeconds>%s</TotalTimeSeconds>\n', format(total_s)),
    sprintf('        <DistanceMeters>%s</DistanceMeters>\n', format(total_m)),
    '        <Track>\n'
  )

  trackpoints <- vapply(seq_along(times), function(i) {
    paste0(
      '          <Trackpoint>\n',
      sprintf('            <Time>%s</Time>\n', timestamps[i]),
      sprintf('            <DistanceMeters>%s</DistanceMeters>\n', format(distances[i], nsmall = 1)),
      '            <HeartRateBpm><Value>150</Value></HeartRateBpm>\n',
      '          </Trackpoint>\n'
    )
  }, character(1))

  footer <- paste0(
    '        </Track>\n',
    '      </Lap>\n',
    '    </Activity>\n',
    '  </Activities>\n',
    '</TrainingCenterDatabase>\n'
  )

  writeLines(c(header, trackpoints, footer), con = path, sep = "")
  invisible(path)
}

# Create realistic stream data with quality issues
# Used by: test-flag-quality-streams.R
create_stream_data <- function(n_points = 3600, sport = "Run") {
  # Create time series (1 hour at 1Hz)
  time_sec <- seq(0, n_points - 1)

  # Base heart rate with some variation
  base_hr <- 140 + 20 * sin(time_sec / 300) + rnorm(n_points, 0, 5)
  base_hr <- pmax(60, pmin(180, base_hr))

  # Add some HR spikes for testing
  hr_spikes <- sample(1:n_points, 5)
  base_hr[hr_spikes] <- c(250, 10, 200, 5, 220) # Out of range values

  # Power data (for cycling)
  if (sport == "Ride") {
    base_power <- 200 + 50 * sin(time_sec / 200) + rnorm(n_points, 0, 20)
    base_power <- pmax(0, pmin(800, base_power))

    # Add power spikes
    pw_spikes <- sample(1:n_points, 3)
    base_power[pw_spikes] <- c(2000, -100, 1800) # Out of range values
  } else {
    base_power <- rep(NA, n_points)
  }

  # Speed data
  if (sport == "Run") {
    base_speed <- 3.5 + 0.5 * sin(time_sec / 400) + rnorm(n_points, 0, 0.2)
    base_speed <- pmax(0, pmin(6, base_speed))

    # Add GPS drift
    gps_drift <- sample(1:n_points, 10)
    base_speed[gps_drift] <- c(15, 0.1, 12, 0.05, 8, 0.2, 10, 0.1, 9, 0.15)
  } else {
    base_speed <- 8 + 2 * sin(time_sec / 300) + rnorm(n_points, 0, 0.5)
    base_speed <- pmax(0, pmin(20, base_speed))
  }

  # Distance (cumulative)
  distance <- cumsum(base_speed)

  data.frame(
    time = time_sec,
    heartrate = base_hr,
    watts = base_power,
    velocity_smooth = base_speed,
    speed = base_speed,
    distance = distance,
    stringsAsFactors = FALSE
  )
}
