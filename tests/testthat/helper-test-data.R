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
