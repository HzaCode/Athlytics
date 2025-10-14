# Test flag_quality with stream data

library(testthat)
library(Athlytics)
library(dplyr)

# Create realistic stream data
create_stream_data <- function(n_points = 3600, sport = "Run") {
  # Create time series (1 hour at 1Hz)
  time_sec <- seq(0, n_points - 1)
  
  # Base heart rate with some variation
  base_hr <- 140 + 20 * sin(time_sec / 300) + rnorm(n_points, 0, 5)
  base_hr <- pmax(60, pmin(180, base_hr))
  
  # Add some HR spikes for testing
  hr_spikes <- sample(1:n_points, 5)
  base_hr[hr_spikes] <- c(250, 10, 200, 5, 220)  # Out of range values
  
  # Power data (for cycling)
  if (sport == "Ride") {
    base_power <- 200 + 50 * sin(time_sec / 200) + rnorm(n_points, 0, 20)
    base_power <- pmax(0, pmin(800, base_power))
    
    # Add power spikes
    pw_spikes <- sample(1:n_points, 3)
    base_power[pw_spikes] <- c(2000, -100, 1800)  # Out of range values
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

# Test basic functionality
test_that("flag_quality works with running stream data", {
  streams <- create_stream_data(sport = "Run")
  
  flagged <- flag_quality(streams, sport = "Run")
  
  expect_s3_class(flagged, "data.frame")
  expect_equal(nrow(flagged), nrow(streams))
  expect_true(all(c("flag_hr_spike", "flag_pw_spike", "flag_gps_drift", 
                   "flag_any", "is_steady_state", "quality_score") %in% names(flagged)))
  
  # Should detect some quality issues
  expect_gt(sum(flagged$flag_any, na.rm = TRUE), 0)
  expect_lt(mean(flagged$quality_score, na.rm = TRUE), 1.0)
})

test_that("flag_quality works with cycling stream data", {
  streams <- create_stream_data(sport = "Ride")
  
  flagged <- flag_quality(streams, sport = "Ride")
  
  expect_s3_class(flagged, "data.frame")
  expect_equal(nrow(flagged), nrow(streams))
  
  # Should detect power spikes
  expect_gt(sum(flagged$flag_pw_spike, na.rm = TRUE), 0)
})

test_that("flag_quality detects HR spikes", {
  streams <- create_stream_data(sport = "Run")
  
  flagged <- flag_quality(streams, sport = "Run")
  
  # Should detect the HR spikes we added
  expect_gt(sum(flagged$flag_hr_spike, na.rm = TRUE), 0)
  
  # Check that extreme values are flagged
  extreme_hr_indices <- which(streams$heartrate > 200 | streams$heartrate < 30)
  if (length(extreme_hr_indices) > 0) {
    expect_true(all(flagged$flag_hr_spike[extreme_hr_indices], na.rm = TRUE))
  }
})

test_that("flag_quality detects GPS drift", {
  streams <- create_stream_data(sport = "Run")
  
  flagged <- flag_quality(streams, sport = "Run")
  
  # Should detect some GPS drift
  expect_gt(sum(flagged$flag_gps_drift, na.rm = TRUE), 0)
})

test_that("flag_quality identifies steady state segments", {
  # Create data with a clear steady state segment
  n_points <- 3600
  time_sec <- seq(0, n_points - 1)
  
  # Create steady HR for middle segment
  hr <- c(
    rep(120, 600),  # Warm up
    rep(150, 1200), # Steady state (20 min)
    rep(180, 1800)  # High intensity
  )
  
  # Create steady speed
  speed <- c(
    rep(2.5, 600),
    rep(3.5, 1200),
    rep(5.0, 1800)
  )
  
  streams <- data.frame(
    time = time_sec,
    heartrate = hr,
    watts = NA,
    velocity_smooth = speed,
    speed = speed,
    distance = cumsum(speed),
    stringsAsFactors = FALSE
  )
  
  flagged <- flag_quality(streams, sport = "Run", min_steady_minutes = 15)
  
  # Should identify steady state in the middle segment
  steady_segment <- 601:1800  # Middle 20 minutes
  expect_gt(sum(flagged$is_steady_state[steady_segment], na.rm = TRUE), 0)
})

test_that("flag_quality handles different sport types", {
  streams_run <- create_stream_data(sport = "Run")
  streams_ride <- create_stream_data(sport = "Ride")
  
  flagged_run <- flag_quality(streams_run, sport = "Run")
  flagged_ride <- flag_quality(streams_ride, sport = "Ride")
  
  # Both should work
  expect_s3_class(flagged_run, "data.frame")
  expect_s3_class(flagged_ride, "data.frame")
  
  # Cycling should have power data
  expect_true(any(!is.na(streams_ride$watts)))
})

test_that("flag_quality handles custom thresholds", {
  streams <- create_stream_data(sport = "Run")
  
  # Test with stricter thresholds
  flagged_strict <- flag_quality(
    streams, 
    sport = "Run",
    hr_range = c(50, 200),
    max_hr_jump = 5,
    max_run_speed = 5.0
  )
  
  # Should flag more issues with stricter thresholds
  expect_gt(sum(flagged_strict$flag_any, na.rm = TRUE), 0)
  
  # Test with lenient thresholds
  flagged_lenient <- flag_quality(
    streams,
    sport = "Run", 
    hr_range = c(20, 250),
    max_hr_jump = 20,
    max_run_speed = 10.0
  )
  
  # Should flag fewer issues
  expect_s3_class(flagged_lenient, "data.frame")
})

test_that("flag_quality handles missing data gracefully", {
  streams <- create_stream_data(sport = "Run")
  
  # Remove some columns
  streams_no_hr <- streams[, !names(streams) %in% "heartrate"]
  streams_no_speed <- streams[, !names(streams) %in% c("velocity_smooth", "speed")]
  
  # Should still work
  flagged_no_hr <- flag_quality(streams_no_hr, sport = "Run")
  expect_s3_class(flagged_no_hr, "data.frame")
  
  flagged_no_speed <- flag_quality(streams_no_speed, sport = "Run")
  expect_s3_class(flagged_no_speed, "data.frame")
})

test_that("flag_quality calculates quality score correctly", {
  streams <- create_stream_data(sport = "Run")
  
  flagged <- flag_quality(streams, sport = "Run")
  
  # Quality score should be between 0 and 1
  expect_true(all(flagged$quality_score >= 0 & flagged$quality_score <= 1, na.rm = TRUE))
  
  # Should be less than 1 due to quality issues
  expect_lt(mean(flagged$quality_score, na.rm = TRUE), 1.0)
})

test_that("flag_quality handles edge cases", {
  # Test with very short data
  short_streams <- data.frame(
    time = 1:10,
    heartrate = rep(150, 10),
    watts = NA,
    velocity_smooth = rep(3.5, 10),
    speed = rep(3.5, 10),
    distance = cumsum(rep(3.5, 10)),
    stringsAsFactors = FALSE
  )
  
  flagged <- flag_quality(short_streams, sport = "Run")
  expect_s3_class(flagged, "data.frame")
  
  # Test with all NA data
  na_streams <- data.frame(
    time = 1:100,
    heartrate = NA,
    watts = NA,
    velocity_smooth = NA,
    speed = NA,
    distance = NA,
    stringsAsFactors = FALSE
  )
  
  flagged_na <- flag_quality(na_streams, sport = "Run")
  expect_s3_class(flagged_na, "data.frame")
})

# Test quality_summary function
test_that("quality_summary works with flagged stream data", {
  streams <- create_stream_data(sport = "Run")
  flagged <- flag_quality(streams, sport = "Run")
  
  summary <- quality_summary(flagged)
  
  expect_type(summary, "list")
  expect_true(all(c("total_points", "flagged_points", "flagged_pct") %in% names(summary)))
  expect_true(is.numeric(summary$total_points))
  expect_true(summary$total_points > 0)
})
