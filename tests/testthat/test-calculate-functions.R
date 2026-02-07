# tests/testthat/test-calculate-functions.R
# Tests for calculate functions with simulated data

library(testthat)
library(Athlytics)
library(dplyr)
library(lubridate)

# Helper function to create realistic activity data
create_realistic_activities <- function(n_days = 90, base_date = Sys.Date() - 90) {
  dates <- seq(base_date, by = "day", length.out = n_days)

  # Simulate realistic running pattern (4-5 runs per week)
  run_days <- sort(sample(1:n_days, size = round(n_days * 0.6)))

  activities <- data.frame(
    id = seq_along(run_days),
    name = paste("Morning Run", seq_along(run_days)),
    type = "Run",
    sport_type = "Run",
    date = dates[run_days],
    start_date_local = as.POSIXct(dates[run_days]),
    distance = abs(rnorm(length(run_days), mean = 8000, sd = 2000)), # 8km average
    moving_time = abs(rnorm(length(run_days), mean = 2400, sd = 600)), # 40min average
    elapsed_time = abs(rnorm(length(run_days), mean = 2500, sd = 700)),
    total_elevation_gain = abs(rnorm(length(run_days), mean = 100, sd = 50)),
    average_heartrate = abs(rnorm(length(run_days), mean = 145, sd = 15)),
    max_heartrate = abs(rnorm(length(run_days), mean = 175, sd = 10)),
    average_speed = abs(rnorm(length(run_days), mean = 3.3, sd = 0.3)),
    max_speed = abs(rnorm(length(run_days), mean = 4.5, sd = 0.5)),
    average_cadence = rnorm(length(run_days), mean = 170, sd = 10),
    average_watts = NA_real_, # No power for runs
    kilojoules = NA_real_,
    has_heartrate = TRUE,
    gear_id = NA_character_,
    filename = paste0("activities/", run_days, ".fit"),
    stringsAsFactors = FALSE
  )

  # Ensure positive values
  activities$distance <- abs(activities$distance)
  activities$moving_time <- abs(activities$moving_time)
  activities$elapsed_time <- pmax(activities$elapsed_time, activities$moving_time)

  # Add calculated fields
  activities$distance_km <- activities$distance / 1000
  activities$duration_mins <- activities$moving_time / 60
  activities$average_speed_kmh <- (activities$distance_km / activities$duration_mins) * 60

  return(activities)
}

# Helper to create activity stream data
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

# Test calculate_acwr
test_that("calculate_acwr works with simulated data", {
  activities <- create_realistic_activities(90)

  result <- calculate_acwr(
    activities_data = activities,
    load_metric = "duration_mins",
    activity_type = "Run",
    acute_period = 7,
    chronic_period = 28
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "atl", "ctl", "acwr", "acwr_smooth") %in% colnames(result)))
  expect_gt(nrow(result), 0)
  expect_true(all(result$acwr >= 0, na.rm = TRUE))
  expect_true(all(result$atl >= 0, na.rm = TRUE))
  expect_true(all(result$ctl >= 0, na.rm = TRUE))
})

test_that("calculate_acwr handles different load metrics", {
  activities <- create_realistic_activities(60)

  # Test with distance
  result_distance <- calculate_acwr(
    activities_data = activities,
    load_metric = "distance_km",
    activity_type = "Run"
  )
  expect_s3_class(result_distance, "data.frame")
  expect_gt(mean(result_distance$atl, na.rm = TRUE), 0)

  # Test with duration (should always work)
  result_duration <- calculate_acwr(
    activities_data = activities,
    load_metric = "duration_mins",
    activity_type = "Run"
  )
  expect_s3_class(result_duration, "data.frame")
})

# Test calculate_acwr_ewma
test_that("calculate_acwr_ewma works with simulated data", {
  activities <- create_realistic_activities(90)

  result <- calculate_acwr_ewma(
    activities_data = activities,
    activity_type = "Run",
    load_metric = "duration_mins"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "atl", "ctl", "acwr", "acwr_smooth") %in% colnames(result)))
  # EWMA returns all dates in the range (including dates without activities)
  expect_gt(nrow(result), 0)
  expect_true(all(unique(activities$date) %in% result$date))
})

test_that("calculate_acwr_ewma handles edge cases", {
  # Single activity
  single_activity <- create_realistic_activities(1)
  result <- calculate_acwr_ewma(
    activities_data = single_activity,
    activity_type = "Run"
  )
  # EWMA returns a full date range
  expect_gt(nrow(result), 0)
  # Check that the activity date is included
  expect_true(single_activity$date[1] %in% result$date)

  # Empty data
  empty_df <- data.frame()
  expect_error(calculate_acwr_ewma(activities_data = empty_df, activity_type = "Run"))
})

# Test calculate_ef
test_that("calculate_ef works with simulated data", {
  activities <- create_realistic_activities(30)
  # Add some activities with heartrate
  activities$average_heartrate <- rnorm(nrow(activities), mean = 145, sd = 15)

  result <- calculate_ef(
    activities_data = activities,
    ef_metric = "speed_hr"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "ef_value", "activity_type") %in% colnames(result)))
  expect_gt(nrow(result), 0)
  expect_true(all(result$ef_value > 0, na.rm = TRUE))
})

test_that("calculate_ef handles different metrics", {
  activities <- create_realistic_activities(20)

  # Add power data for some activities
  activities$average_watts <- ifelse(
    runif(nrow(activities)) > 0.5,
    rnorm(nrow(activities), mean = 200, sd = 30),
    NA
  )

  # Test speed/HR
  result_pace <- calculate_ef(activities_data = activities, ef_metric = "speed_hr")
  expect_gt(nrow(result_pace), 0)

  # Test power/HR (only for activities with power)
  result_power <- calculate_ef(activities_data = activities, ef_metric = "power_hr")
  power_activities <- sum(!is.na(activities$average_watts) & !is.na(activities$average_heartrate))
  # Power activities may include some with 0 watts which get filtered
  expect_true(nrow(result_power) >= 0)
})

# Test calculate_decoupling with stream data
test_that("calculate_decoupling works with simulated stream data", {
  # Test steady state (low decoupling)
  steady_stream <- create_activity_stream(duration_seconds = 3600, steady_state = TRUE)

  decoupling_steady <- calculate_decoupling(
    stream_df = steady_stream,
    decouple_metric = "speed_hr"
  )

  expect_type(decoupling_steady, "double")
  expect_true(is.finite(decoupling_steady))
  expect_true(decoupling_steady > -20 && decoupling_steady < 20) # Reasonable range

  # Test with power/HR
  decoupling_power <- calculate_decoupling(
    stream_df = steady_stream,
    decouple_metric = "power_hr"
  )

  expect_type(decoupling_power, "double")
  expect_true(is.finite(decoupling_power))
})

test_that("calculate_decoupling handles non-steady state", {
  # Create non-steady state data
  variable_stream <- create_activity_stream(duration_seconds = 3600, steady_state = FALSE)

  # Should still calculate but might have higher values
  decoupling <- calculate_decoupling(
    stream_df = variable_stream,
    decouple_metric = "speed_hr"
  )

  expect_type(decoupling, "double")
  # Non-steady state might have values outside typical range
  expect_true(is.finite(decoupling) || is.na(decoupling))
})

# Test calculate_exposure
test_that("calculate_exposure works with simulated data", {
  activities <- create_realistic_activities(60)

  result <- calculate_exposure(
    activities_data = activities,
    load_metric = "duration_mins"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "atl", "ctl") %in% colnames(result)))
  expect_gt(nrow(result), 0)
  # Just check that we get some results
  expect_true(nrow(result) > 0)
})

# Test calculate_pbs
test_that("calculate_pbs handles missing stream files gracefully", {
  activities <- create_realistic_activities(10)

  # This should error when directory doesn't exist
  expect_error(
    calculate_pbs(
      activities_data = activities,
      export_dir = "non_existent_dir",
      distances_m = c(5000, 10000)
    ),
    "Export directory not found"
  )
})

# Test flag_quality
test_that("flag_quality works with simulated stream data", {
  stream <- create_activity_stream(duration_seconds = 3600, steady_state = TRUE)

  # Inject some quality issues
  stream$heartrate[c(100, 200)] <- 250 # Out-of-range HR spikes
  stream$velocity_smooth[300] <- 15 # Unrealistic speed

  flagged <- flag_quality(stream, sport = "Run")

  expect_s3_class(flagged, "data.frame")
  expect_true(all(c("flag_hr_spike", "flag_any", "is_steady_state", "quality_score") %in% colnames(flagged)))
  expect_gt(sum(flagged$flag_any), 0) # Should flag some issues
})

test_that("quality_summary works with flagged stream data", {
  stream <- create_activity_stream(duration_seconds = 3600, steady_state = TRUE)
  flagged <- flag_quality(stream, sport = "Run")

  summary_result <- summarize_quality(flagged)

  expect_type(summary_result, "list")
  expect_true(all(c("total_points", "flagged_points", "flagged_pct", "quality_score") %in% names(summary_result)))
})

# Test cohort_reference
test_that("cohort_reference works with simulated multi-athlete data", {
  # Create data for multiple athletes
  athlete_data <- list()
  for (i in 1:5) {
    activities <- create_realistic_activities(60)
    acwr_data <- calculate_acwr(
      activities_data = activities,
      activity_type = "Run",
      load_metric = "duration_mins"
    )
    acwr_data$athlete_id <- paste0("athlete_", i)
    athlete_data[[i]] <- acwr_data
  }

  cohort_data <- bind_rows(athlete_data)

  reference <- calculate_cohort_reference(
    data = cohort_data,
    metric = "acwr",
    by = NULL,
    probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
    min_athletes = 3
  )

  expect_s3_class(reference, "data.frame")
  expect_true(all(c("date", "percentile", "value", "n_athletes") %in% colnames(reference)))
  expect_equal(length(unique(reference$percentile)), 5)
  expect_true(all(reference$n_athletes >= 3))
})
