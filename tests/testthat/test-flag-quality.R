# tests/testthat/test-flag_quality.R

test_that("flag_quality detects HR spikes", {
  # Create synthetic data with HR spike
  stream_data <- data.frame(
    time = 1:100,
    heartrate = c(rep(150, 50), 250, rep(150, 49)) # One spike at position 51
  )

  result <- flag_quality(stream_data, sport = "Run")

  # Check that spike is flagged
  expect_true(result$flag_hr_spike[51])
  expect_true(result$flag_any[51])

  # Check that non-spike values before are not flagged
  expect_false(result$flag_hr_spike[50])
  # Note: position 52 might also be flagged due to jump back down from spike
  # This is correct behavior, so we check that at least the spike itself is detected
})

test_that("flag_quality detects power spikes", {
  # Create synthetic data with power spike
  stream_data <- data.frame(
    time = 1:100,
    watts = c(rep(200, 50), 1600, rep(200, 49)) # One spike at position 51
  )

  result <- flag_quality(stream_data, sport = "Ride")

  # Check that spike is flagged
  expect_true(result$flag_pw_spike[51])
  expect_true(result$flag_any[51])
})

test_that("flag_quality detects excessive HR jumps", {
  # Create data with excessive HR jump
  stream_data <- data.frame(
    time = 1:100,
    heartrate = c(rep(140, 50), 160, rep(140, 49)) # Jump of 20 bpm in 1 sec
  )

  result <- flag_quality(stream_data, sport = "Run", max_hr_jump = 10)

  # Check that jump is flagged
  expect_true(result$flag_hr_spike[51])
})

test_that("flag_quality detects GPS drift", {
  # Create data with implausible speed
  stream_data <- data.frame(
    time = 1:100,
    velocity_smooth = c(rep(3.5, 50), 10, rep(3.5, 49)) # 10 m/s = ~36 km/h running
  )

  result <- flag_quality(stream_data, sport = "Run", max_run_speed = 7.0)

  # Check that drift is flagged
  expect_true(result$flag_gps_drift[51])
  expect_true(result$flag_any[51])
})

test_that("flag_quality calculates quality score", {
  # Create clean data
  clean_data <- data.frame(
    time = 1:100,
    heartrate = rep(150, 100),
    watts = rep(200, 100),
    velocity_smooth = rep(3.5, 100)
  )

  result <- flag_quality(clean_data, sport = "Run")

  # Quality score should be 1.0 (perfect)
  expect_equal(result$quality_score[1], 1.0)

  # Create data with 10% flagged points
  dirty_data <- data.frame(
    time = 1:100,
    heartrate = c(rep(250, 10), rep(150, 90)) # 10% out of range
  )

  result2 <- flag_quality(dirty_data, sport = "Run")

  # Quality score should be ~0.9
  expect_lt(result2$quality_score[1], 1.0)
  expect_gt(result2$quality_score[1], 0.85)
})

test_that("flag_quality handles empty data gracefully", {
  empty_data <- data.frame(time = numeric(0))

  expect_warning(result <- flag_quality(empty_data))
  expect_equal(nrow(result), 0)
  expect_contains(colnames(result), "flag_any")
})

test_that("flag_quality detects steady state", {
  # Create long steady-state data (25 minutes at constant pace)
  steady_data <- data.frame(
    time = 0:(25 * 60 - 1), # 25 minutes
    velocity_smooth = rnorm(25 * 60, mean = 3.5, sd = 0.1) # Low variability
  )

  result <- flag_quality(steady_data, sport = "Run", min_steady_minutes = 20)

  # Should have some steady-state points
  expect_gt(sum(result$is_steady_state, na.rm = TRUE), 0)
})

test_that("quality_summary provides correct statistics", {
  stream_data <- data.frame(
    time = 1:100,
    heartrate = c(rep(250, 10), rep(150, 90)) # 10% out of range + 1 transition
  )

  result <- flag_quality(stream_data, sport = "Run")
  summary_stats <- summarize_quality(result)

  expect_equal(summary_stats$total_points, 100)
  # Expect 10 out-of-range + 1 jump (transition from 250 to 150)
  expect_equal(summary_stats$flagged_points, 11)
  expect_equal(summary_stats$flagged_pct, 11)
  expect_true(summary_stats$quality_score > 0 && summary_stats$quality_score <= 1)
})

test_that("flag_quality is sport-aware", {
  # Create data with speed that's OK for cycling but not running
  stream_data <- data.frame(
    time = 1:100,
    velocity_smooth = rep(12, 100) # 12 m/s = ~43 km/h
  )

  # Should be flagged for running
  result_run <- flag_quality(stream_data, sport = "Run", max_run_speed = 7.0)
  expect_gt(sum(result_run$flag_gps_drift), 0)

  # Should NOT be flagged for cycling
  result_ride <- flag_quality(stream_data, sport = "Ride", max_ride_speed = 25.0)
  expect_equal(sum(result_ride$flag_gps_drift), 0)
})

test_that("quality_summary is deprecated but remains available", {
  stream_data <- data.frame(
    time = 1:100,
    heartrate = rep(150, 100)
  )
  flagged <- flag_quality(stream_data, sport = "Run")

  expect_warning(
    out <- quality_summary(flagged),
    "deprecated"
  )
  expect_type(out, "list")
})

# --- Stream-level tests (using create_stream_data from helper-test-data.R) ---

test_that("flag_quality works with full cycling stream data", {
  set.seed(402)
  streams <- create_stream_data(sport = "Ride")

  flagged <- flag_quality(streams, sport = "Ride")

  expect_s3_class(flagged, "data.frame")
  expect_equal(nrow(flagged), nrow(streams))
  expect_gt(sum(flagged$flag_pw_spike, na.rm = TRUE), 0)
})

test_that("flag_quality handles custom thresholds", {
  set.seed(406)
  streams <- create_stream_data(sport = "Run")

  flagged_strict <- flag_quality(
    streams,
    sport = "Run",
    hr_range = c(50, 200),
    max_hr_jump = 5,
    max_run_speed = 5.0
  )
  expect_gt(sum(flagged_strict$flag_any, na.rm = TRUE), 0)

  flagged_lenient <- flag_quality(
    streams,
    sport = "Run",
    hr_range = c(20, 250),
    max_hr_jump = 20,
    max_run_speed = 10.0
  )
  expect_s3_class(flagged_lenient, "data.frame")
})

test_that("flag_quality handles missing data columns", {
  set.seed(407)
  streams <- create_stream_data(sport = "Run")

  streams_no_hr <- streams[, !names(streams) %in% "heartrate"]
  streams_no_speed <- streams[, !names(streams) %in% c("velocity_smooth", "speed")]

  flagged_no_hr <- flag_quality(streams_no_hr, sport = "Run")
  expect_s3_class(flagged_no_hr, "data.frame")

  flagged_no_speed <- flag_quality(streams_no_speed, sport = "Run")
  expect_s3_class(flagged_no_speed, "data.frame")
})

test_that("flag_quality handles short and all-NA edge cases", {
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
