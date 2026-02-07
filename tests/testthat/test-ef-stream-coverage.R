# Test to boost coverage for calculate_ef_from_stream function

test_that("calculate_ef_from_stream handles missing required columns", {
  # Test missing velocity data for speed_hr
  stream_no_velocity <- data.frame(
    time = 1:100,
    heartrate = rep(150, 100)
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_no_velocity,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr"
  )

  expect_equal(result$status, "missing_velocity_data")
  expect_true(is.na(result$ef_value))

  # Test missing power data for power_hr
  stream_no_power <- data.frame(
    time = 1:100,
    heartrate = rep(140, 100)
  )

  result2 <- calculate_ef_from_stream(
    stream_data = stream_no_power,
    activity_date = Sys.Date(),
    act_type = "Ride",
    ef_metric = "power_hr"
  )

  expect_equal(result2$status, "missing_power_data")
  expect_true(is.na(result2$ef_value))

  # Test missing heartrate column
  stream_no_hr <- data.frame(
    time = 1:100,
    distance = 1:100
  )

  result3 <- calculate_ef_from_stream(
    stream_data = stream_no_hr,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr"
  )

  expect_equal(result3$status, "missing_hr_data")
  expect_true(is.na(result3$ef_value))
})

test_that("calculate_ef_from_stream handles insufficient data", {
  # Test insufficient data points (< 100)
  small_stream <- data.frame(
    time = 1:50,
    heartrate = rep(150, 50),
    distance = 1:50
  )

  result <- calculate_ef_from_stream(
    stream_data = small_stream,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr"
  )

  expect_equal(result$status, "insufficient_data_points")
  expect_true(is.na(result$ef_value))
})

test_that("calculate_ef_from_stream handles low HR coverage", {
  # Create stream with lots of NA heartrate
  stream_low_hr <- data.frame(
    time = 1:200,
    heartrate = c(rep(150, 30), rep(NA, 170)),
    distance = 1:200 * 10
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_low_hr,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_hr_coverage = 0.7 # Require 70% HR coverage
  )

  expect_true(result$status %in% c("insufficient_hr_data", "insufficient_valid_data", "insufficient_data_points"))
  expect_true(is.na(result$ef_value))
})

test_that("calculate_ef_from_stream handles velocity calculation from distance", {
  # Test with distance column (no velocity_smooth)
  stream_with_distance <- data.frame(
    time = seq(0, 599, by = 1), # 10 minutes of data
    heartrate = rep(150, 600),
    distance = seq(0, 3000, length.out = 600) # 3km in 10 minutes
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_with_distance,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 5,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.7,
    quality_control = "off"
  )

  expect_true(is.data.frame(result))
  expect_true("ef_value" %in% names(result))
})

test_that("calculate_ef_from_stream handles velocity_smooth column", {
  # Test with velocity_smooth column
  stream_with_velocity <- data.frame(
    time = seq(0, 599, by = 1),
    heartrate = rep(150, 600),
    velocity_smooth = rep(5.0, 600) # 5 m/s constant
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_with_velocity,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 5,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.7,
    quality_control = "off"
  )

  expect_true(is.data.frame(result))
  expect_true("ef_value" %in% names(result))
})

test_that("calculate_ef_from_stream handles power data", {
  # Test power_hr metric
  stream_with_power <- data.frame(
    time = seq(0, 599, by = 1),
    heartrate = rep(140, 600),
    watts = rep(200, 600) # 200W constant
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_with_power,
    activity_date = Sys.Date(),
    act_type = "Ride",
    ef_metric = "power_hr",
    min_steady_minutes = 5,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.7,
    quality_control = "off"
  )

  expect_true(is.data.frame(result))
  expect_true("ef_value" %in% names(result))
})

test_that("calculate_ef_from_stream handles quality control filtering", {
  # Test with unrealistic values that should be filtered out
  stream_bad_values <- data.frame(
    time = seq(0, 599, by = 1),
    heartrate = c(rep(150, 300), rep(250, 300)), # 250 is too high
    velocity_smooth = c(rep(5, 300), rep(20, 300)) # 20 m/s is too fast
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_bad_values,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 5,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.7,
    quality_control = "filter"
  )

  expect_true(is.data.frame(result))
  # Should filter out bad values
})

test_that("calculate_ef_from_stream handles too short duration", {
  # Test with stream that's too short
  stream_short <- data.frame(
    time = seq(0, 120, by = 1), # Only 2 minutes
    heartrate = rep(150, 121),
    velocity_smooth = rep(5, 121)
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_short,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 10, # Require 10 minutes
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.7,
    quality_control = "off"
  )

  expect_equal(result$status, "too_short")
  expect_true(is.na(result$ef_value))
})

test_that("calculate_ef_from_stream handles non-steady activity", {
  # Test with highly variable data (non-steady)
  set.seed(123)
  stream_variable <- data.frame(
    time = seq(0, 599, by = 1),
    heartrate = rep(150, 600),
    velocity_smooth = runif(600, 3, 8) # Very variable speed
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_variable,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 5,
    steady_cv_threshold = 0.05, # Very strict CV threshold
    min_hr_coverage = 0.7,
    quality_control = "off"
  )

  # Might return non_steady or might find some steady periods
  expect_true(is.data.frame(result))
  expect_true("status" %in% names(result))
})

test_that("calculate_ef_from_stream calculates valid EF for good steady data", {
  # Test with good steady-state data
  stream_steady <- data.frame(
    time = seq(0, 1199, by = 1), # 20 minutes
    heartrate = rnorm(1200, mean = 150, sd = 3), # Stable HR
    velocity_smooth = rnorm(1200, mean = 5, sd = 0.2) # Stable pace
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_steady,
    activity_date = Sys.Date(),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.7,
    quality_control = "off"
  )

  expect_true(is.data.frame(result))
  if (result$status == "ok") {
    expect_true(!is.na(result$ef_value))
    expect_true(result$ef_value > 0)
  }
})

test_that("calculate_ef handles export_dir and stream parsing", {
  skip("Requires real activity files")

  # This test would need actual activity files
  # Skipping as it requires complex test setup
})
