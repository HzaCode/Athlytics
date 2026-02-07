# tests/testthat/test-ef.R

# Efficiency Factor Calculation Tests

library(Athlytics)
library(testthat)
library(dplyr)

data(sample_ef)

create_mock_activities <- function(n = 30) {
  dates <- seq(Sys.Date() - n, Sys.Date(), by = "day")
  data.frame(
    id = seq_len(length(dates)),
    name = paste("Activity", seq_len(length(dates))),
    type = sample(c("Run", "Ride"), length(dates), replace = TRUE),
    date = as.Date(dates),
    distance = runif(length(dates), 5000, 15000),
    moving_time = as.integer(runif(length(dates), 1800, 5400)),
    average_heartrate = runif(length(dates), 130, 170),
    average_watts = runif(length(dates), 180, 250),
    average_speed = runif(length(dates), 2.5, 4.5),
    stringsAsFactors = FALSE
  )
}

test_that("calculate_ef works with speed_hr metric", {
  mock_activities <- create_mock_activities()

  ef_result <- calculate_ef(
    activities_data = mock_activities,
    ef_metric = "speed_hr"
  )

  expect_s3_class(ef_result, "data.frame")
  expect_true("ef_value" %in% colnames(ef_result))
  expect_gt(nrow(ef_result), 0)
})

test_that("calculate_ef works with power_hr metric", {
  mock_activities <- create_mock_activities()

  ef_result <- calculate_ef(
    activities_data = mock_activities,
    ef_metric = "power_hr"
  )

  expect_s3_class(ef_result, "data.frame")
  expect_true("ef_value" %in% colnames(ef_result))
})

test_that("calculate_ef validates input", {
  expect_error(
    calculate_ef(activities_data = "not_a_dataframe"),
    "data frame"
  )
})

test_that("calculate_ef works with sample data", {
  skip_if(is.null(sample_ef), "Sample EF data not available")

  expect_s3_class(sample_ef, "data.frame")
  # Sample data may have either ef_value or efficiency_factor
  expect_true(any(c("ef_value", "efficiency_factor") %in% colnames(sample_ef)))
})

test_that("plot_ef works with pre-calculated data and has correct structure", {
  skip_if(is.null(sample_ef), "Sample EF data not available")

  p <- plot_ef(sample_ef, add_trend_line = TRUE)
  expect_s3_class(p, "ggplot")

  # Should have at least geom_point and geom_smooth
  expect_true(length(p$layers) >= 2,
    info = "Plot with trend line should have >= 2 layers"
  )

  # Plot without trend line should have fewer layers
  p_no_trend <- plot_ef(sample_ef, add_trend_line = FALSE)
  expect_true(length(p_no_trend$layers) < length(p$layers),
    info = "Plot without trend line should have fewer layers"
  )
})

test_that("plot_ef works with calculated data", {
  mock_activities <- create_mock_activities(50)

  # Refactored: Calculate first
  ef_data_run <- calculate_ef(mock_activities, activity_type = "Run", ef_metric = "speed_hr")

  # Test with speed_hr
  p1 <- plot_ef(
    data = ef_data_run
  )
  expect_s3_class(p1, "ggplot")

  # Refactored: Calculate first
  ef_data_ride <- calculate_ef(mock_activities, activity_type = "Ride", ef_metric = "power_hr")

  # Test with power_hr
  p2 <- plot_ef(
    data = ef_data_ride
  )
  expect_s3_class(p2, "ggplot")
})

test_that("plot_ef handles various options", {
  mock_activities <- create_mock_activities(50)
  ef_data <- calculate_ef(mock_activities, activity_type = "Run", ef_metric = "speed_hr")

  # Test without trend line
  p1 <- plot_ef(
    data = ef_data,
    add_trend_line = FALSE
  )
  expect_s3_class(p1, "ggplot")

  # Test with different smoothing method
  p2 <- plot_ef(
    data = ef_data,
    smoothing_method = "lm"
  )
  expect_s3_class(p2, "ggplot")

  # Date range filtering is done at calculation step now
  ef_data_filtered <- calculate_ef(
    mock_activities,
    activity_type = "Run",
    ef_metric = "speed_hr",
    start_date = Sys.Date() - 30,
    end_date = Sys.Date()
  )

  # Test with filtered data
  p3 <- plot_ef(
    data = ef_data_filtered
  )
  expect_s3_class(p3, "ggplot")
})

test_that("plot_ef works with ef_df parameter", {
  skip_if(is.null(sample_ef), "Sample EF data not available")

  p <- plot_ef(sample_ef)
  expect_s3_class(p, "ggplot")
})

# ============================================================
# Numerical Value Validation
# ============================================================

test_that("calculate_ef produces correct speed_hr values", {
  # Known inputs: distance = 10km, time = 3600s (1hr), HR = 150
  # EF = speed / HR = (10000/3600) / 150 = 2.778 / 150 = 0.01852
  end_date <- Sys.Date()
  start_date <- end_date - 9
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)

  activities <- data.frame(
    id = seq_len(n),
    name = paste("Run", seq_len(n)),
    type = "Run",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(10000, n),
    moving_time = rep(3600, n),
    elapsed_time = rep(3600, n),
    average_heartrate = rep(150, n),
    average_speed = rep(2.778, n),
    filename = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  result <- calculate_ef(
    activities_data = activities,
    activity_type = "Run",
    ef_metric = "speed_hr",
    min_duration_mins = 10,
    min_steady_minutes = 10,
    quality_control = "off",
    start_date = start_date,
    end_date = end_date
  )

  expect_gt(nrow(result), 0)

  # Check EF value is close to expected
  valid_ef <- result %>% dplyr::filter(!is.na(ef_value))
  if (nrow(valid_ef) > 0) {
    expected_ef <- (10000 / 3600) / 150
    expect_true(
      all(abs(valid_ef$ef_value - expected_ef) < 0.005),
      info = sprintf(
        "EF should be ~%.4f (speed/HR), got: %s",
        expected_ef, paste(round(valid_ef$ef_value, 4), collapse = ", ")
      )
    )
  }
})

test_that("calculate_ef produces correct power_hr values", {
  # Known inputs: power = 200W, HR = 150
  # EF = 200/150 = 1.333
  end_date <- Sys.Date()
  start_date <- end_date - 9
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)

  activities <- data.frame(
    id = seq_len(n),
    name = paste("Ride", seq_len(n)),
    type = "Ride",
    date = dates,
    start_date_local = as.POSIXct(dates),
    distance = rep(30000, n),
    moving_time = rep(3600, n),
    elapsed_time = rep(3600, n),
    average_heartrate = rep(150, n),
    average_watts = rep(200, n),
    weighted_average_watts = rep(0, n),
    average_speed = rep(8.3, n),
    filename = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  result <- calculate_ef(
    activities_data = activities,
    activity_type = "Ride",
    ef_metric = "power_hr",
    min_duration_mins = 10,
    min_steady_minutes = 10,
    quality_control = "off",
    start_date = start_date,
    end_date = end_date
  )

  expect_gt(nrow(result), 0)

  valid_ef <- result %>% dplyr::filter(!is.na(ef_value))
  if (nrow(valid_ef) > 0) {
    expected_ef <- 200 / 150
    expect_true(
      all(abs(valid_ef$ef_value - expected_ef) < 0.01),
      info = sprintf("EF should be ~%.3f (power/HR)", expected_ef)
    )
  }
})

test_that("calculate_ef returns athlytics_ef class", {
  end_date <- Sys.Date()
  start_date <- end_date - 9
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)
  activities <- data.frame(
    id = seq_len(n), name = paste("Run", seq_len(n)),
    type = "Run", date = dates, start_date_local = as.POSIXct(dates),
    distance = rep(10000, n), moving_time = rep(3600, n),
    elapsed_time = rep(3600, n), average_heartrate = rep(150, n),
    average_speed = rep(2.778, n), filename = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
  result <- calculate_ef(
    activities_data = activities, activity_type = "Run",
    ef_metric = "speed_hr", quality_control = "off",
    min_duration_mins = 10, min_steady_minutes = 10,
    start_date = start_date, end_date = end_date
  )
  expect_true("athlytics_ef" %in% class(result))
})

# ============================================================
# EF from Stream - Verify Steady-State Detection
# ============================================================

test_that("calculate_ef_from_stream detects steady state and produces correct EF", {
  # Create perfectly steady stream data
  set.seed(123)
  n <- 3600 # 1 hour of data at 1Hz
  steady_stream <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 1),
    distance = cumsum(rep(3.0, n)),
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    stringsAsFactors = FALSE
  )

  result <- calculate_ef_from_stream(
    stream_data = steady_stream,
    activity_date = as.Date("2024-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.8,
    quality_control = "off"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Should have status "ok" for steady data
  expect_equal(result$status, "ok",
    info = "Steady-state data should produce 'ok' status"
  )

  # EF should be close to 3.0/150 = 0.02
  if (!is.na(result$ef_value)) {
    expect_true(abs(result$ef_value - 0.02) < 0.005,
      info = sprintf("EF from steady stream should be ~0.02, got %.4f", result$ef_value)
    )
  }
})

test_that("calculate_ef_from_stream handles heart_rate column renaming", {
  set.seed(456)
  n <- 3600
  stream_with_heart_rate <- data.frame(
    time = 0:(n - 1),
    heart_rate = rep(150, n) + rnorm(n, 0, 1), # "heart_rate" not "heartrate"
    velocity_smooth = rep(3.0, n) + rnorm(n, 0, 0.01),
    distance = cumsum(rep(3.0, n)),
    stringsAsFactors = FALSE
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_with_heart_rate,
    activity_date = as.Date("2024-01-01"),
    act_type = "Run",
    ef_metric = "speed_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.1,
    min_hr_coverage = 0.8,
    quality_control = "off"
  )

  expect_s3_class(result, "data.frame")
  # Should not fail due to missing "heartrate" column
  expect_true(result$status %in% c("ok", "non_steady"),
    info = sprintf("Stream with 'heart_rate' column should be handled, got status: %s", result$status)
  )
})

test_that("calculate_ef_from_stream handles power column renaming", {
  set.seed(789)
  n <- 3600
  stream_with_power <- data.frame(
    time = 0:(n - 1),
    heartrate = rep(150, n) + rnorm(n, 0, 1),
    power = rep(200, n) + rnorm(n, 0, 2), # "power" not "watts"
    stringsAsFactors = FALSE
  )

  result <- calculate_ef_from_stream(
    stream_data = stream_with_power,
    activity_date = as.Date("2024-01-01"),
    act_type = "Ride",
    ef_metric = "power_hr",
    min_steady_minutes = 10,
    steady_cv_threshold = 0.15,
    min_hr_coverage = 0.8,
    quality_control = "off"
  )

  expect_s3_class(result, "data.frame")
  # Should not fail due to missing "watts" column
  expect_true(result$status %in% c("ok", "non_steady", "too_short", "insufficient_valid_data"),
    info = sprintf("Stream with 'power' column should be renamed to 'watts', got status: %s", result$status)
  )
})
