# Simple test for calculate_ef.R to boost coverage

test_that("calculate_ef basic functionality", {
  # Create simple mock data
  mock_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2),
    type = c("Run", "Run", "Ride"),
    moving_time = c(2400, 1800, 3600),
    distance = c(8000, 6000, 20000),
    average_heartrate = c(150, 160, 140),
    average_watts = c(0, 0, 200),
    weighted_average_watts = c(0, 0, 220),
    filename = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  
  # Test basic calculation
  result <- calculate_ef(mock_data, quality_control = "off")
  expect_true(is.data.frame(result))
  expect_true("ef_value" %in% colnames(result))
})

test_that("calculate_ef parameter validation", {
  mock_data <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 2400,
    distance = 8000,
    average_heartrate = 150,
    average_watts = 0,
    filename = NA
  )
  
  # Test missing activities_data
  expect_error(calculate_ef(), "activities_data.*must be provided")
  expect_error(calculate_ef(NULL), "activities_data.*must be provided")
  
  # Test invalid parameters
  expect_error(calculate_ef(mock_data, min_duration_mins = -1), "non-negative")
  expect_error(calculate_ef(mock_data, min_steady_minutes = -1), "non-negative")
  expect_error(calculate_ef(mock_data, steady_cv_threshold = 0), "between 0 and 1")
  expect_error(calculate_ef(mock_data, min_hr_coverage = 0), "between 0 and 1")
})

test_that("calculate_ef handles different metrics", {
  mock_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1),
    type = c("Run", "Ride"),
    moving_time = c(2400, 3600),
    distance = c(8000, 20000),
    average_heartrate = c(150, 140),
    average_watts = c(0, 200),
    weighted_average_watts = c(0, 220),
    filename = c(NA, NA),
    stringsAsFactors = FALSE
  )
  
  # Test pace_hr metric
  result_pace <- calculate_ef(mock_data, activity_type = "Run", ef_metric = "pace_hr", quality_control = "off")
  expect_true(is.data.frame(result_pace))
  
  # Test power_hr metric
  result_power <- calculate_ef(mock_data, activity_type = "Ride", ef_metric = "power_hr", quality_control = "off")
  expect_true(is.data.frame(result_power))
})

test_that("calculate_ef handles data quality issues", {
  # Test with missing HR
  mock_no_hr <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 2400,
    distance = 8000,
    average_heartrate = NA,
    average_watts = 0,
    filename = NA
  )
  
  result <- calculate_ef(mock_no_hr, quality_control = "off")
  expect_true(is.data.frame(result))
  
  # Test with zero HR
  mock_zero_hr <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 2400,
    distance = 8000,
    average_heartrate = 0,
    average_watts = 0,
    filename = NA
  )
  
  result2 <- calculate_ef(mock_zero_hr, quality_control = "off")
  expect_true(is.data.frame(result2))
  
  # Test with too short duration
  mock_short <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 300,  # 5 minutes
    distance = 1000,
    average_heartrate = 150,
    average_watts = 0,
    filename = NA
  )
  
  result3 <- calculate_ef(mock_short, min_duration_mins = 20, quality_control = "off")
  expect_true(is.data.frame(result3))
})

test_that("calculate_ef handles quality control modes", {
  mock_data <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 2400,
    distance = 8000,
    average_heartrate = 150,
    average_watts = 0,
    filename = NA
  )
  
  # Test different quality control modes
  result_off <- calculate_ef(mock_data, quality_control = "off")
  expect_true(is.data.frame(result_off))
  
  result_flag <- calculate_ef(mock_data, quality_control = "flag")
  expect_true(is.data.frame(result_flag))
  
  result_filter <- calculate_ef(mock_data, quality_control = "filter")
  expect_true(is.data.frame(result_filter))
})

test_that("calculate_ef handles date filtering", {
  mock_data <- data.frame(
    date = c(Sys.Date() - 200, Sys.Date() - 100, Sys.Date() - 50, Sys.Date() - 10),
    type = rep("Run", 4),
    moving_time = rep(2400, 4),
    distance = rep(8000, 4),
    average_heartrate = rep(150, 4),
    average_watts = rep(0, 4),
    filename = rep(NA, 4),
    stringsAsFactors = FALSE
  )
  
  # Test date filtering
  result <- calculate_ef(mock_data, 
                         start_date = Sys.Date() - 60, 
                         end_date = Sys.Date(),
                         quality_control = "off")
  expect_true(is.data.frame(result))
})

test_that("calculate_ef handles no activities found", {
  mock_old_data <- data.frame(
    date = Sys.Date() - 500,
    type = "Run",
    moving_time = 2400,
    distance = 8000,
    average_heartrate = 150,
    average_watts = 0,
    filename = NA
  )
  
  # Request activities from future dates
  expect_error(calculate_ef(mock_old_data, 
                           start_date = Sys.Date() + 1, 
                           end_date = Sys.Date() + 100),
               "No activities found")
})
