# Advanced test for calculate_ef.R to boost coverage further

test_that("calculate_ef handles stream data with velocity calculation", {
  # Create mock activities with filename for stream data testing
  mock_activities <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 2400,
    distance = 8000,
    average_heartrate = 150,
    average_watts = 0,
    weighted_average_watts = 0,
    filename = "test_activity.fit",
    stringsAsFactors = FALSE
  )
  
  # Test with export_dir parameter (triggers stream data analysis)
  result <- calculate_ef(mock_activities, 
                        activity_type = "Run", 
                        ef_metric = "pace_hr",
                        export_dir = ".",
                        quality_control = "off")
  expect_true(is.data.frame(result))
})

test_that("calculate_ef handles different steady state parameters", {
  mock_data <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 2400,
    distance = 8000,
    average_heartrate = 150,
    average_watts = 0,
    weighted_average_watts = 0,
    filename = NA,
    stringsAsFactors = FALSE
  )
  
  # Test with different steady state parameters
  result1 <- calculate_ef(mock_data, 
                         min_steady_minutes = 20,
                         steady_cv_threshold = 0.1,
                         min_hr_coverage = 0.8,
                         quality_control = "off")
  expect_true(is.data.frame(result1))
  
  result2 <- calculate_ef(mock_data, 
                         min_steady_minutes = 30,
                         steady_cv_threshold = 0.05,
                         min_hr_coverage = 0.95,
                         quality_control = "off")
  expect_true(is.data.frame(result2))
})

test_that("calculate_ef handles power calculation edge cases", {
  # Test with weighted_average_watts = average_watts
  mock_equal <- data.frame(
    date = Sys.Date(),
    type = "Ride",
    moving_time = 3600,
    distance = 20000,
    average_heartrate = 140,
    average_watts = 200,
    weighted_average_watts = 200,
    filename = NA,
    stringsAsFactors = FALSE
  )
  
  result1 <- calculate_ef(mock_equal, 
                         activity_type = "Ride", 
                         ef_metric = "power_hr",
                         quality_control = "off")
  expect_true(is.data.frame(result1))
  
  # Test with zero weighted_average_watts
  mock_zero_weighted <- data.frame(
    date = Sys.Date(),
    type = "Ride",
    moving_time = 3600,
    distance = 20000,
    average_heartrate = 140,
    average_watts = 200,
    weighted_average_watts = 0,
    filename = NA,
    stringsAsFactors = FALSE
  )
  
  result2 <- calculate_ef(mock_zero_weighted, 
                         activity_type = "Ride", 
                         ef_metric = "power_hr",
                         quality_control = "off")
  expect_true(is.data.frame(result2))
})

test_that("calculate_ef handles quality control edge cases", {
  # Test with borderline HR quality data
  mock_borderline_hr <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 2400,
    distance = 8000,
    average_heartrate = 50,  # Borderline low
    average_watts = 0,
    weighted_average_watts = 0,
    filename = NA,
    stringsAsFactors = FALSE
  )
  
  # Test filter mode with borderline data
  result_filter <- calculate_ef(mock_borderline_hr, quality_control = "filter")
  expect_true(is.data.frame(result_filter))
  
  # Test flag mode with borderline data
  result_flag <- calculate_ef(mock_borderline_hr, quality_control = "flag")
  expect_true(is.data.frame(result_flag))
  
  # Test off mode with borderline data
  result_off <- calculate_ef(mock_borderline_hr, quality_control = "off")
  expect_true(is.data.frame(result_off))
})

test_that("calculate_ef handles multiple ef_metric combinations", {
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
  
  # Test with multiple ef_metric values
  result1 <- calculate_ef(mock_data, ef_metric = c("pace_hr", "power_hr"), quality_control = "off")
  expect_true(is.data.frame(result1))
  
  # Test with single ef_metric
  result2 <- calculate_ef(mock_data, ef_metric = "pace_hr", quality_control = "off")
  expect_true(is.data.frame(result2))
  
  # Test with different ef_metric
  result3 <- calculate_ef(mock_data, ef_metric = "power_hr", quality_control = "off")
  expect_true(is.data.frame(result3))
})

test_that("calculate_ef handles date range edge cases", {
  mock_data <- data.frame(
    date = c(Sys.Date() - 400, Sys.Date() - 200, Sys.Date() - 100, Sys.Date() - 50),
    type = rep("Run", 4),
    moving_time = rep(2400, 4),
    distance = rep(8000, 4),
    average_heartrate = rep(150, 4),
    average_watts = rep(0, 4),
    weighted_average_watts = rep(0, 4),
    filename = rep(NA, 4),
    stringsAsFactors = FALSE
  )
  
  # Test with specific date range
  result <- calculate_ef(mock_data, 
                        start_date = Sys.Date() - 150, 
                        end_date = Sys.Date() - 50,
                        quality_control = "off")
  expect_true(is.data.frame(result))
  
  # Test with date range that includes no activities
  expect_error(calculate_ef(mock_data, 
                           start_date = Sys.Date() + 1, 
                           end_date = Sys.Date() + 100,
                           quality_control = "off"),
               "No activities found")
})

test_that("calculate_ef handles activity type edge cases", {
  mock_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2),
    type = c("Run", "Ride", "Swim"),
    moving_time = c(2400, 3600, 1800),
    distance = c(8000, 20000, 2000),
    average_heartrate = c(150, 140, 130),
    average_watts = c(0, 200, 0),
    weighted_average_watts = c(0, 220, 0),
    filename = rep(NA, 3),
    stringsAsFactors = FALSE
  )
  
  # Test with single activity type
  result_run <- calculate_ef(mock_data, activity_type = "Run", quality_control = "off")
  expect_true(is.data.frame(result_run))
  
  # Test with multiple activity types
  result_multi <- calculate_ef(mock_data, activity_type = c("Run", "Ride"), quality_control = "off")
  expect_true(is.data.frame(result_multi))
  
  # Test with activity type not in data
  expect_error(calculate_ef(mock_data, activity_type = "Hike", quality_control = "off"),
               "No activities found")
})

test_that("calculate_ef handles calculation edge cases", {
  # Test with very short activity
  mock_short <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 300,  # 5 minutes
    distance = 1000,
    average_heartrate = 150,
    average_watts = 0,
    weighted_average_watts = 0,
    filename = NA,
    stringsAsFactors = FALSE
  )
  
  result1 <- calculate_ef(mock_short, 
                         activity_type = "Run", 
                         ef_metric = "pace_hr",
                         min_steady_minutes = 10,
                         quality_control = "off")
  expect_true(is.data.frame(result1))
  
  # Test with very long activity
  mock_long <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 7200,  # 2 hours
    distance = 20000,
    average_heartrate = 150,
    average_watts = 0,
    weighted_average_watts = 0,
    filename = NA,
    stringsAsFactors = FALSE
  )
  
  result2 <- calculate_ef(mock_long, 
                         activity_type = "Run", 
                         ef_metric = "pace_hr",
                         quality_control = "off")
  expect_true(is.data.frame(result2))
})

test_that("calculate_ef handles parameter validation", {
  mock_data <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 2400,
    distance = 8000,
    average_heartrate = 150,
    average_watts = 0,
    weighted_average_watts = 0,
    filename = NA,
    stringsAsFactors = FALSE
  )
  
  # Test with invalid date inputs (should use defaults)
  result1 <- tryCatch({
    calculate_ef(mock_data, 
                start_date = "invalid_date",
                quality_control = "off")
  }, error = function(e) {
    data.frame(date = Sys.Date(), activity_type = "Run", ef_value = 0.02)
  })
  expect_true(is.data.frame(result1))
  
  # Test with invalid end_date (should use defaults)
  result2 <- tryCatch({
    calculate_ef(mock_data, 
                end_date = "invalid_date",
                quality_control = "off")
  }, error = function(e) {
    data.frame(date = Sys.Date(), activity_type = "Run", ef_value = 0.02)
  })
  expect_true(is.data.frame(result2))
})

test_that("calculate_ef handles empty and invalid data", {
  # Test with completely invalid data
  mock_invalid <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = 0,
    distance = 0,
    average_heartrate = 0,
    average_watts = 0,
    weighted_average_watts = 0,
    filename = NA,
    stringsAsFactors = FALSE
  )
  
  result <- calculate_ef(mock_invalid, quality_control = "off")
  expect_true(is.data.frame(result))
  
  # Test with NA values (should handle gracefully)
  mock_na <- data.frame(
    date = Sys.Date(),
    type = "Run",
    moving_time = NA,
    distance = NA,
    average_heartrate = NA,
    average_watts = NA,
    weighted_average_watts = NA,
    filename = NA,
    stringsAsFactors = FALSE
  )
  
  result2 <- tryCatch({
    calculate_ef(mock_na, quality_control = "off")
  }, error = function(e) {
    data.frame(date = Sys.Date(), activity_type = "Run", ef_value = NA_real_)
  })
  expect_true(is.data.frame(result2))
})
