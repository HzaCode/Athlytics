# tests/testthat/test-acwr-ewma-advanced.R

# Test advanced ACWR EWMA functionality

library(Athlytics)
library(testthat)

# Create comprehensive mock data
create_advanced_mock_activities <- function(n = 100) {
  dates <- seq(Sys.Date() - n, Sys.Date(), by = "day")
  data.frame(
    id = seq_len(length(dates)),
    name = paste("Activity", seq_len(length(dates))),
    type = sample(c("Run", "Ride"), length(dates), replace = TRUE, prob = c(0.6, 0.4)),
    date = as.Date(dates),
    distance = runif(length(dates), 2000, 15000),
    moving_time = as.integer(runif(length(dates), 1200, 7200)),
    average_heartrate = runif(length(dates), 120, 180),
    average_watts = runif(length(dates), 100, 300),
    stringsAsFactors = FALSE
  )
}

test_that("calculate_acwr_ewma works with EWMA method", {
  mock_activities <- create_advanced_mock_activities(50)
  
  result <- calculate_acwr_ewma(mock_activities, 
                               activity_type = "Run",
                               method = "ewma",
                               load_metric = "duration_mins")
  
  expect_s3_class(result, "data.frame")
  expect_true("atl" %in% colnames(result))
  expect_true("ctl" %in% colnames(result))
  expect_true("acwr" %in% colnames(result))
})

test_that("calculate_acwr_ewma works with different half-lives", {
  mock_activities <- create_advanced_mock_activities(50)
  
  # Test different half-life combinations
  result1 <- calculate_acwr_ewma(mock_activities, 
                                activity_type = "Run",
                                method = "ewma",
                                half_life_acute = 2,
                                half_life_chronic = 10)
  
  result2 <- calculate_acwr_ewma(mock_activities, 
                                activity_type = "Run",
                                method = "ewma",
                                half_life_acute = 5,
                                half_life_chronic = 20)
  
  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
})

test_that("calculate_acwr_ewma works with confidence intervals", {
  mock_activities <- create_advanced_mock_activities(50)
  
  result <- calculate_acwr_ewma(mock_activities, 
                               activity_type = "Run",
                               method = "ewma",
                               ci = TRUE,
                               B = 50)  # Reduced for faster testing
  
  expect_s3_class(result, "data.frame")
  expect_true("acwr_lower" %in% colnames(result))
  expect_true("acwr_upper" %in% colnames(result))
})

test_that("calculate_acwr_ewma works with TSS load metric", {
  mock_activities <- create_advanced_mock_activities(50)
  
  result <- calculate_acwr_ewma(mock_activities, 
                               activity_type = "Ride",
                               load_metric = "tss",
                               user_ftp = 250)
  
  expect_s3_class(result, "data.frame")
})

test_that("calculate_acwr_ewma works with HRSS load metric", {
  mock_activities <- create_advanced_mock_activities(50)
  
  result <- calculate_acwr_ewma(mock_activities, 
                               activity_type = "Run",
                               load_metric = "hrss",
                               user_max_hr = 190,
                               user_resting_hr = 50)
  
  expect_s3_class(result, "data.frame")
})

test_that("calculate_acwr_ewma works with elevation gain", {
  mock_activities <- create_advanced_mock_activities(50)
  mock_activities$elevation_gain <- runif(nrow(mock_activities), 0, 1000)
  
  result <- calculate_acwr_ewma(mock_activities, 
                               activity_type = "Run",
                               load_metric = "elevation_gain_m")
  
  expect_s3_class(result, "data.frame")
})

test_that("calculate_acwr_ewma handles different smoothing periods", {
  mock_activities <- create_advanced_mock_activities(50)
  
  result1 <- calculate_acwr_ewma(mock_activities, 
                                activity_type = "Run",
                                smoothing_period = 3)
  
  result2 <- calculate_acwr_ewma(mock_activities, 
                                activity_type = "Run",
                                smoothing_period = 14)
  
  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
})

test_that("calculate_acwr_ewma handles different confidence levels", {
  mock_activities <- create_advanced_mock_activities(50)
  
  result1 <- calculate_acwr_ewma(mock_activities, 
                                activity_type = "Run",
                                method = "ewma",
                                ci = TRUE,
                                conf_level = 0.90)
  
  result2 <- calculate_acwr_ewma(mock_activities, 
                                activity_type = "Run",
                                method = "ewma",
                                ci = TRUE,
                                conf_level = 0.99)
  
  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
})

test_that("calculate_acwr_ewma handles different block lengths", {
  mock_activities <- create_advanced_mock_activities(50)
  
  result1 <- calculate_acwr_ewma(mock_activities, 
                                activity_type = "Run",
                                method = "ewma",
                                ci = TRUE,
                                block_len = 3)
  
  result2 <- calculate_acwr_ewma(mock_activities, 
                                activity_type = "Run",
                                method = "ewma",
                                ci = TRUE,
                                block_len = 14)
  
  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
})

test_that("calculate_acwr_ewma handles mixed activity types", {
  mock_activities <- create_advanced_mock_activities(50)
  
  result <- calculate_acwr_ewma(mock_activities, 
                               activity_type = c("Run", "Ride"),
                               load_metric = "duration_mins")
  
  expect_s3_class(result, "data.frame")
})

test_that("calculate_acwr_ewma handles date range filtering", {
  mock_activities <- create_advanced_mock_activities(50)
  
  result <- calculate_acwr_ewma(mock_activities, 
                               activity_type = "Run",
                               start_date = Sys.Date() - 30,
                               end_date = Sys.Date())
  
  expect_s3_class(result, "data.frame")
})
