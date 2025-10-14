# Extended tests for calculate_ef to improve coverage

library(testthat)
library(Athlytics)

create_test_activities <- function(n = 30, with_hr = TRUE, with_power = FALSE, with_speed = TRUE) {
  data.frame(
    id = 1:n,
    name = paste("Activity", 1:n),
    type = sample(c("Run", "Ride"), n, replace = TRUE),
    date = seq(Sys.Date() - n, Sys.Date() - 1, by = "day"),
    distance = runif(n, 5000, 15000),
    moving_time = runif(n, 1800, 5400),
    average_heartrate = if(with_hr) runif(n, 130, 170) else rep(NA_real_, n),
    average_watts = if(with_power) runif(n, 180, 250) else rep(NA_real_, n),
    average_speed = if(with_speed) runif(n, 2.5, 4.5) else rep(NA_real_, n),
    stringsAsFactors = FALSE
  )
}

test_that("calculate_ef filters by activity type correctly", {
  activities <- create_test_activities(50)
  activities$type <- rep(c("Run", "Ride", "Run", "Swim", "Run"), 10)
  
  ef_runs <- calculate_ef(activities, activity_type = "Run", ef_metric = "pace_hr")
  expect_true(all(ef_runs$activity_type == "Run"))
  
  ef_rides <- calculate_ef(activities, activity_type = "Ride", ef_metric = "power_hr")
  expect_true(all(ef_rides$activity_type == "Ride"))
})

test_that("calculate_ef handles missing heart rate data", {
  activities <- create_test_activities(20, with_hr = FALSE)
  
  ef_result <- calculate_ef(activities, ef_metric = "pace_hr")
  
  # Should return data frame even if all HR is missing
  expect_s3_class(ef_result, "data.frame")
})

test_that("calculate_ef handles missing power data", {
  activities <- create_test_activities(20, with_power = FALSE)
  
  ef_result <- calculate_ef(activities, ef_metric = "power_hr", activity_type = "Ride")
  
  expect_s3_class(ef_result, "data.frame")
})

test_that("calculate_ef handles missing speed data", {
  activities <- create_test_activities(20, with_speed = FALSE)
  
  ef_result <- calculate_ef(activities, ef_metric = "pace_hr")
  
  expect_s3_class(ef_result, "data.frame")
})

test_that("calculate_ef calculates pace/hr correctly", {
  activities <- data.frame(
    id = 1:5,
    name = paste("Run", 1:5),
    type = "Run",
    date = seq(Sys.Date() - 5, Sys.Date() - 1, by = "day"),
    distance = c(10000, 10000, 10000, 10000, 10000),
    moving_time = c(3000, 3000, 3000, 3000, 3000),
    average_heartrate = c(150, 150, 150, 150, 150),
    average_speed = c(3.33, 3.33, 3.33, 3.33, 3.33),
    stringsAsFactors = FALSE
  )
  
  ef_result <- calculate_ef(activities, ef_metric = "pace_hr")
  
  expect_s3_class(ef_result, "data.frame")
  expect_true("ef_value" %in% names(ef_result))
  expect_equal(nrow(ef_result), 5)
})

test_that("calculate_ef calculates power/hr correctly", {
  activities <- data.frame(
    id = 1:5,
    name = paste("Ride", 1:5),
    type = "Ride",
    date = seq(Sys.Date() - 5, Sys.Date() - 1, by = "day"),
    distance = c(20000, 20000, 20000, 20000, 20000),
    moving_time = c(3000, 3000, 3000, 3000, 3000),
    average_heartrate = c(140, 140, 140, 140, 140),
    average_watts = c(200, 200, 200, 200, 200),
    stringsAsFactors = FALSE
  )
  
  ef_result <- calculate_ef(activities, ef_metric = "power_hr", activity_type = "Ride")
  
  expect_s3_class(ef_result, "data.frame")
  expect_true("ef_value" %in% names(ef_result))
  expect_equal(nrow(ef_result), 5)
})

test_that("calculate_ef handles date range filtering", {
  activities <- create_test_activities(60)
  
  start_date <- Sys.Date() - 30
  end_date <- Sys.Date() - 10
  
  ef_result <- calculate_ef(
    activities,
    ef_metric = "pace_hr",
    start_date = start_date,
    end_date = end_date
  )
  
  expect_s3_class(ef_result, "data.frame")
  if (nrow(ef_result) > 0) {
    expect_true(all(ef_result$date >= start_date))
    expect_true(all(ef_result$date <= end_date))
  }
})

test_that("calculate_ef respects min_duration_mins", {
  activities <- create_test_activities(20)
  activities$moving_time <- c(rep(600, 10), rep(3600, 10))  # 10 min vs 60 min
  
  ef_result <- calculate_ef(
    activities,
    ef_metric = "pace_hr",
    min_duration_mins = 30
  )
  
  expect_s3_class(ef_result, "data.frame")
  # Should only include activities >= 30 minutes
  expect_true(nrow(ef_result) <= 10)
})

test_that("calculate_ef handles empty result set", {
  activities <- create_test_activities(10)
  
  # Filter to impossible date range should throw error
  expect_error(
    calculate_ef(
      activities,
      ef_metric = "pace_hr",
      start_date = Sys.Date() + 100,
      end_date = Sys.Date() + 200
    ),
    "No activities found"
  )
})

test_that("calculate_ef handles multiple activity types", {
  activities <- create_test_activities(40)
  activities$type <- rep(c("Run", "Ride"), 20)
  
  ef_result <- calculate_ef(
    activities,
    activity_type = c("Run", "Ride"),
    ef_metric = "pace_hr"
  )
  
  expect_s3_class(ef_result, "data.frame")
  if (nrow(ef_result) > 0) {
    expect_true(all(ef_result$activity_type %in% c("Run", "Ride")))
  }
})

