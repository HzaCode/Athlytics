# Extended tests for calculate_exposure

library(testthat)
library(Athlytics)

create_test_activities_exposure <- function(n_days = 90) {
  dates <- seq(Sys.Date() - n_days, Sys.Date() - 1, by = "day")
  n_activities <- round(n_days * 0.6)  # ~60% of days have activities
  activity_dates <- sort(sample(dates, n_activities, replace = FALSE))
  
  data.frame(
    id = seq_along(activity_dates),
    name = paste("Activity", seq_along(activity_dates)),
    type = sample(c("Run", "Ride"), n_activities, replace = TRUE),
    date = activity_dates,
    distance = abs(rnorm(n_activities, 8000, 2000)),
    moving_time = abs(rnorm(n_activities, 2400, 600)),
    average_watts = ifelse(sample(c(TRUE, FALSE), n_activities, replace = TRUE), 
                           rnorm(n_activities, 200, 50), NA_real_),
    average_heartrate = rnorm(n_activities, 145, 15),
    stringsAsFactors = FALSE
  )
}

test_that("calculate_exposure handles different load metrics", {
  activities <- create_test_activities_exposure(60)
  
  # Test duration_mins
  exp_duration <- calculate_exposure(activities, load_metric = "duration_mins")
  expect_s3_class(exp_duration, "data.frame")
  expect_true("atl" %in% names(exp_duration))
  expect_true("ctl" %in% names(exp_duration))
  
  # Test distance_km
  exp_distance <- calculate_exposure(activities, load_metric = "distance_km")
  expect_s3_class(exp_distance, "data.frame")
})

test_that("calculate_exposure handles different time windows", {
  activities <- create_test_activities_exposure(100)
  
  # Test with different acute/chronic windows
  exp1 <- calculate_exposure(activities, acute_period = 7, chronic_period = 28)
  expect_s3_class(exp1, "data.frame")
  
  exp2 <- calculate_exposure(activities, acute_period = 10, chronic_period = 42)
  expect_s3_class(exp2, "data.frame")
})

test_that("calculate_exposure filters by activity type", {
  activities <- create_test_activities_exposure(60)
  activities$type <- rep(c("Run", "Ride"), length.out = nrow(activities))
  
  exp_runs <- calculate_exposure(activities, activity_type = "Run")
  expect_s3_class(exp_runs, "data.frame")
  
  exp_rides <- calculate_exposure(activities, activity_type = "Ride")
  expect_s3_class(exp_rides, "data.frame")
})

test_that("calculate_exposure handles date ranges", {
  activities <- create_test_activities_exposure(120)
  
  exp <- calculate_exposure(
    activities,
    end_date = Sys.Date() - 30
  )
  
  expect_s3_class(exp, "data.frame")
  if (nrow(exp) > 0) {
    expect_true(all(exp$date <= Sys.Date() - 30))
  }
})

test_that("calculate_exposure calculates ACWR correctly", {
  activities <- create_test_activities_exposure(60)
  
  exp <- calculate_exposure(activities)
  
  expect_s3_class(exp, "data.frame")
  expect_true("acwr" %in% names(exp))
  
  # ACWR should be ATL / CTL (where CTL > 0)
  if (nrow(exp) > 0) {
    valid_rows <- exp$ctl > 0
    if (any(valid_rows)) {
      calculated_acwr <- exp$atl[valid_rows] / exp$ctl[valid_rows]
      expect_equal(exp$acwr[valid_rows], calculated_acwr, tolerance = 0.01)
    }
  }
})

test_that("calculate_exposure handles sparse activity data", {
  # Create very sparse data (only a few activities)
  activities <- create_test_activities_exposure(90)
  activities <- activities[sample(1:nrow(activities), 5), ]  # Keep only 5 activities
  
  exp <- calculate_exposure(activities)
  
  expect_s3_class(exp, "data.frame")
})

test_that("calculate_exposure handles missing data gracefully", {
  skip("Missing data handling needs to be tested at activity load level")
})

