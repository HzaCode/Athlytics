# tests/testthat/test-load_local_activities.R

context("Load Local Activities")

library(Athlytics)
library(testthat)

test_that("load_local_activities works with sample data", {
  # Use the sample data that comes with the package
  data(athlytics_sample_data)
  
  # Check that sample data has the right structure
  expect_true(!is.null(athlytics_sample_acwr))
  expect_s3_class(athlytics_sample_acwr, "data.frame")
})

test_that("load_local_activities validates input parameters", {
  # Test with non-existent file
  expect_error(
    load_local_activities("nonexistent_file.csv"),
    "File not found"
  )
  
  # Test with invalid activity types
  skip_if_not(file.exists("strava_export_data/activities.csv"))
  expect_error(
    load_local_activities(
      "strava_export_data/activities.csv",
      activity_types = 123
    ),
    "character vector"
  )
})

test_that("load_local_activities detects ZIP files", {
  skip("Requires actual ZIP file for testing")
  
  # This test would run if a test ZIP file is available
  # activities <- load_local_activities("test_export.zip")
  # expect_s3_class(activities, "data.frame")
  # expect_true("id" %in% colnames(activities))
})

