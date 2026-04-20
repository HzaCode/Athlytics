# tests/testthat/test-internal-load.R
#
# Tests for the internal load calculation helpers that back calculate_acwr(),
# calculate_acwr_ewma() and calculate_exposure(). The public-facing
# missing_load behaviour is covered in test-acwr.R; this file focuses on the
# `compute_single_load()` value/status contract itself (v1.0.5 regression).

test_that("compute_single_load returns ok status for valid inputs", {
  ok <- Athlytics:::compute_single_load(
    load_metric = "hrss",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = 150, elevation_gain = 0, np_proxy = 0,
    user_ftp = NULL, user_max_hr = 190, user_resting_hr = 50
  )
  expect_equal(ok$status, "ok")
  expect_gt(ok$value, 0)
})

test_that("compute_single_load reports missing_heart_rate for HRSS with no HR", {
  rec <- Athlytics:::compute_single_load(
    load_metric = "hrss",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = NA, elevation_gain = 0, np_proxy = 0,
    user_ftp = NULL, user_max_hr = 190, user_resting_hr = 50
  )
  expect_equal(rec$status, "missing_heart_rate")
  expect_true(is.na(rec$value))
})

test_that("compute_single_load reports missing_ftp for TSS without FTP", {
  rec <- Athlytics:::compute_single_load(
    load_metric = "tss",
    duration_sec = 3600, distance_m = 10000, elapsed_sec = 3600,
    avg_hr = 150, elevation_gain = 0, np_proxy = 250,
    user_ftp = NULL, user_max_hr = NULL, user_resting_hr = NULL
  )
  expect_equal(rec$status, "missing_ftp")
  expect_true(is.na(rec$value))
})
