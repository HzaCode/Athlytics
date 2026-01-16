# tests/testthat/test-utils.R

library(testthat)
library(Athlytics) # To make internal functions available if using devtools::load_all()
library(lubridate)

# Utility Functions Tests

test_that("english_month_year correctly formats dates", {
  # Test with a single date
  single_date <- ymd("2023-01-15")
  expect_equal(Athlytics:::english_month_year(single_date), "Jan 2023")

  # Test with multiple dates
  multiple_dates <- c(ymd("2023-03-10"), ymd("2024-11-05"))
  expect_equal(Athlytics:::english_month_year(multiple_dates), c("Mar 2023", "Nov 2024"))

  # Test with dates spanning year-end
  year_end_dates <- c(ymd("2022-12-25"), ymd("2023-01-01"))
  expect_equal(Athlytics:::english_month_year(year_end_dates), c("Dec 2022", "Jan 2023"))

  # Test with a leap year date
  leap_date <- ymd("2024-02-29")
  expect_equal(Athlytics:::english_month_year(leap_date), "Feb 2024")

  # Test with an empty vector of dates (should return empty character vector)
  empty_dates <- ymd(character(0))
  expect_equal(Athlytics:::english_month_year(empty_dates), character(0))

  # Test with NA date
  # expect_equal(Athlytics:::english_month_year(ymd(NA)), NA_character_)
})
