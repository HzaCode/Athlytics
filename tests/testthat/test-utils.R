# tests/testthat/test-utils.R

# Utility Functions Tests

test_that("english_month_year correctly formats dates", {
  single_date <- lubridate::ymd("2023-01-15")
  expect_equal(Athlytics:::english_month_year(single_date), "Jan 2023")

  # Test with multiple dates
  multiple_dates <- c(lubridate::ymd("2023-03-10"), lubridate::ymd("2024-11-05"))
  expect_equal(Athlytics:::english_month_year(multiple_dates), c("Mar 2023", "Nov 2024"))

  # Test with dates spanning year-end
  year_end_dates <- c(lubridate::ymd("2022-12-25"), lubridate::ymd("2023-01-01"))
  expect_equal(Athlytics:::english_month_year(year_end_dates), c("Dec 2022", "Jan 2023"))

  # Test with a leap year date
  leap_date <- lubridate::ymd("2024-02-29")
  expect_equal(Athlytics:::english_month_year(leap_date), "Feb 2024")

  # Test with an empty vector of dates (should return empty character vector)
  empty_dates <- lubridate::ymd(character(0))
  expect_equal(Athlytics:::english_month_year(empty_dates), character(0))

  # NA dates return "NA NA" (format(NA, ...) behaviour); not tested here
})

test_that("palette functions return valid hex colors", {
  pal_nature <- athlytics_palette_nature()
  pal_vibrant <- athlytics_palette_vibrant()

  expect_length(pal_nature, 9)
  expect_length(pal_vibrant, 8)

  expect_true(all(grepl("^#", pal_nature)))
  expect_true(all(grepl("^#", pal_vibrant)))
})

test_that("theme_athlytics returns a valid theme", {
  for (size in c(10, 12, 14)) {
    theme <- theme_athlytics(base_size = size)
    expect_s3_class(theme, "theme")
  }
})
