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

# ============================================================
# Regression tests — internal stream / date helpers
# ============================================================

# --- time_weighted_coverage integrates over time gaps (v1.0.4) ----------

test_that("time_weighted_coverage integrates over time gaps (regression)", {
  # Hand-constructed stream: 9 seconds of valid HR (t=0..9) then a 991-second
  # gap with NA HR (t=1000). Row-fraction coverage would report 9/10 = 90%
  # (hiding the sensor dropout); time-weighted coverage must instead report
  # the 9-second valid segment out of 1000 seconds total, i.e. ~0.9 %.
  stream <- data.frame(
    time = c(0:9, 1000),
    heartrate = c(rep(150, 10), NA_real_)
  )

  cov <- Athlytics:::time_weighted_coverage(stream, "heartrate")

  expect_lt(cov, 0.05)
  expect_gt(cov, 0.0)
})

# --- parse_analysis_date warns instead of silent fallback (v1.0.4) ------

test_that("parse_analysis_date warns on malformed input instead of silent fallback (regression)", {
  default <- as.Date("2024-01-01")

  expect_warning(
    parsed <- Athlytics:::parse_analysis_date(
      "2024/13/45",
      default = default,
      arg_name = "start_date"
    ),
    regexp = "Could not parse.*start_date"
  )
  expect_identical(parsed, default)

  # Valid input: no warning, correct parse.
  expect_no_warning(
    parsed_ok <- Athlytics:::parse_analysis_date(
      "2024-06-15",
      default = default,
      arg_name = "start_date"
    )
  )
  expect_identical(parsed_ok, as.Date("2024-06-15"))

  # NULL / NA: silent fallback (documented behaviour).
  expect_no_warning(
    Athlytics:::parse_analysis_date(NULL, default = default, arg_name = "start_date")
  )
  expect_no_warning(
    Athlytics:::parse_analysis_date(NA, default = default, arg_name = "start_date")
  )
})

# --- estimate_sampling_interval recovers the stream cadence (v1.0.5) ----

test_that("estimate_sampling_interval recovers 1 Hz, 0.5 Hz and 2 Hz streams (regression)", {
  one_hz <- data.frame(time = 0:100)
  half_hz <- data.frame(time = seq(0, 200, by = 2))
  two_hz <- data.frame(time = seq(0, 50, by = 0.5))

  expect_equal(Athlytics:::estimate_sampling_interval(one_hz), 1)
  expect_equal(Athlytics:::estimate_sampling_interval(half_hz), 2)
  expect_equal(Athlytics:::estimate_sampling_interval(two_hz), 0.5)
})
