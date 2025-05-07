# tests/testthat/test-acwr.R

context("ACWR Calculation and Plotting")

library(Athlytics)
library(testthat)
library(mockery)

# Load sample data from the package
data(Athlytics_sample_data)

# Load mock activity list for testing calculate_acwr's internal processing
# This specific mock is for simulating rStrava::get_activity_list output
# Keep this source line if mock_activity_list_list is not part of Athlytics_sample_data
# and is essential for testing the raw calculation logic of calculate_acwr.
# Consider refactoring helper-mockdata.R to only contain truly raw API mocks if needed,
# or integrate such mocks into a different structure if Athlytics_sample_data contains only processed data.
source(test_path("helpe-mockdata.R"), local = TRUE) # Assuming this file name is correct and it defines mock_activity_list_list

# Mock Strava token - use structure() to correctly set class attributes
mock_stoken <- structure(
  list(
    token = list(
      access_token = "fake_access_token",
      token_type = "Bearer",
      expires_at = as.integer(Sys.time() + 3600),
      refresh_token = "fake_refresh_token"
    )
  ),
  class = c("Token2.0", "Token")
)

# Define test date range
test_start_date <- "2023-09-13"
test_end_date <- "2023-10-01"
expected_rows <- as.numeric(lubridate::ymd(test_end_date) - lubridate::ymd(test_start_date)) + 1

# --- Test calculate_acwr --- 

test_that("calculate_acwr output structure and basic numeric checks with mocked API", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list # Uses list of lists from helpe-mockdata.R
  )

  acwr_duration <- calculate_acwr(
    stoken = mock_stoken,
    load_metric = "duration_mins",
    activity_type = "Run", # Specify activity_type for filtering within calculate_acwr
    acute_period = 7,
    chronic_period = 14,
    start_date = test_start_date,
    end_date = test_end_date
  )

  # Structure checks
  expect_s3_class(acwr_duration, "data.frame")
  expect_true(all(c("date", "atl", "ctl", "acwr", "acwr_smooth") %in% colnames(acwr_duration)))
  expect_s3_class(acwr_duration$date, "Date")
  # The number of rows might vary based on actual mock data and filtering, adjust if necessary
  # For this specific mocked data, the date range is small, so expected_rows might be accurate.
  # However, calculate_acwr also fetches data prior to start_date for CTL calculation.
  # The final output is filtered for the analysis_start_date to analysis_end_date.
  expect_equal(nrow(acwr_duration), expected_rows) 

  # Numerical type and range checks (remove NA or Inf values)
  atl_valid <- acwr_duration$atl[!is.na(acwr_duration$atl)]
  ctl_valid <- acwr_duration$ctl[!is.na(acwr_duration$ctl)]
  acwr_valid <- acwr_duration$acwr[!is.na(acwr_duration$acwr) & is.finite(acwr_duration$acwr)]
  acwr_smooth_valid <- acwr_duration$acwr_smooth[!is.na(acwr_duration$acwr_smooth) & is.finite(acwr_duration$acwr_smooth)]

  expect_true(is.numeric(atl_valid))
  expect_true(is.numeric(ctl_valid))
  expect_true(is.numeric(acwr_valid))
  expect_true(is.numeric(acwr_smooth_valid))

  # Check if calculations produce non-negative values (where applicable)
  if(length(atl_valid) > 0) expect_true(all(atl_valid >= 0))
  if(length(ctl_valid) > 0) expect_true(all(ctl_valid >= 0))
  # ACWR can be positive or NA/Inf if CTL is zero, already handled by is.finite
})

test_that("calculate_acwr handles invalid inputs", {
  expect_error(calculate_acwr(stoken = "invalid"), ".*Token2.0 object.*")

  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list 
  )

  expect_error(
    calculate_acwr(
      stoken = mock_stoken,
      acute_period = 28,
      chronic_period = 7,
      start_date = test_start_date,
      end_date = test_end_date
    ),
    message = "`acute_period` must be less than `chronic_period`."
  )
})

# --- Test plot_acwr --- 

test_that("plot_acwr returns a ggplot object using pre-calculated Athlytics_sample_data", {
  # Ensure Athlytics_sample_data is loaded and contains athlytics_sample_acwr
  expect_true(exists("athlytics_sample_acwr"))
  expect_s3_class(athlytics_sample_acwr, "data.frame")
  
  # Assuming plot_acwr uses 'load_metric' for plot labels even with pre-calculated df.
  # Provide a relevant activity_type if it influences plot aesthetics or default calculations if df was NULL.
  p <- plot_acwr(acwr_df = athlytics_sample_acwr, load_metric = "duration_mins", activity_type = "Run") 
  expect_s3_class(p, "ggplot")
  # Check if the plot data contains the key column used for y-axis
  expect_true("acwr_smooth" %in% names(p$data))
})

# This test verifies the branch where plot_acwr calls calculate_acwr internally
test_that("plot_acwr correctly calls calculate_acwr and generates plot with mocked API", {
  local_mocked_bindings(
    .package = "rStrava",
    get_activity_list = function(...) mock_activity_list_list 
  )

  p <- plot_acwr(
    stoken = mock_stoken,
    activity_type = "Run",
    load_metric = "duration_mins",
    start_date = test_start_date,
    end_date = test_end_date,
    acute_period = 7,
    chronic_period = 14 # Ensure periods are consistent with calculate_acwr test if needed
  )
  expect_s3_class(p, "ggplot")
  # Further checks on p$data could be done here, e.g., expected columns from calculate_acwr
  expect_true(all(c("date", "atl", "ctl", "acwr", "acwr_smooth") %in% colnames(p$data)))
})
