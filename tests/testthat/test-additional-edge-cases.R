test_that("additional edge cases for coverage", {
  # Test with very small numeric values - using reasonable date range
  activities <- data.frame(
    date = as.Date("2023-06-01"),
    type = "Run",
    distance = 0.001,
    moving_time = 1,
    average_heartrate = 50,
    average_speed = 0.001 / 1 * 1000
  )

  result <- calculate_ef(activities, start_date = "2023-05-01", end_date = "2023-07-01", quality_control = "off")
  expect_s3_class(result, "data.frame")

  # Test with very large numeric values
  activities2 <- data.frame(
    date = as.Date("2023-06-01"),
    type = "Run",
    distance = 100000,
    moving_time = 86400,
    average_heartrate = 200,
    average_speed = 100000 / 86400 * 1000
  )

  result2 <- calculate_ef(activities2, start_date = "2023-05-01", end_date = "2023-07-01", quality_control = "off")
  expect_s3_class(result2, "data.frame")

  # Test with NA values in data
  activities3 <- data.frame(
    date = as.Date("2023-06-01"),
    type = "Run",
    distance = 10,
    moving_time = 3600,
    average_heartrate = NA,
    average_speed = 10 / 3600 * 1000
  )

  result3 <- calculate_ef(activities3, start_date = "2023-05-01", end_date = "2023-07-01", quality_control = "off")
  expect_s3_class(result3, "data.frame")
})
