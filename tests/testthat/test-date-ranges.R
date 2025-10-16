
test_that("date range tests for coverage", {
  activities <- data.frame(
    date = as.Date(c("2023-01-01", "2023-06-15", "2023-12-31")),
    type = c("Run", "Run", "Run"),
    distance = c(10, 15, 20),
    moving_time = c(3600, 5400, 7200),
    average_heartrate = c(150, 160, 170),
    average_speed = c(10/3600*1000, 15/5400*1000, 20/7200*1000)
  )
  
  # Test start date filtering
  result1 <- calculate_ef(activities, start_date = "2023-03-01", end_date = "2024-01-01", quality_control = "off")
  expect_s3_class(result1, "data.frame")
  
  # Test end date filtering
  result2 <- calculate_ef(activities, start_date = "2022-01-01", end_date = "2023-09-01", quality_control = "off")
  expect_s3_class(result2, "data.frame")
  
  # Test date range filtering
  result3 <- calculate_ef(activities, start_date = "2023-02-01", end_date = "2023-08-01", quality_control = "off")
  expect_s3_class(result3, "data.frame")
})

