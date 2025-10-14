
test_that("parameter boundary tests for coverage", {
  activities <- data.frame(
    date = as.Date("2023-06-01"),
    type = "Run",
    distance = 10,
    moving_time = 3600,
    average_heartrate = 150,
    average_speed = 10/3600*1000
  )
  
  # 测试最小持续时间
  result1 <- calculate_ef(activities, start_date = "2023-05-01", end_date = "2023-07-01", min_duration_mins = 0, quality_control = "off")
  expect_s3_class(result1, "data.frame")
  
  # 测试最大持续时间
  result2 <- calculate_ef(activities, start_date = "2023-05-01", end_date = "2023-07-01", min_duration_mins = 1000, quality_control = "off")
  expect_s3_class(result2, "data.frame")
  
  # 测试不同的质量控制设置
  result3 <- calculate_ef(activities, start_date = "2023-05-01", end_date = "2023-07-01", quality_control = "off")
  expect_s3_class(result3, "data.frame")
  
  result4 <- calculate_ef(activities, start_date = "2023-05-01", end_date = "2023-07-01", quality_control = "flag")
  expect_s3_class(result4, "data.frame")
})

