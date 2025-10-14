# Advanced test for plot_ef.R to boost coverage further

test_that("plot_ef handles complex data scenarios", {
  # Create mock activities data with more complex scenarios
  mock_activities <- data.frame(
    date = seq(Sys.Date() - 200, Sys.Date(), by = "3 days"),
    type = rep("Run", 67),
    moving_time = rep(2400, 67),
    distance = rep(8000, 67),
    average_heartrate = seq(140, 160, length.out = 67),
    average_watts = rep(200, 67),
    filename = rep(NA, 67),
    stringsAsFactors = FALSE
  )
  
  # Test plot_ef with complex data calculation
  p <- plot_ef(data = mock_activities, 
               activity_type = "Run", 
               ef_metric = "pace_hr",
               start_date = Sys.Date() - 100,
               end_date = Sys.Date(),
               min_duration_mins = 15)
  expect_s3_class(p, "ggplot")
  
  # Test with different smoothing parameters
  p2 <- plot_ef(data = mock_activities, 
                activity_type = "Run", 
                ef_metric = "pace_hr",
                smoothing_method = "loess",
                add_trend_line = TRUE)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_ef handles edge cases with data", {
  # Test with data that has gaps
  mock_gapped <- data.frame(
    date = c(Sys.Date() - 100, Sys.Date() - 50, Sys.Date() - 10, Sys.Date()),
    type = rep("Run", 4),
    moving_time = rep(2400, 4),
    distance = rep(8000, 4),
    average_heartrate = c(150, 155, 160, 165),
    average_watts = rep(200, 4),
    filename = rep(NA, 4),
    stringsAsFactors = FALSE
  )
  
  p <- plot_ef(data = mock_gapped, 
               activity_type = "Run", 
               ef_metric = "pace_hr")
  expect_s3_class(p, "ggplot")
  
  # Test with data that has outliers
  mock_outliers <- data.frame(
    date = seq(Sys.Date() - 50, Sys.Date(), by = "7 days"),
    type = rep("Run", 8),
    moving_time = rep(2400, 8),
    distance = rep(8000, 8),
    average_heartrate = c(150, 155, 160, 200, 165, 170, 175, 180),  # Outlier at 200
    average_watts = rep(200, 8),
    filename = rep(NA, 8),
    stringsAsFactors = FALSE
  )
  
  p2 <- plot_ef(data = mock_outliers, 
                activity_type = "Run", 
                ef_metric = "pace_hr")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_ef handles different smoothing scenarios", {
  data("athlytics_sample_ef")
  
  # Test with different smoothing methods and parameters
  p1 <- plot_ef(athlytics_sample_ef, smoothing_method = "loess", add_trend_line = TRUE)
  expect_s3_class(p1, "ggplot")
  
  p2 <- plot_ef(athlytics_sample_ef, smoothing_method = "lm", add_trend_line = FALSE)
  expect_s3_class(p2, "ggplot")
  
  p3 <- plot_ef(athlytics_sample_ef, smoothing_method = "gam", add_trend_line = TRUE)
  expect_s3_class(p3, "ggplot")
})

test_that("plot_ef handles activity type filtering edge cases", {
  data("athlytics_sample_ef")
  
  # Test with activity type that exists in data
  p1 <- plot_ef(athlytics_sample_ef, activity_type = "Run")
  expect_s3_class(p1, "ggplot")
  
  # Test with activity type that doesn't exist in data
  p2 <- plot_ef(athlytics_sample_ef, activity_type = "Swim")
  expect_s3_class(p2, "ggplot")
  
  # Test with multiple activity types
  p3 <- plot_ef(athlytics_sample_ef, activity_type = c("Run", "Ride"))
  expect_s3_class(p3, "ggplot")
})

test_that("plot_ef handles date filtering edge cases", {
  data("athlytics_sample_ef")
  
  # Test with date range that includes all data
  p1 <- plot_ef(athlytics_sample_ef, 
                start_date = Sys.Date() - 365,
                end_date = Sys.Date() + 1)
  expect_s3_class(p1, "ggplot")
  
  # Test with date range that includes no data
  p2 <- plot_ef(athlytics_sample_ef, 
                start_date = Sys.Date() + 1,
                end_date = Sys.Date() + 100)
  expect_s3_class(p2, "ggplot")
  
  # Test with only start_date
  p3 <- plot_ef(athlytics_sample_ef, 
                start_date = Sys.Date() - 30)
  expect_s3_class(p3, "ggplot")
  
  # Test with only end_date
  p4 <- plot_ef(athlytics_sample_ef, 
                end_date = Sys.Date())
  expect_s3_class(p4, "ggplot")
})

test_that("plot_ef handles ef_metric edge cases", {
  data("athlytics_sample_ef")
  
  # Test with pace_hr metric
  p1 <- plot_ef(athlytics_sample_ef, ef_metric = "pace_hr")
  expect_s3_class(p1, "ggplot")
  
  # Test with power_hr metric
  p2 <- plot_ef(athlytics_sample_ef, ef_metric = "power_hr")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_ef handles data structure variations", {
  # Test with minimal required columns
  minimal_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2),
    activity_type = c("Run", "Run", "Run"),
    ef_value = c(0.02, 0.021, 0.019)
  )
  
  p1 <- plot_ef(minimal_data)
  expect_s3_class(p1, "ggplot")
  
  # Test with extra columns
  extra_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2),
    activity_type = c("Run", "Run", "Run"),
    ef_value = c(0.02, 0.021, 0.019),
    extra_col1 = c("A", "B", "C"),
    extra_col2 = c(1, 2, 3)
  )
  
  p2 <- plot_ef(extra_data)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_ef handles single data point", {
  # Test with single data point
  single_data <- data.frame(
    date = Sys.Date(),
    activity_type = "Run",
    ef_value = 0.02
  )
  
  p <- plot_ef(single_data)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ef handles data with NA values", {
  # Test with some NA ef_values
  na_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2),
    activity_type = c("Run", "Run", "Run"),
    ef_value = c(0.02, NA, 0.019)
  )
  
  p <- plot_ef(na_data)
  expect_s3_class(p, "ggplot")
  
  # Test with all NA ef_values
  all_na_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1),
    activity_type = c("Run", "Run"),
    ef_value = c(NA, NA)
  )
  
  p2 <- plot_ef(all_na_data)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_ef handles complex parameter combinations", {
  data("athlytics_sample_ef")
  
  # Test multiple parameters together
  p <- plot_ef(athlytics_sample_ef,
               activity_type = "Run",
               ef_metric = "pace_hr",
               add_trend_line = TRUE,
               smoothing_method = "loess",
               start_date = Sys.Date() - 30,
               end_date = Sys.Date(),
               min_duration_mins = 15)
  expect_s3_class(p, "ggplot")
})
