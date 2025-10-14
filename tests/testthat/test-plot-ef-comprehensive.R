# Comprehensive test for plot_ef.R to boost coverage

test_that("plot_ef handles data calculation from activities", {
  # Create mock activities data
  mock_activities <- data.frame(
    date = seq(Sys.Date() - 100, Sys.Date(), by = "7 days"),
    type = rep("Run", 15),
    moving_time = rep(2400, 15),
    distance = rep(8000, 15),
    average_heartrate = seq(140, 160, length.out = 15),
    average_watts = rep(200, 15),
    filename = rep(NA, 15),
    stringsAsFactors = FALSE
  )
  
  # Test plot_ef calculating EF from activities data
  p <- plot_ef(data = mock_activities, 
               activity_type = "Run", 
               ef_metric = "pace_hr")
  expect_s3_class(p, "ggplot")
  
  # Test with different parameters
  p2 <- plot_ef(data = mock_activities, 
                activity_type = "Run", 
                ef_metric = "pace_hr",
                start_date = Sys.Date() - 50,
                end_date = Sys.Date(),
                min_duration_mins = 15)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_ef handles different smoothing methods", {
  data("athlytics_sample_ef")
  
  # Test different smoothing methods
  p_loess <- plot_ef(athlytics_sample_ef, smoothing_method = "loess")
  expect_s3_class(p_loess, "ggplot")
  
  p_lm <- plot_ef(athlytics_sample_ef, smoothing_method = "lm")
  expect_s3_class(p_lm, "ggplot")
  
  p_gam <- plot_ef(athlytics_sample_ef, smoothing_method = "gam")
  expect_s3_class(p_gam, "ggplot")
})

test_that("plot_ef handles different activity type combinations", {
  data("athlytics_sample_ef")
  
  # Test with single activity type
  p_run <- plot_ef(athlytics_sample_ef, activity_type = "Run")
  expect_s3_class(p_run, "ggplot")
  
  # Test with multiple activity types
  p_multi <- plot_ef(athlytics_sample_ef, activity_type = c("Run", "Ride"))
  expect_s3_class(p_multi, "ggplot")
  
  # Test with activity type not in data
  p_none <- plot_ef(athlytics_sample_ef, activity_type = "Swim")
  expect_s3_class(p_none, "ggplot")
})

test_that("plot_ef handles date filtering", {
  data("athlytics_sample_ef")
  
  # Test with date range
  p_date <- plot_ef(athlytics_sample_ef, 
                    start_date = Sys.Date() - 30,
                    end_date = Sys.Date())
  expect_s3_class(p_date, "ggplot")
  
  # Test with only start_date
  p_start <- plot_ef(athlytics_sample_ef, 
                     start_date = Sys.Date() - 30)
  expect_s3_class(p_start, "ggplot")
  
  # Test with only end_date
  p_end <- plot_ef(athlytics_sample_ef, 
                   end_date = Sys.Date())
  expect_s3_class(p_end, "ggplot")
})

test_that("plot_ef handles different ef_metric values", {
  data("athlytics_sample_ef")
  
  # Test with pace_hr metric
  p_pace <- plot_ef(athlytics_sample_ef, ef_metric = "pace_hr")
  expect_s3_class(p_pace, "ggplot")
  
  # Test with power_hr metric
  p_power <- plot_ef(athlytics_sample_ef, ef_metric = "power_hr")
  expect_s3_class(p_power, "ggplot")
})

test_that("plot_ef handles trend line options", {
  data("athlytics_sample_ef")
  
  # Test with trend line
  p_trend <- plot_ef(athlytics_sample_ef, add_trend_line = TRUE)
  expect_s3_class(p_trend, "ggplot")
  
  # Test without trend line
  p_no_trend <- plot_ef(athlytics_sample_ef, add_trend_line = FALSE)
  expect_s3_class(p_no_trend, "ggplot")
})

test_that("plot_ef handles data with different structures", {
  # Test with minimal required columns
  minimal_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1),
    activity_type = c("Run", "Run"),
    ef_value = c(0.02, 0.021)
  )
  
  p_minimal <- plot_ef(minimal_data)
  expect_s3_class(p_minimal, "ggplot")
  
  # Test with extra columns
  extra_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1),
    activity_type = c("Run", "Run"),
    ef_value = c(0.02, 0.021),
    extra_col = c("A", "B")
  )
  
  p_extra <- plot_ef(extra_data)
  expect_s3_class(p_extra, "ggplot")
})

test_that("plot_ef handles edge cases with data", {
  # Test with single row
  single_row <- data.frame(
    date = Sys.Date(),
    activity_type = "Run",
    ef_value = 0.02
  )
  
  p_single <- plot_ef(single_row)
  expect_s3_class(p_single, "ggplot")
  
  # Test with all NA ef_values
  na_data <- data.frame(
    date = c(Sys.Date(), Sys.Date() - 1),
    activity_type = c("Run", "Run"),
    ef_value = c(NA, NA)
  )
  
  p_na <- plot_ef(na_data)
  expect_s3_class(p_na, "ggplot")
})

test_that("plot_ef handles parameter combinations", {
  data("athlytics_sample_ef")
  
  # Test multiple parameters together
  p_combo <- plot_ef(athlytics_sample_ef,
                     activity_type = "Run",
                     ef_metric = "pace_hr",
                     add_trend_line = TRUE,
                     smoothing_method = "loess",
                     start_date = Sys.Date() - 30,
                     end_date = Sys.Date(),
                     min_duration_mins = 15)
  expect_s3_class(p_combo, "ggplot")
})
