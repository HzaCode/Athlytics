# Extended tests for plot_pbs to improve coverage

library(testthat)
library(Athlytics)
library(ggplot2)

create_mock_pbs <- function(n = 20) {
  data.frame(
    activity_id = 1:n,
    activity_date = seq(Sys.Date() - n*5, by = "5 days", length.out = n),
    distance = rep(c(1000, 5000, 10000, 21097), length.out = n),
    time_seconds = runif(n, 180, 7200),
    is_pb = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
    pace_min_per_km = runif(n, 4, 6),
    speed_km_per_h = runif(n, 10, 15),
    activity_type = "Run",
    stringsAsFactors = FALSE
  )
}

test_that("plot_pbs works with basic pbs_df", {
  skip_if_not_installed("ggplot2")
  
  pbs_data <- create_mock_pbs(15)
  
  # Create plot using pbs_df parameter
  p <- plot_pbs(pbs_df = pbs_data)
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_pbs handles different distance groups", {
  skip_if_not_installed("ggplot2")
  
  pbs_data <- create_mock_pbs(30)
  pbs_data$distance <- rep(c(1000, 5000, 10000), 10)
  
  p <- plot_pbs(pbs_df = pbs_data)
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_pbs handles date range filtering", {
  skip_if_not_installed("ggplot2")
  
  pbs_data <- create_mock_pbs(40)
  
  p <- plot_pbs(
    pbs_df = pbs_data,
    date_range = c(Sys.Date() - 100, Sys.Date() - 50)
  )
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_pbs handles data with no PBs", {
  skip_if_not_installed("ggplot2")
  
  pbs_data <- create_mock_pbs(10)
  pbs_data$is_pb <- FALSE
  
  p <- plot_pbs(pbs_df = pbs_data)
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_pbs handles data with all PBs", {
  skip_if_not_installed("ggplot2")
  
  pbs_data <- create_mock_pbs(10)
  pbs_data$is_pb <- TRUE
  
  p <- plot_pbs(pbs_df = pbs_data)
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_pbs handles single distance", {
  skip_if_not_installed("ggplot2")
  
  pbs_data <- create_mock_pbs(15)
  pbs_data$distance <- 5000  # All same distance
  
  p <- plot_pbs(pbs_df = pbs_data)
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_pbs works with activities data", {
  skip_if_not_installed("ggplot2")
  skip("Requires export_dir - tested elsewhere")
})

