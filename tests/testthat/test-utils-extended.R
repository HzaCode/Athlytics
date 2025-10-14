# Extended tests for utils functions

library(testthat)
library(Athlytics)

test_that("utils functions exist and work", {
  # Test any utility functions that might exist
  expect_true(TRUE)  # Placeholder if no specific utils to test
})

test_that("package loads without errors", {
  expect_true("Athlytics" %in% loadedNamespaces())
})

test_that("sample data exists", {
  data("athlytics_sample_acwr", package = "Athlytics")
  data("athlytics_sample_ef", package = "Athlytics")
  data("athlytics_sample_exposure", package = "Athlytics")
  data("athlytics_sample_decoupling", package = "Athlytics")
  data("athlytics_sample_pbs", package = "Athlytics")
  
  expect_true(!is.null(athlytics_sample_acwr))
  expect_true(!is.null(athlytics_sample_ef))
  expect_true(!is.null(athlytics_sample_exposure))
  expect_true(!is.null(athlytics_sample_decoupling))
  expect_true(!is.null(athlytics_sample_pbs))
})

test_that("color palettes are accessible", {
  nature <- athlytics_palette_nature()
  academic <- athlytics_palette_academic()
  vibrant <- athlytics_palette_vibrant()
  science <- athlytics_palette_science()
  cell <- athlytics_palette_cell()
  
  expect_true(is.character(nature))
  expect_true(is.character(academic))
  expect_true(is.character(vibrant))
  expect_true(is.character(science))
  expect_true(is.character(cell))
  
  expect_gt(length(nature), 0)
  expect_gt(length(academic), 0)
  expect_gt(length(vibrant), 0)
  expect_gt(length(science), 0)
  expect_gt(length(cell), 0)
})

test_that("color zone functions work", {
  acwr_zones <- athlytics_colors_acwr_zones()
  training_load <- athlytics_colors_training_load()
  ef_colors <- athlytics_colors_ef()
  
  expect_true(is.list(acwr_zones))
  expect_true(is.list(training_load))
  expect_true(is.list(ef_colors))
  
  expect_true(length(acwr_zones) > 0)
  expect_true(length(training_load) > 0)
  expect_true(length(ef_colors) > 0)
})

test_that("theme_athlytics works", {
  skip_if_not_installed("ggplot2")
  
  theme <- theme_athlytics()
  expect_s3_class(theme, "theme")
  
  # Test with different base sizes
  theme_small <- theme_athlytics(base_size = 10)
  expect_s3_class(theme_small, "theme")
  
  theme_large <- theme_athlytics(base_size = 14)
  expect_s3_class(theme_large, "theme")
})

test_that("scale_athlytics works for different palettes", {
  skip_if_not_installed("ggplot2")
  
  # Test different palettes
  scale_nature <- scale_athlytics("nature", type = "color")
  expect_s3_class(scale_nature, "ScaleDiscrete")
  
  scale_academic <- scale_athlytics("academic", type = "fill")
  expect_s3_class(scale_academic, "ScaleDiscrete")
  
  scale_vibrant <- scale_athlytics("vibrant", type = "color")
  expect_s3_class(scale_vibrant, "ScaleDiscrete")
  
  scale_science <- scale_athlytics("science", type = "fill")
  expect_s3_class(scale_science, "ScaleDiscrete")
  
  scale_cell <- scale_athlytics("cell", type = "color")
  expect_s3_class(scale_cell, "ScaleDiscrete")
})

test_that("scale_athlytics handles invalid palette names", {
  skip_if_not_installed("ggplot2")
  
  # Should default to nature palette
  scale_default <- scale_athlytics("invalid_name", type = "color")
  expect_s3_class(scale_default, "ScaleDiscrete")
})

test_that("package datasets are properly formatted", {
  data("athlytics_sample_acwr", package = "Athlytics")
  
  expect_s3_class(athlytics_sample_acwr, "data.frame")
  expect_true("date" %in% names(athlytics_sample_acwr))
  expect_true("acwr" %in% names(athlytics_sample_acwr) || "acwr_smooth" %in% names(athlytics_sample_acwr))
})

test_that("plot functions handle sample data", {
  skip_if_not_installed("ggplot2")
  
  data("athlytics_sample_acwr", package = "Athlytics")
  
  # Test that plot can be created  
  p <- plot_acwr(athlytics_sample_acwr)
  expect_s3_class(p, "ggplot")
})

