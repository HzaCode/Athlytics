# Test color palettes functionality

library(testthat)
library(Athlytics)
library(ggplot2)

# Test palette functions
test_that("athlytics_palette_nature returns correct colors", {
  palette <- athlytics_palette_nature()
  
  expect_type(palette, "character")
  expect_length(palette, 9)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette)))
  
  # Check specific colors
  expect_equal(palette[1], "#E64B35")  # Red
  expect_equal(palette[2], "#4DBBD5")  # Cyan
})

test_that("athlytics_palette_academic returns correct colors", {
  palette <- athlytics_palette_academic()
  
  expect_type(palette, "character")
  expect_length(palette, 8)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette)))
})

test_that("athlytics_palette_vibrant returns correct colors", {
  palette <- athlytics_palette_vibrant()
  
  expect_type(palette, "character")
  expect_length(palette, 8)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette)))
})

test_that("athlytics_palette_science returns correct colors", {
  palette <- athlytics_palette_science()
  
  expect_type(palette, "character")
  expect_length(palette, 8)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette)))
})

test_that("athlytics_palette_cell returns correct colors", {
  palette <- athlytics_palette_cell()
  
  expect_type(palette, "character")
  expect_length(palette, 8)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette)))
})

# Test specialized color functions
test_that("athlytics_colors_acwr_zones returns named list", {
  colors <- athlytics_colors_acwr_zones()
  
  expect_type(colors, "list")
  expect_named(colors)
  expect_true(all(c("undertraining", "safe", "caution", "high_risk") %in% names(colors)))
  expect_true(all(sapply(colors, function(x) grepl("^#[0-9A-Fa-f]{6}$", x))))
})

test_that("athlytics_colors_training_load returns named list", {
  colors <- athlytics_colors_training_load()
  
  expect_type(colors, "list")
  expect_named(colors)
  expect_true(all(c("acute", "chronic", "ratio") %in% names(colors)))
  expect_true(all(sapply(colors, function(x) grepl("^#[0-9A-Fa-f]{6}$", x))))
})

test_that("athlytics_colors_ef returns named list", {
  colors <- athlytics_colors_ef()
  
  expect_type(colors, "list")
  expect_named(colors)
  expect_true(all(c("run", "ride", "swim", "other") %in% names(colors)))
  expect_true(all(sapply(colors, function(x) grepl("^#[0-9A-Fa-f]{6}$", x))))
})

# Test theme function
test_that("theme_athlytics returns ggplot theme", {
  theme <- theme_athlytics()
  
  expect_s3_class(theme, "theme")
  expect_s3_class(theme, "gg")
})

test_that("theme_athlytics accepts custom parameters", {
  theme1 <- theme_athlytics(base_size = 14)
  theme2 <- theme_athlytics(base_family = "Arial")
  
  expect_s3_class(theme1, "theme")
  expect_s3_class(theme2, "theme")
})

# Test scale function
test_that("scale_athlytics works with different palettes", {
  # Test with nature palette
  scale1 <- scale_athlytics("nature", "color")
  expect_s3_class(scale1, "ScaleDiscrete")
  
  # Test with academic palette
  scale2 <- scale_athlytics("academic", "fill")
  expect_s3_class(scale2, "ScaleDiscrete")
  
  # Test with vibrant palette
  scale3 <- scale_athlytics("vibrant", "color")
  expect_s3_class(scale3, "ScaleDiscrete")
  
  # Test with science palette
  scale4 <- scale_athlytics("science", "fill")
  expect_s3_class(scale4, "ScaleDiscrete")
  
  # Test with cell palette
  scale5 <- scale_athlytics("cell", "color")
  expect_s3_class(scale5, "ScaleDiscrete")
})

test_that("scale_athlytics handles invalid palette names", {
  # Should default to nature palette
  scale <- scale_athlytics("invalid", "color")
  expect_s3_class(scale, "ScaleDiscrete")
})

# Test integration with ggplot
test_that("color palettes work with ggplot", {
  # Create test data
  test_data <- data.frame(
    x = 1:5,
    y = 1:5,
    group = letters[1:5]
  )
  
  # Test with nature palette
  p1 <- ggplot(test_data, aes(x, y, color = group)) +
    geom_line() +
    scale_athlytics("nature", "color")
  expect_s3_class(p1, "ggplot")
  
  # Test with academic palette
  p2 <- ggplot(test_data, aes(x, y, fill = group)) +
    geom_bar(stat = "identity") +
    scale_athlytics("academic", "fill")
  expect_s3_class(p2, "ggplot")
  
  # Test with theme
  p3 <- ggplot(test_data, aes(x, y)) +
    geom_line() +
    theme_athlytics()
  expect_s3_class(p3, "ggplot")
})

# Test colorblind accessibility
test_that("palettes contain distinct colors", {
  palettes <- list(
    nature = athlytics_palette_nature(),
    academic = athlytics_palette_academic(),
    vibrant = athlytics_palette_vibrant(),
    science = athlytics_palette_science(),
    cell = athlytics_palette_cell()
  )
  
  for (palette_name in names(palettes)) {
    colors <- palettes[[palette_name]]
    # Check that all colors are unique
    expect_equal(length(colors), length(unique(colors)))
  }
})

# Test that colors are valid hex codes
test_that("all colors are valid hex codes", {
  all_palettes <- c(
    athlytics_palette_nature(),
    athlytics_palette_academic(),
    athlytics_palette_vibrant(),
    athlytics_palette_science(),
    athlytics_palette_cell()
  )
  
  all_specialized <- c(
    unlist(athlytics_colors_acwr_zones()),
    unlist(athlytics_colors_training_load()),
    unlist(athlytics_colors_ef())
  )
  
  all_colors <- c(all_palettes, all_specialized)
  
  # All should be valid hex codes
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", all_colors)))
})

# Test theme elements
test_that("theme_athlytics has expected elements", {
  theme <- theme_athlytics()
  
  # Check that key theme elements are present
  expect_true("plot.title" %in% names(theme))
  expect_true("axis.title" %in% names(theme))
  expect_true("legend.position" %in% names(theme))
  expect_true("panel.grid.major" %in% names(theme))
})
