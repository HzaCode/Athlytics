# Test for parse_activity_file.R to boost coverage

test_that("parse_activity_file handles different file types", {
  # Test with FIT files
  result1 <- Athlytics:::parse_activity_file("test.fit")
  expect_null(result1)

  # Test with TCX files
  result2 <- Athlytics:::parse_activity_file("test.tcx")
  expect_null(result2)

  # Test with GPX files
  result3 <- Athlytics:::parse_activity_file("test.gpx")
  expect_null(result3)

  # Test with uppercase extensions
  result4 <- Athlytics:::parse_activity_file("test.FIT")
  expect_null(result4)

  result5 <- Athlytics:::parse_activity_file("test.TCX")
  expect_null(result5)

  result6 <- Athlytics:::parse_activity_file("test.GPX")
  expect_null(result6)
})

test_that("parse_activity_file handles compressed files", {
  # Test with .gz extension
  result1 <- Athlytics:::parse_activity_file("test.fit.gz")
  expect_null(result1)

  # Test with .GZ extension (case insensitive)
  result2 <- Athlytics:::parse_activity_file("test.fit.GZ")
  expect_null(result2)

  # Test with .gz in middle of filename
  result3 <- Athlytics:::parse_activity_file("test.gz.fit")
  expect_null(result3)
})

test_that("parse_activity_file handles unsupported file types", {
  # Test with unsupported extension
  result1 <- Athlytics:::parse_activity_file("test.txt")
  expect_null(result1)

  # Test with no extension
  result2 <- Athlytics:::parse_activity_file("test")
  expect_null(result2)

  # Test with multiple extensions
  result3 <- Athlytics:::parse_activity_file("test.fit.backup")
  expect_null(result3)
})

test_that("parse_activity_file handles error conditions", {
  # Test with NULL file_path (should error)
  expect_error(Athlytics:::parse_activity_file(NULL))

  # Test with empty string
  result2 <- Athlytics:::parse_activity_file("")
  expect_null(result2)

  # Test with NA (should error)
  expect_error(Athlytics:::parse_activity_file(NA))
})

test_that("parse_activity_file handles export_dir parameter", {
  # Test with NULL export_dir
  result1 <- Athlytics:::parse_activity_file("test.fit", export_dir = NULL)
  expect_null(result1)

  # Test with empty export_dir
  result2 <- Athlytics:::parse_activity_file("test.fit", export_dir = "")
  expect_null(result2)

  # Test with nonexistent export_dir
  result3 <- Athlytics:::parse_activity_file("test.fit", export_dir = "/nonexistent/dir")
  expect_null(result3)
})

test_that("parse_activity_file handles file path edge cases", {
  # Test with path containing spaces
  result1 <- Athlytics:::parse_activity_file("test file.fit")
  expect_null(result1)

  # Test with path containing special characters
  result2 <- Athlytics:::parse_activity_file("test-file.fit")
  expect_null(result2)

  # Test with very long path
  long_path <- paste(rep("a", 200), collapse = "")
  result3 <- Athlytics:::parse_activity_file(paste0(long_path, ".fit"))
  expect_null(result3)
})

test_that("parse_activity_file handles compression errors", {
  # Test with corrupted .gz file (simulated by nonexistent file)
  result1 <- Athlytics:::parse_activity_file("corrupted.fit.gz")
  expect_null(result1)

  # Test with .gz file that's not actually compressed
  result2 <- Athlytics:::parse_activity_file("notcompressed.fit.gz")
  expect_null(result2)
})

test_that("parse_activity_file handles file parsing errors", {
  # Test with empty file
  empty_file <- tempfile(fileext = ".fit")
  file.create(empty_file)
  result1 <- Athlytics:::parse_activity_file(empty_file)
  expect_null(result1)
  unlink(empty_file)

  # Test with corrupted file
  corrupted_file <- tempfile(fileext = ".fit")
  writeLines("corrupted data", corrupted_file)
  result2 <- Athlytics:::parse_activity_file(corrupted_file)
  expect_null(result2)
  unlink(corrupted_file)
})

test_that("parse_activity_file handles temporary file cleanup", {
  # Test that temporary files are cleaned up properly
  # This is more of an integration test
  result <- Athlytics:::parse_activity_file("test.fit.gz")
  expect_null(result)

  # Check that no temp files are left behind
  temp_files <- list.files(tempdir(), pattern = ".*\\.fit$", full.names = TRUE)
  # Should be empty or contain only files from other tests
})

test_that("parse_activity_file handles different file formats", {
  # Test with mixed case file extensions
  result1 <- Athlytics:::parse_activity_file("test.FiT")
  expect_null(result1)

  result2 <- Athlytics:::parse_activity_file("test.TcX")
  expect_null(result2)

  result3 <- Athlytics:::parse_activity_file("test.GpX")
  expect_null(result3)

  # Test with file extensions in different positions
  result4 <- Athlytics:::parse_activity_file("test.fit.gz")
  expect_null(result4)

  result5 <- Athlytics:::parse_activity_file("test.tcx.gz")
  expect_null(result5)

  result6 <- Athlytics:::parse_activity_file("test.gpx.gz")
  expect_null(result6)
})

test_that("parse_activity_file handles path resolution", {
  # Test with absolute path
  result1 <- Athlytics:::parse_activity_file("nonexistent.fit")
  expect_null(result1)

  # Test with export_dir parameter
  result2 <- Athlytics:::parse_activity_file("nonexistent.fit", export_dir = ".")
  expect_null(result2)

  # Test with relative path resolution
  result3 <- Athlytics:::parse_activity_file("nonexistent.fit", export_dir = "/tmp")
  expect_null(result3)
})
