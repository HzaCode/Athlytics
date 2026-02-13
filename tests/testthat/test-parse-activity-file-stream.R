# Tests for parse_activity_file.R
# Consolidated: redundant nonexistent-file tests merged into focused groups

test_that("parse_activity_file handles missing and invalid files", {
  # Nonexistent files across all supported formats return NULL
  suppressWarnings({
    expect_null(parse_activity_file("nonexistent.fit"))
    expect_null(parse_activity_file("nonexistent.tcx"))
    expect_null(parse_activity_file("nonexistent.gpx"))
    expect_null(parse_activity_file("nonexistent.fit.gz"))
  })

  # Unsupported format
  temp_file <- tempfile(fileext = ".unknown")
  on.exit(unlink(temp_file))
  writeLines("test", temp_file)
  expect_warning(parse_activity_file(temp_file), "Unsupported file format")

  # NULL and NA inputs error
  expect_error(parse_activity_file(NULL))
  expect_error(parse_activity_file(NA))

  # Empty string returns NULL
  suppressWarnings(expect_null(parse_activity_file("")))
})

test_that("parse_activity_file handles corrupted files", {
  # Empty file
  empty_file <- tempfile(fileext = ".fit")
  on.exit(unlink(empty_file), add = TRUE)
  file.create(empty_file)
  suppressWarnings(expect_null(parse_activity_file(empty_file)))

  # File with invalid content
  bad_file <- tempfile(fileext = ".fit")
  on.exit(unlink(bad_file), add = TRUE)
  writeLines("corrupted data", bad_file)
  suppressWarnings(expect_null(parse_activity_file(bad_file)))
})

test_that("parse_activity_file with .gz compression", {
  skip_if_not_installed("R.utils")

  temp_tcx <- tempfile(fileext = ".tcx")
  on.exit(unlink(c(temp_tcx, paste0(temp_tcx, ".gz"))), add = TRUE)
  writeLines("<xml>test</xml>", temp_tcx)
  temp_gz <- paste0(temp_tcx, ".gz")
  R.utils::gzip(temp_tcx, destname = temp_gz, remove = FALSE)

  # Compressed file with invalid content should return NULL
  suppressWarnings(expect_null(parse_activity_file(temp_gz)))
})

test_that("parse_activity_file handles export_dir parameter", {
  suppressWarnings({
    expect_null(parse_activity_file("test.fit", export_dir = NULL))
    expect_null(parse_activity_file("test.fit", export_dir = "/nonexistent/dir"))
  })
})

# ============================================================
# Real File Parsing (inst/extdata)
# ============================================================

test_that("parse_activity_file parses real TCX file correctly", {
  tcx_path <- system.file("extdata", "activities", "example.tcx", package = "Athlytics")
  result <- suppressWarnings(parse_activity_file(tcx_path))
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(any(c("time", "distance", "heartrate") %in% names(result)))
})

test_that("parse_activity_file parses real GPX file correctly", {
  gpx_path <- system.file("extdata", "activities", "example.gpx", package = "Athlytics")
  result <- suppressWarnings(parse_activity_file(gpx_path))
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(any(c("time", "distance", "lat", "lon") %in% names(result)))
})

test_that("parse_activity_file with export_dir resolves path", {
  result <- suppressWarnings(
    parse_activity_file("activities/example.tcx", export_dir = extdata_dir)
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
})

test_that("parse_tcx_file handles empty/invalid XML content", {
  skip_if(!requireNamespace("xml2", quietly = TRUE), "xml2 not available")

  empty_file <- tempfile(fileext = ".tcx")
  on.exit(unlink(empty_file), add = TRUE)
  file.create(empty_file)
  suppressWarnings(expect_null(parse_activity_file(empty_file)))

  bad_file <- tempfile(fileext = ".tcx")
  on.exit(unlink(bad_file), add = TRUE)
  writeLines("this is not xml", bad_file)
  suppressWarnings(expect_null(parse_activity_file(bad_file)))
})

test_that("parse_gpx_file handles empty/invalid XML content", {
  skip_if(!requireNamespace("xml2", quietly = TRUE), "xml2 not available")

  empty_file <- tempfile(fileext = ".gpx")
  on.exit(unlink(empty_file), add = TRUE)
  file.create(empty_file)
  suppressWarnings(expect_null(parse_activity_file(empty_file)))

  bad_file <- tempfile(fileext = ".gpx")
  on.exit(unlink(bad_file), add = TRUE)
  writeLines("not gpx content", bad_file)
  suppressWarnings(expect_null(parse_activity_file(bad_file)))
})
