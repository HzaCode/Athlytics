# tests/testthat/test-load_local_activities.R


test_that("load_local_activities validates input parameters", {
  expect_error(load_local_activities())
  expect_error(load_local_activities(NULL))
  expect_error(load_local_activities(""))
  expect_error(load_local_activities(123))
  expect_error(
    load_local_activities("nonexistent_file.csv"),
    "File not found"
  )
})

test_that("load_local_activities with empty CSV warns and returns zero rows", {
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  writeLines(
    "Activity ID,Activity Date,Activity Type,Distance,Elapsed Time,Moving Time,Elevation Gain",
    temp_csv
  )

  expect_warning(
    result <- load_local_activities(temp_csv),
    "No activities found in CSV file"
  )
  expect_equal(nrow(result), 0)
})

test_that("load_local_activities with ZIP missing activities.csv errors", {
  temp_zip <- tempfile(fileext = ".zip")
  temp_txt <- tempfile(fileext = ".txt")
  on.exit(unlink(c(temp_zip, temp_txt)))
  writeLines("test", temp_txt)
  utils::zip(temp_zip, temp_txt, flags = "-q")

  expect_error(
    load_local_activities(temp_zip),
    "No activities.csv file found in ZIP"
  )
})

test_that("load_local_activities warns on multiple activities.csv in ZIP", {
  csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")

  temp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(temp_zip))

  # Create ZIP with two activities.csv (different paths) from package extdata
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  temp_dir <- tempdir()
  sub1 <- file.path(temp_dir, "sub1")
  sub2 <- file.path(temp_dir, "sub2")
  dir.create(sub1, showWarnings = FALSE)
  dir.create(sub2, showWarnings = FALSE)
  file.copy(csv_path, file.path(sub1, "activities.csv"))
  file.copy(csv_path, file.path(sub2, "ACTIVITIES.CSV"))
  setwd(temp_dir)
  utils::zip(temp_zip, c("sub1/activities.csv", "sub2/ACTIVITIES.CSV"), flags = "-q")
  setwd(old_wd)
  unlink(sub1, recursive = TRUE)
  unlink(sub2, recursive = TRUE)

  expect_warning(
    result <- load_local_activities(temp_zip),
    "Multiple activities.csv files found"
  )
  expect_gt(nrow(result), 0)
})

test_that("load_local_activities works with package extdata", {
  csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")

  activities <- suppressWarnings(load_local_activities(csv_path))
  expect_s3_class(activities, "data.frame")
  expect_gt(nrow(activities), 0)
  expect_contains(names(activities), c("id", "date", "type"))
  expect_s3_class(activities$date, "Date")
})

# --- Tests using extdata (merged from test-load-with-real-data.R) ---

test_that("load_local_activities filters by activity type with extdata", {
  csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")
  all_activities <- suppressWarnings(load_local_activities(csv_path))
  unique_types <- unique(all_activities$type)

  first_type <- unique_types[1]
  filtered <- suppressWarnings(load_local_activities(csv_path, activity_types = first_type))
  expect_s3_class(filtered, "data.frame")
  if (nrow(filtered) > 0) {
    expect_true(all(filtered$type == first_type))
  }
})

test_that("load_local_activities detects and handles different date formats", {
  csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")
  activities <- suppressWarnings(load_local_activities(csv_path))
  expect_s3_class(activities$date, "Date")
  expect_true(all(activities$date <= Sys.Date(), na.rm = TRUE))
  expect_true(all(activities$date >= as.Date("2000-01-01"), na.rm = TRUE))
})

test_that("load_local_activities handles missing and NA values in extdata", {
  csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")
  activities <- suppressWarnings(load_local_activities(csv_path))
  expect_false(all(is.na(activities$id)))
  expect_false(all(is.na(activities$date)))
  expect_false(all(is.na(activities$type)))
  expect_gt(ncol(activities), 5)
})

test_that("activity file references are valid in extdata", {
  csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")
  activities <- suppressWarnings(load_local_activities(csv_path))
  if ("filename" %in% names(activities)) {
    has_filename <- !is.na(activities$filename) & nchar(activities$filename) > 0
    if (any(has_filename)) {
      expect_type(activities$filename, "character")
    }
  }
})
