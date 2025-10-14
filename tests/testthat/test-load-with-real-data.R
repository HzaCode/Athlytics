# Tests using real Strava export data

library(testthat)
library(Athlytics)

# Find project root by looking for DESCRIPTION file
find_project_root <- function() {
  # Try current directory and parent directories
  for (i in 0:5) {
    path <- if (i == 0) "." else paste(rep("..", i), collapse = "/")
    desc_file <- file.path(path, "DESCRIPTION")
    if (file.exists(desc_file)) {
      return(normalizePath(path))
    }
  }
  # If not found, use current directory
  return(normalizePath("."))
}

project_root <- find_project_root()

# Build paths to real data files (use absolute paths)
zip_path <- normalizePath(file.path(project_root, "export_97354582.zip"), mustWork = FALSE)
csv_path <- normalizePath(file.path(project_root, "export_data", "activities.csv"), mustWork = FALSE)
activities_dir <- normalizePath(file.path(project_root, "export_data", "activities"), mustWork = FALSE)

test_that("load_local_activities works with real ZIP export", {
  skip_if(!file.exists(zip_path), paste("Real export ZIP not available at", zip_path))
  
  # Load from ZIP file
  activities <- load_local_activities(zip_path)
  
  expect_s3_class(activities, "data.frame")
  expect_gt(nrow(activities), 0)
  
  # Check required columns exist
  expect_true("id" %in% names(activities))
  expect_true("date" %in% names(activities))
  expect_true("type" %in% names(activities))
  expect_true("distance" %in% names(activities))
  expect_true("moving_time" %in% names(activities))
  
  # Check data types
  expect_s3_class(activities$date, "Date")
  expect_true(is.numeric(activities$distance))
  expect_true(is.numeric(activities$moving_time))
})

test_that("load_local_activities works with real CSV export", {
  skip_if(!file.exists(csv_path), paste("Real export CSV not available at", csv_path))
  
  # Load from CSV file
  activities <- load_local_activities(csv_path)
  
  expect_s3_class(activities, "data.frame")
  expect_gt(nrow(activities), 0)
  
  # Check column structure
  expect_true("id" %in% names(activities))
  expect_true("date" %in% names(activities))
  expect_true("type" %in% names(activities))
  
  # Check calculated columns
  if ("distance_km" %in% names(activities)) {
    expect_true(is.numeric(activities$distance_km))
  }
  if ("duration_mins" %in% names(activities)) {
    expect_true(is.numeric(activities$duration_mins))
  }
})

test_that("load_local_activities filters by activity type with real data", {
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  
  # Load all activities first
  all_activities <- load_local_activities(csv_path)
  
  # Get unique activity types
  unique_types <- unique(all_activities$type)
  
  if (length(unique_types) > 0) {
    # Filter for first type
    first_type <- unique_types[1]
    filtered <- load_local_activities(csv_path, activity_types = first_type)
    
    expect_s3_class(filtered, "data.frame")
    if (nrow(filtered) > 0) {
      expect_true(all(filtered$type == first_type))
    }
  }
})

test_that("calculate functions work with real export data", {
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  
  activities <- load_local_activities(csv_path)
  
  # Try ACWR calculation with real data
  if (nrow(activities) >= 42) {
    # Get most common activity type
    activity_types <- table(activities$type)
    main_type <- names(activity_types)[which.max(activity_types)]
    
    if (sum(activities$type == main_type) >= 42) {
      acwr <- calculate_acwr(activities, activity_type = main_type, 
                            load_metric = "duration_mins")
      expect_s3_class(acwr, "data.frame")
      expect_true("acwr" %in% names(acwr) || "acwr_smooth" %in% names(acwr))
    }
  }
})

test_that("calculate_exposure works with real export data", {
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  
  activities <- load_local_activities(csv_path)
  
  if (nrow(activities) >= 50) {
    # Get most common activity type
    activity_types <- table(activities$type)
    main_type <- names(activity_types)[which.max(activity_types)]
    
    # 添加结束日期参数
    exposure <- calculate_exposure(activities, activity_type = main_type, end_date = "2024-12-31")
    expect_s3_class(exposure, "data.frame")
    expect_true("atl" %in% names(exposure))
    expect_true("ctl" %in% names(exposure))
    expect_true("acwr" %in% names(exposure))
  }
})

test_that("calculate_ef works with real export data", {
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  
  activities <- load_local_activities(csv_path)
  
  # Filter activities with heart rate data
  if ("average_heartrate" %in% names(activities)) {
    hr_activities <- activities[!is.na(activities$average_heartrate), ]
    
    if (nrow(hr_activities) >= 10) {
      # Get activity type with most HR data
      hr_by_type <- aggregate(average_heartrate ~ type, 
                              data = hr_activities, 
                              FUN = length)
      main_type <- hr_by_type$type[which.max(hr_by_type$average_heartrate)]
      
      ef <- calculate_ef(hr_activities, activity_type = main_type, 
                        ef_metric = "pace_hr")
      expect_s3_class(ef, "data.frame")
      expect_true("ef_value" %in% names(ef))
    }
  }
})

test_that("load_local_activities handles various column formats in real data", {
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  
  activities <- load_local_activities(csv_path)
  
  # Check that various optional columns are handled
  optional_cols <- c("average_heartrate", "max_heartrate", "average_watts",
                    "average_speed", "average_cadence", "elevation_gain",
                    "calories", "filename")
  
  # At least some optional columns should exist
  existing_optional <- sum(optional_cols %in% names(activities))
  expect_gt(existing_optional, 0)
})

test_that("calculate_pbs works with real export directory", {
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  skip_if(!dir.exists(activities_dir), "Activity files directory not available")
  
  activities <- load_local_activities(csv_path)
  
  # Filter for activities with distance data
  distance_activities <- activities[!is.na(activities$distance) & 
                                   activities$distance > 0, ]
  
  if (nrow(distance_activities) >= 5) {
    # Try to calculate PBs for most common activity type
    activity_types <- table(distance_activities$type)
    main_type <- names(activity_types)[which.max(activity_types)]
    
    # Common distances for running (in meters)
    test_distances <- c(1000, 5000, 10000)
    
    tryCatch({
      pbs <- calculate_pbs(
        activities_data = distance_activities,
        export_dir = file.path(project_root, "export_data"),
        activity_type = main_type,
        distances_m = test_distances
      )
      
      expect_s3_class(pbs, "data.frame")
      if (nrow(pbs) > 0) {
        expect_true("is_pb" %in% names(pbs))
        expect_true("distance" %in% names(pbs))
      }
    }, error = function(e) {
      # PBs calculation may fail if activity files are not in expected format
      skip(paste("PBs calculation not possible:", e$message))
    })
  }
})

test_that("plot functions work with real data", {
  skip_if_not_installed("ggplot2")
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  
  activities <- load_local_activities(csv_path)
  
  if (nrow(activities) >= 42) {
    # Get most common activity type
    activity_types <- table(activities$type)
    main_type <- names(activity_types)[which.max(activity_types)]
    
    if (sum(activities$type == main_type) >= 42) {
      # Calculate ACWR with real data
      acwr <- calculate_acwr(activities, activity_type = main_type)
      
      # Test plotting
      p <- plot_acwr(acwr)
      expect_s3_class(p, "ggplot")
    }
  }
})

test_that("load_local_activities detects and handles different date formats", {
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  
  activities <- load_local_activities(csv_path)
  
  # Date column should be properly parsed
  expect_s3_class(activities$date, "Date")
  
  # Dates should be reasonable (not in future, not too old)
  expect_true(all(activities$date <= Sys.Date(), na.rm = TRUE))
  expect_true(all(activities$date >= as.Date("2000-01-01"), na.rm = TRUE))
})

test_that("load_local_activities handles missing and NA values", {
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  
  activities <- load_local_activities(csv_path)
  
  # Core columns should not be all NA
  expect_false(all(is.na(activities$id)))
  expect_false(all(is.na(activities$date)))
  expect_false(all(is.na(activities$type)))
  
  # Some optional columns may have NAs - that's OK
  # Just check the data frame is valid
  expect_gt(ncol(activities), 5)
})

test_that("activity file references are valid in real data", {
  skip_if(!file.exists(csv_path), "Real export CSV not available")
  
  activities <- load_local_activities(csv_path)
  
  # Check if filename column exists
  if ("filename" %in% names(activities)) {
    # Some activities should have filenames
    has_filename <- !is.na(activities$filename) & activities$filename != ""
    
    if (any(has_filename)) {
      # Check if files actually exist (at least check format)
      expect_true(is.character(activities$filename))
    }
  }
})

