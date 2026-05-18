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
  expect_contains(names(result), c("time", "latitude", "longitude", "elevation"))
  expect_contains(names(result), "heart_rate")
  expect_contains(names(result), "distance")
  expect_true(any(!is.na(result$heart_rate)))
  expect_true(any(!is.na(result$distance)))
})

test_that("parse_activity_file parses real GPX file correctly", {
  gpx_path <- system.file("extdata", "activities", "example.gpx", package = "Athlytics")
  result <- suppressWarnings(parse_activity_file(gpx_path))
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_contains(names(result), c("time", "latitude", "longitude", "elevation"))
  expect_true(any(!is.na(result$latitude)))
  expect_true(any(!is.na(result$longitude)))
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

test_that("parse_activity_file handles alternate TCX and GPX extension prefixes", {
  skip_if(!requireNamespace("xml2", quietly = TRUE), "xml2 not available")

  tcx_file <- tempfile(fileext = ".tcx")
  gpx_file <- tempfile(fileext = ".gpx")
  on.exit(unlink(c(tcx_file, gpx_file)), add = TRUE)

  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<TrainingCenterDatabase xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"',
    '  xmlns:pwr="http://www.garmin.com/xmlschemas/ActivityExtension/v2">',
    '  <Activities><Activity Sport="Running"><Lap StartTime="2024-01-01T00:00:00">',
    '    <Track>',
    '      <Trackpoint>',
    '        <Time>2024-01-01T00:00:00</Time>',
    '        <Position><LatitudeDegrees>31.1</LatitudeDegrees><LongitudeDegrees>121.1</LongitudeDegrees></Position>',
    '        <AltitudeMeters>10</AltitudeMeters>',
    '        <DistanceMeters>0</DistanceMeters>',
    '        <HeartRateBpm><Value>140</Value></HeartRateBpm>',
    '        <Cadence>80</Cadence>',
    '        <Extensions><pwr:TPX><pwr:Watts>220</pwr:Watts></pwr:TPX></Extensions>',
    '      </Trackpoint>',
    '    </Track>',
    '  </Lap></Activity></Activities>',
    '</TrainingCenterDatabase>'
  ), tcx_file)

  tcx_result <- parse_activity_file(tcx_file)
  expect_s3_class(tcx_result, "data.frame")
  expect_true(any(!is.na(tcx_result$power)))
  expect_true(any(!is.na(tcx_result$heart_rate)))
  expect_true(any(!is.na(tcx_result$cadence)))

  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<gpx version="1.1" creator="Athlytics test"',
    '  xmlns="http://www.topografix.com/GPX/1/1"',
    '  xmlns:altpx="http://www.garmin.com/xmlschemas/TrackPointExtension/v1">',
    '  <trk><trkseg>',
    '    <trkpt lat="31.1" lon="121.1">',
    '      <ele>10</ele>',
    '      <time>2024-01-01T00:00:00Z</time>',
    '      <extensions><altpx:TrackPointExtension><altpx:hr>142</altpx:hr><altpx:cad>81</altpx:cad><altpx:PowerInWatts>215</altpx:PowerInWatts></altpx:TrackPointExtension></extensions>',
    '    </trkpt>',
    '  </trkseg></trk>',
    '</gpx>'
  ), gpx_file)

  gpx_result <- parse_activity_file(gpx_file)
  expect_s3_class(gpx_result, "data.frame")
  expect_true(any(!is.na(gpx_result$heart_rate)))
  expect_true(any(!is.na(gpx_result$cadence)))
  expect_true(any(!is.na(gpx_result$power)))
})

test_that("parse_activity_file accepts TCX timestamps with trailing Z", {
  skip_if(!requireNamespace("xml2", quietly = TRUE), "xml2 not available")

  tcx_file <- tempfile(fileext = ".tcx")
  on.exit(unlink(tcx_file), add = TRUE)

  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<TrainingCenterDatabase xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2">',
    '  <Activities><Activity Sport="Running"><Lap StartTime="2024-01-01T00:00:00Z">',
    '    <Track>',
    '      <Trackpoint>',
    '        <Time>2024-01-01T00:00:00Z</Time>',
    '        <DistanceMeters>0</DistanceMeters>',
    '      </Trackpoint>',
    '      <Trackpoint>',
    '        <Time>2024-01-01T00:00:10Z</Time>',
    '        <DistanceMeters>25</DistanceMeters>',
    '      </Trackpoint>',
    '    </Track>',
    '  </Lap></Activity></Activities>',
    '</TrainingCenterDatabase>'
  ), tcx_file)

  tcx_result <- parse_activity_file(tcx_file)

  expect_s3_class(tcx_result, "data.frame")
  expect_equal(nrow(tcx_result), 2)
  expect_false(any(is.na(tcx_result$time)))
  expect_equal(as.numeric(diff(tcx_result$time)), 10)
})
