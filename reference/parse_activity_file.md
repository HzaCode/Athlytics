# Parse Activity File (FIT, TCX, or GPX)

Parse activity files from Strava export data. Supports FIT, TCX, and GPX
formats (including .gz compressed files).

## Usage

``` r
parse_activity_file(file_path, export_dir = NULL)
```

## Arguments

- file_path:

  Path to the activity file (can be .fit, .tcx, .gpx, or .gz compressed)

- export_dir:

  Base directory of the Strava export (for resolving relative paths)

## Value

A data frame with columns: time, latitude, longitude, elevation,
heart_rate, power, cadence, speed (all optional depending on file
content). Returns NULL if file cannot be parsed or does not exist.

## Examples

``` r
if (FALSE) { # \dontrun{
# Parse a FIT file
streams <- parse_activity_file("activity_12345.fit", export_dir = "strava_export/")

# Parse a compressed GPX file
streams <- parse_activity_file("activity_12345.gpx.gz")
} # }
```
