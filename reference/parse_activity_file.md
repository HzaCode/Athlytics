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
# Parse a built-in example TCX file
tcx_path <- system.file("extdata", "activities", "example.tcx", package = "Athlytics")
if (nzchar(tcx_path)) {
  streams <- parse_activity_file(tcx_path)
  if (!is.null(streams)) head(streams)
}
#>                  time latitude longitude elevation heart_rate power cadence
#> 1 2025-01-01 00:00:00   1.3000  103.8000        10        120   150      80
#> 2 2025-01-01 00:00:10   1.3001  103.8001        11        122   155      81
#> 3 2025-01-01 00:00:20   1.3002  103.8002        12        124   160      82
#>   distance speed
#> 1        0    NA
#> 2       15   1.5
#> 3       30   1.5

if (FALSE) { # \dontrun{
# Parse a FIT file from a Strava export
streams <- parse_activity_file("activity_12345.fit", export_dir = "strava_export/")
} # }
```
