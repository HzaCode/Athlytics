# Load Activities from Local Strava Export

Reads and processes activity data from a local Strava export, supporting
both direct CSV files and compressed ZIP archives. This function
converts Strava export data to a format compatible with all Athlytics
analysis functions. Designed to work with Strava's official bulk data
export (Settings \> My Account \> Download or Delete Your Account \> Get
Started).

## Usage

``` r
load_local_activities(
  path = "strava_export_data/activities.csv",
  start_date = NULL,
  end_date = NULL,
  activity_types = NULL
)
```

## Arguments

- path:

  Path to activities.csv file OR a .zip archive from Strava export.
  Supports both CSV and ZIP formats. If a .zip file is provided, the
  function will automatically extract and read the activities.csv file
  from within the archive. Default is
  "strava_export_data/activities.csv".

- start_date:

  Optional. Start date (YYYY-MM-DD or Date/POSIXct) for filtering
  activities. Defaults to NULL (no filtering).

- end_date:

  Optional. End date (YYYY-MM-DD or Date/POSIXct) for filtering
  activities. Defaults to NULL (no filtering).

- activity_types:

  Optional. Character vector of activity types to include (e.g.,
  c("Run", "Ride")). Defaults to NULL (include all types).

## Value

A tibble of activity data with standardized column names compatible with
Athlytics functions. Key columns include:

- `id`: Activity ID (numeric)

- `name`: Activity name

- `type`: Activity type (Run, Ride, etc.)

- `start_date_local`: Activity start datetime (POSIXct)

- `date`: Activity date (Date)

- `distance`: Distance in meters (numeric)

- `moving_time`: Moving time in seconds (integer)

- `elapsed_time`: Elapsed time in seconds (integer)

- `average_heartrate`: Average heart rate (numeric)

- `average_watts`: Average power in watts (numeric)

- `elevation_gain`: Elevation gain in meters (numeric)

## Details

This function reads the activities.csv file from a Strava data export
and transforms the data to match the structure expected by Athlytics
analysis functions. The transformation includes:

- Standardizing column names for analysis functions

- Parsing dates into POSIXct format

- Converting distances to meters

- Converting times to seconds

- Filtering by date range and activity type if specified

**Language Note**: Strava export language must be set to **English** for
proper CSV parsing. Change this in Strava Settings \> Display
Preferences \> Language before requesting your data export.

**Privacy Note**: This function processes local export data only and
does not connect to the internet. Ensure you have permission to analyze
the data and follow applicable privacy regulations when using this data
for research purposes.

## Examples

``` r
# Example using built-in sample CSV
csv_path <- system.file("extdata", "activities.csv", package = "Athlytics")
if (nzchar(csv_path)) {
  activities <- load_local_activities(csv_path)
  head(activities)
}
#> # A tibble: 6 × 22
#>      id name           type  sport_type start_date_local    date       distance
#>   <dbl> <chr>          <chr> <chr>      <dttm>              <date>        <dbl>
#> 1     4 Afternoon Ride Ride  Ride       2024-10-01 11:35:00 2024-10-01    33892
#> 2     5 Tempo Run      Run   Run        2024-10-02 08:35:00 2024-10-02     7925
#> 3     6 Morning Run    Run   Run        2024-10-04 06:41:00 2024-10-04     6976
#> 4     7 Long Run       Run   Run        2024-10-05 07:44:00 2024-10-05    18440
#> 5     8 Morning Run    Run   Run        2024-10-07 06:25:00 2024-10-07     6834
#> 6     9 Afternoon Ride Ride  Ride       2024-10-08 10:39:00 2024-10-08    31874
#> # ℹ 15 more variables: moving_time <int>, elapsed_time <int>,
#> #   average_heartrate <dbl>, max_heartrate <dbl>, average_watts <dbl>,
#> #   max_watts <dbl>, weighted_average_watts <dbl>, elevation_gain <dbl>,
#> #   elevation_loss <dbl>, average_speed <dbl>, max_speed <dbl>,
#> #   average_gap <dbl>, calories <dbl>, relative_effort <dbl>, filename <chr>

if (FALSE) { # \dontrun{
# Load from a local Strava export ZIP archive
activities <- load_local_activities("export_12345678.zip")

# Filter by date and activity type
activities <- load_local_activities(
  path = "export_12345678.zip",
  start_date = "2023-01-01",
  end_date = "2023-12-31",
  activity_types = "Run"
)
} # }
```
