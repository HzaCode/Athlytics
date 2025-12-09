# Calculate Training Load Exposure (ATL, CTL, ACWR)

Calculates training load metrics like ATL, CTL, and ACWR from local
Strava data.

## Usage

``` r
calculate_exposure(
  activities_data,
  activity_type = c("Run", "Ride", "VirtualRide", "VirtualRun"),
  load_metric = "duration_mins",
  acute_period = 7,
  chronic_period = 42,
  user_ftp = NULL,
  user_max_hr = NULL,
  user_resting_hr = NULL,
  end_date = NULL
)
```

## Arguments

- activities_data:

  A data frame of activities from
  [`load_local_activities()`](https://hezhiang.com/Athlytics/reference/load_local_activities.md).
  Must contain columns: date, distance, moving_time, elapsed_time,
  average_heartrate, average_watts, type, elevation_gain.

- activity_type:

  Type(s) of activities to include (e.g., "Run", "Ride"). Default
  includes common run/ride types.

- load_metric:

  Method for calculating daily load (e.g., "duration_mins",
  "distance_km", "tss", "hrss"). Default "duration_mins".

- acute_period:

  Days for the acute load window (e.g., 7).

- chronic_period:

  Days for the chronic load window (e.g., 42). Must be greater than
  `acute_period`.

- user_ftp:

  Required if `load_metric = "tss"`. Your Functional Threshold Power.

- user_max_hr:

  Required if `load_metric = "hrss"`. Your maximum heart rate.

- user_resting_hr:

  Required if `load_metric = "hrss"`. Your resting heart rate.

- end_date:

  Optional. Analysis end date (YYYY-MM-DD string or Date). Defaults to
  today. The analysis period covers the `chronic_period` days ending on
  this date.

## Value

A data frame with columns: `date`, `daily_load`, `atl` (Acute Load),
`ctl` (Chronic Load), and `acwr` (Acute:Chronic Ratio) for the analysis
period.

## Details

Calculates daily load, ATL, CTL, and ACWR from Strava activities based
on the chosen metric and periods.

Provides data for `plot_exposure`. Requires extra prior data for
accurate initial CTL. Requires FTP/HR parameters for TSS/HRSS metrics.

## Examples

``` r
# Example using simulated data
data(athlytics_sample_exposure)
print(head(athlytics_sample_exposure))
#> # A tibble: 6 × 5
#>   date       daily_load   atl   ctl  acwr
#>   <date>          <dbl> <dbl> <dbl> <dbl>
#> 1 2023-07-01       41.4  45.9  40.7  1.13
#> 2 2023-07-02       73.4  47.4  41.6  1.14
#> 3 2023-07-03       24.2  50.6  42.7  1.18
#> 4 2023-07-04       38.8  49.9  43.2  1.15
#> 5 2023-07-05       50.2  48.0  43.7  1.10
#> 6 2023-07-06       69.4  49.7  44.7  1.11

if (FALSE) { # \dontrun{
# Example using local Strava export data
activities <- load_local_activities("strava_export_data/activities.csv")

# Calculate training load for Rides using TSS
ride_exposure_tss <- calculate_exposure(
  activities_data = activities,
  activity_type = "Ride",
  load_metric = "tss",
  user_ftp = 280,
  acute_period = 7,
  chronic_period = 28
)
print(head(ride_exposure_tss))

# Calculate training load for Runs using HRSS
run_exposure_hrss <- calculate_exposure(
  activities_data = activities,
  activity_type = "Run",
  load_metric = "hrss",
  user_max_hr = 190,
  user_resting_hr = 50
)
print(tail(run_exposure_hrss))
} # }
```
