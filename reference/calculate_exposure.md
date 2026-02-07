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
  end_date = Sys.Date(),
  verbose = FALSE
)
```

## Arguments

- activities_data:

  A data frame of activities from
  [`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md).
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

- verbose:

  Logical. If TRUE, prints progress messages. Default FALSE.

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
data(sample_exposure)
print(head(sample_exposure))
#> # A tibble: 6 × 5
#>   date       daily_load   ctl   atl  acwr
#>   <date>          <dbl> <dbl> <dbl> <dbl>
#> 1 2023-01-01        8.8    NA    NA    NA
#> 2 2023-01-02       23.7    NA    NA    NA
#> 3 2023-01-03       20.2    NA    NA    NA
#> 4 2023-01-04       35.1    NA    NA    NA
#> 5 2023-01-05       14.6    NA    NA    NA
#> 6 2023-01-06       31.9    NA    NA    NA

# Runnable example with dummy data:
end <- Sys.Date()
dates <- seq(end - 59, end, by = "day")
dummy_activities <- data.frame(
  date = dates,
  type = "Run",
  moving_time = rep(3600, length(dates)), # 1 hour
  distance = rep(10000, length(dates)),   # 10 km
  average_heartrate = rep(140, length(dates)),
  suffer_score = rep(50, length(dates)),
  tss = rep(50, length(dates)),
  stringsAsFactors = FALSE
)

# Calculate Exposure (ATL/CTL)
exposure_result <- calculate_exposure(
  activities_data = dummy_activities,
  activity_type = "Run",
  load_metric = "distance_km",
  acute_period = 7,
  chronic_period = 28,
  end_date = end
)
print(head(exposure_result))
#>         date daily_load atl ctl acwr
#> 1 2026-01-11         10  10  10    1
#> 2 2026-01-12         10  10  10    1
#> 3 2026-01-13         10  10  10    1
#> 4 2026-01-14         10  10  10    1
#> 5 2026-01-15         10  10  10    1
#> 6 2026-01-16         10  10  10    1

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
