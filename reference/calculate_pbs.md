# Calculate Personal Bests (PBs) from Local Strava Data

Tracks personal best times for standard distances (1k, 5k, 10k, half
marathon, marathon) by analyzing detailed activity files from Strava
export data.

## Usage

``` r
calculate_pbs(
  activities_data,
  export_dir = "strava_export_data",
  activity_type = "Run",
  start_date = NULL,
  end_date = NULL,
  distances_m = c(1000, 5000, 10000, 21097.5, 42195)
)
```

## Arguments

- activities_data:

  A data frame of activities from
  [`load_local_activities()`](https://hezhiang.com/Athlytics/reference/load_local_activities.md).
  Must contain columns: date, type, filename, distance.

- export_dir:

  Base directory of the Strava export containing the activities folder.
  Default is "strava_export_data".

- activity_type:

  Type of activities to analyze (typically "Run"). Default "Run".

- start_date:

  Optional start date for analysis (YYYY-MM-DD). Defaults to NULL (all
  dates).

- end_date:

  Optional end date for analysis (YYYY-MM-DD). Defaults to NULL (all
  dates).

- distances_m:

  Target distances in meters to track. Default: c(1000, 5000, 10000,
  21097.5, 42195) for 1k, 5k, 10k, half, full marathon.

## Value

A data frame with columns: activity_id, activity_date, distance,
elapsed_time, moving_time, time_seconds, cumulative_pb_seconds, is_pb,
distance_label, time_period

## Details

This function analyzes detailed activity files (FIT/TCX/GPX) to find the
fastest efforts at specified distances. It tracks cumulative personal
bests over time, showing when new PBs are set.

**Note**: Requires detailed activity files from your Strava export.
Activities must be long enough to contain the target distance segments.

## Examples

``` r
# Example using simulated data
data(athlytics_sample_pbs)
print(head(athlytics_sample_pbs))
#> # A tibble: 6 × 10
#>   activity_id activity_date       distance elapsed_time moving_time time_seconds
#>   <chr>       <dttm>                 <dbl>        <dbl>       <dbl>        <dbl>
#> 1 activity_1… 2023-03-12 00:00:00     1000         240         235.         240 
#> 2 activity_1… 2023-03-19 00:00:00     1000         237.        233.         237.
#> 3 activity_1… 2023-03-27 00:00:00     1000         235.        230.         235.
#> 4 activity_1… 2023-04-20 00:00:00     1000         232.        227.         232.
#> 5 activity_1… 2023-04-27 00:00:00     1000         229.        225.         229.
#> 6 activity_1… 2023-04-30 00:00:00     1000         226.        222.         226.
#> # ℹ 4 more variables: cumulative_pb_seconds <dbl>, is_pb <lgl>,
#> #   distance_label <fct>, time_period <Period>

if (FALSE) { # \dontrun{
# Load local activities
activities <- load_local_activities("strava_export_data/activities.csv")

# Calculate PBs for standard running distances
pbs_data <- calculate_pbs(
  activities_data = activities,
  export_dir = "strava_export_data",
  activity_type = "Run"
)
print(head(pbs_data))
} # }
```
