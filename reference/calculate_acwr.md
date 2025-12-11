# Calculate Acute:Chronic Workload Ratio (ACWR)

This function calculates daily training load and derives acute
(short-term) and chronic (long-term) load averages, then computes their
ratio (ACWR). The ACWR helps identify periods of rapid training load
increases that may elevate injury risk.

**Key Concepts:**

- **Acute Load (ATL)**: Rolling average of recent training (default: 7
  days)

- **Chronic Load (CTL)**: Rolling average of longer-term training
  (default: 28 days)

- **ACWR**: Ratio of ATL to CTL (ATL / CTL)

- **Safe Zone**: ACWR between 0.8-1.3 (optimal training stimulus)

- **Danger Zone**: ACWR \> 1.5 (increased injury risk)

## Usage

``` r
calculate_acwr(
  activities_data,
  activity_type = NULL,
  load_metric = "duration_mins",
  acute_period = 7,
  chronic_period = 28,
  start_date = NULL,
  end_date = NULL,
  user_ftp = NULL,
  user_max_hr = NULL,
  user_resting_hr = NULL,
  smoothing_period = 7
)
```

## Arguments

- activities_data:

  A data frame of activities from
  [`load_local_activities()`](https://hezhiang.com/Athlytics/reference/load_local_activities.md).
  Must contain columns: `date`, `distance`, `moving_time`,
  `elapsed_time`, `average_heartrate`, `average_watts`, `type`,
  `elevation_gain`.

- activity_type:

  **Required** character vector. Filter activities by type (e.g.,
  `"Run"`, `"Ride"`). **Must specify** to avoid mixing incompatible load
  metrics.

- load_metric:

  Character string specifying the load calculation method:

  - `"duration_mins"`: Training duration in minutes (default)

  - `"distance_km"`: Distance in kilometers

  - `"elapsed_time_mins"`: Total elapsed time including stops

  - `"tss"`: Training Stress Score approximation using NP/FTP ratio
    (requires `user_ftp`)

  - `"hrss"`: Heart Rate Stress Score approximation using simplified
    TRIMP (requires `user_max_hr` and `user_resting_hr`)

  - `"elevation_gain_m"`: Elevation gain in meters

- acute_period:

  Integer. Number of days for the acute load window (default: 7).
  Represents recent training stimulus. Common values: 3-7 days.

- chronic_period:

  Integer. Number of days for the chronic load window (default: 28).
  Represents fitness/adaptation level. Must be greater than
  `acute_period`. Common values: 21-42 days.

- start_date:

  Optional. Analysis start date (YYYY-MM-DD string, Date, or POSIXct).
  Defaults to one year before `end_date`. Earlier data is used for
  calculating initial chronic load.

- end_date:

  Optional. Analysis end date (YYYY-MM-DD string, Date, or POSIXct).
  Defaults to current date (Sys.Date()).

- user_ftp:

  Numeric. Your Functional Threshold Power in watts. Required only when
  `load_metric = "tss"`. Used to normalize power-based training stress.

- user_max_hr:

  Numeric. Your maximum heart rate in bpm. Required only when
  `load_metric = "hrss"`. Used for heart rate reserve calculations.

- user_resting_hr:

  Numeric. Your resting heart rate in bpm. Required only when
  `load_metric = "hrss"`. Used for heart rate reserve calculations.

- smoothing_period:

  Integer. Number of days for smoothing the ACWR using a rolling mean
  (default: 7). Reduces day-to-day noise for clearer trend
  visualization.

## Value

A tibble with the following columns:

- date:

  Date (Date class)

- atl:

  Acute Training Load - rolling average over `acute_period` days
  (numeric)

- ctl:

  Chronic Training Load - rolling average over `chronic_period` days
  (numeric)

- acwr:

  Raw ACWR value (atl / ctl) (numeric)

- acwr_smooth:

  Smoothed ACWR using `smoothing_period` rolling mean (numeric)

## Details

Computes the Acute:Chronic Workload Ratio (ACWR) from local Strava
activity data using rolling average methods. ACWR is a key metric for
monitoring training load and injury risk in athletes (Gabbett, 2016;
Hulin et al., 2016).

**Algorithm:**

1.  **Daily Aggregation**: Sum all activities by date to compute daily
    load

2.  **Complete Time Series**: Fill missing days with zero load (critical
    for ACWR accuracy)

3.  **Acute Load (ATL)**: Rolling mean over `acute_period` days
    (default: 7)

4.  **Chronic Load (CTL)**: Rolling mean over `chronic_period` days
    (default: 28)

5.  **ACWR Calculation**: ATL / CTL (set to NA when CTL \< 0.01 to avoid
    division by zero)

6.  **Smoothing**: Optional rolling mean over `smoothing_period` days
    for visualization

**Data Requirements:** The function automatically fetches additional
historical data (chronic_period days before start_date) to ensure
accurate chronic load calculations at the analysis start point. Ensure
your Strava export contains sufficient historical activities.

**Load Metric Implementations:**

- `"tss"`: Uses normalized power (NP) and FTP to approximate Training
  Stress Score (TSS). Formula:
  `(duration_sec × NP × IF) / (FTP × 3600) × 100`, where `IF = NP/FTP`
  (equivalently: `hours × IF^2 × 100`).

- `"hrss"`: HR-based load using heart rate reserve (simplified TRIMP;
  **not** TrainingPeaks hrTSS). Formula:
  `duration_sec * (HR - resting_HR) / (max_HR - resting_HR)`.

**Interpretation Guidelines:**

- ACWR \< 0.8: May indicate detraining or insufficient load

- ACWR 0.8-1.3: "Sweet spot" - optimal training stimulus with lower
  injury risk

- ACWR 1.3-1.5: Caution zone - monitor for fatigue

- ACWR \> 1.5: High risk zone - consider load management

**Multi-Athlete Studies:** For cohort analyses, add an `athlete_id`
column before calculation and use `group_by(athlete_id)` with
[`group_modify()`](https://dplyr.tidyverse.org/reference/group_map.html).
See examples below and vignettes for details.

## References

Gabbett, T. J. (2016). The training-injury prevention paradox: should
athletes be training smarter and harder? *British Journal of Sports
Medicine*, 50(5), 273-280.

Hulin, B. T., et al. (2016). The acute:chronic workload ratio predicts
injury: high chronic workload may decrease injury risk in elite rugby
league players. *British Journal of Sports Medicine*, 50(4), 231-236.

## See also

[`plot_acwr`](https://hezhiang.com/Athlytics/reference/plot_acwr.md) for
visualization,
[`calculate_acwr_ewma`](https://hezhiang.com/Athlytics/reference/calculate_acwr_ewma.md)
for EWMA-based ACWR,
[`load_local_activities`](https://hezhiang.com/Athlytics/reference/load_local_activities.md)
for data loading,
[`cohort_reference`](https://hezhiang.com/Athlytics/reference/cohort_reference.md)
for multi-athlete comparisons

## Examples

``` r
# Example using simulated data (Note: sample data is pre-calculated, shown for demonstration)
data(athlytics_sample_acwr)
print(head(athlytics_sample_acwr))
#> # A tibble: 6 × 6
#>   athlete_id date         atl   ctl  acwr acwr_smooth
#>   <chr>      <date>     <dbl> <dbl> <dbl>       <dbl>
#> 1 Athlete_A  2023-01-01  45.6  44.6  1.02          NA
#> 2 Athlete_A  2023-01-02  45.6  44.8  1.02          NA
#> 3 Athlete_A  2023-01-03  46.4  44.5  1.04          NA
#> 4 Athlete_A  2023-01-04  46.5  44.9  1.04          NA
#> 5 Athlete_A  2023-01-05  46.6  44.8  1.04          NA
#> 6 Athlete_A  2023-01-06  47.5  45.6  1.04          NA

if (FALSE) { # \dontrun{
# Example using local Strava export data
# Step 1: Download your Strava data export
# Go to Strava.com > Settings > My Account > Download or Delete Your Account
# You'll receive a ZIP file via email (e.g., export_12345678.zip)

# Step 2: Load activities directly from ZIP (no extraction needed!)
activities <- load_local_activities("export_12345678.zip")

# Or from extracted CSV
activities <- load_local_activities("strava_export_data/activities.csv")

# Step 3: Calculate ACWR for Runs (using distance)
run_acwr <- calculate_acwr(activities_data = activities, 
                           activity_type = "Run",
                           load_metric = "distance_km")
print(tail(run_acwr))

# Calculate ACWR for Rides (using TSS, requires FTP)
ride_acwr_tss <- calculate_acwr(activities_data = activities,
                                activity_type = "Ride",
                                load_metric = "tss", 
                                user_ftp = 280)
print(tail(ride_acwr_tss))

# Plot the results
plot_acwr(run_acwr, highlight_zones = TRUE)

# Multi-athlete cohort analysis

# Load data for multiple athletes and add athlete_id
athlete1 <- load_local_activities("athlete1_export.zip") %>%
  dplyr::mutate(athlete_id = "athlete1")

athlete2 <- load_local_activities("athlete2_export.zip") %>%
  dplyr::mutate(athlete_id = "athlete2")

# Combine all athletes
cohort_data <- dplyr::bind_rows(athlete1, athlete2)

# Calculate ACWR for each athlete using group_modify()
cohort_acwr <- cohort_data %>%
  dplyr::group_by(athlete_id) %>%
  dplyr::group_modify(~ calculate_acwr(.x, 
                                 activity_type = "Run",
                                 load_metric = "duration_mins")) %>%
  dplyr::ungroup()

# View results
print(cohort_acwr)
} # }
```
