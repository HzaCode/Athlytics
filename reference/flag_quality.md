# Flag Data Quality Issues in Activity Streams

Detects and flags potential data quality issues in activity stream data,
including HR/power spikes, GPS drift, and identifies steady-state
segments suitable for physiological metrics calculation.

## Usage

``` r
flag_quality(
  streams,
  sport = "Run",
  hr_range = c(30, 220),
  pw_range = c(0, 1500),
  max_run_speed = 7,
  max_ride_speed = 25,
  max_accel = 3,
  max_hr_jump = 10,
  max_pw_jump = 300,
  min_steady_minutes = 20,
  steady_cv_threshold = 8
)
```

## Arguments

- streams:

  A data frame containing activity stream data with time-series
  measurements. Expected columns: `time` (seconds), `heartrate` (bpm),
  `watts` (W), `velocity_smooth` or `speed` (m/s), `distance` (m).

- sport:

  Type of activity (e.g., "Run", "Ride"). Default "Run".

- hr_range:

  Valid heart rate range as c(min, max). Default c(30, 220).

- pw_range:

  Valid power range as c(min, max). Default c(0, 1500).

- max_run_speed:

  Maximum plausible running speed in m/s. Default 7.0 (approx. 2:23/km).

- max_ride_speed:

  Maximum plausible riding speed in m/s. Default 25.0 (approx. 90 km/h).

- max_accel:

  Maximum plausible acceleration in m/s². Default 3.0.

- max_hr_jump:

  Maximum plausible HR change per second (bpm/s). Default 10.

- max_pw_jump:

  Maximum plausible power change per second (W/s). Default 300.

- min_steady_minutes:

  Minimum duration (minutes) for steady-state segment. Default 20.

- steady_cv_threshold:

  Coefficient of variation threshold for steady-state (\\). Default 8.

## Value

A data frame identical to `streams` with additional flag columns:

- flag_hr_spike:

  Logical. TRUE if HR is out of range or has excessive jump.

- flag_pw_spike:

  Logical. TRUE if power is out of range or has excessive jump.

- flag_gps_drift:

  Logical. TRUE if speed or acceleration is implausible.

- flag_any:

  Logical. TRUE if any quality flag is raised.

- is_steady_state:

  Logical. TRUE if segment meets steady-state criteria.

- quality_score:

  Numeric 0-1. Proportion of clean data (1 = perfect).

## Details

This function performs several quality checks:

- **HR/Power Spikes**: Flags values outside physiological ranges or with
  sudden jumps (Delta HR \> 10 bpm/s, Delta P \> 300 W/s).

- **GPS Drift**: Flags implausible speeds or accelerations based on
  sport type.

- **Steady-State Detection**: Identifies segments with low variability
  (CV \< 8%) lasting \>= 20 minutes, suitable for EF/decoupling
  calculations.

The function is sport-aware and adjusts thresholds accordingly. All
thresholds are configurable to accommodate different athlete profiles
and data quality.

## Examples

``` r
# Create sample activity stream data
set.seed(42)
stream_data <- data.frame(
  time = seq(0, 3600, by = 1),
  heartrate = pmax(60, pmin(200, rnorm(3601, mean = 150, sd = 10))),
  watts = pmax(0, rnorm(3601, mean = 200, sd = 20)),
  velocity_smooth = pmax(0, rnorm(3601, mean = 3.5, sd = 0.3))
)

# Flag quality issues
flagged_data <- flag_quality(stream_data, sport = "Run")

# Check summary
cat("Quality score range:", range(flagged_data$quality_score), "\n")
#> Quality score range: 0.5242988 0.5242988 
cat("Flagged points:", sum(flagged_data$flag_any), "\n")
#> Flagged points: 1713 
```
