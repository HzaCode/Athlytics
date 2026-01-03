# Calculate ACWR using EWMA Method with Confidence Bands

Calculates the Acute:Chronic Workload Ratio (ACWR) using Exponentially
Weighted Moving Average (EWMA) with optional bootstrap confidence bands.

## Usage

``` r
calculate_acwr_ewma(
  activities_data,
  activity_type = NULL,
  load_metric = "duration_mins",
  method = c("ra", "ewma"),
  acute_period = 7,
  chronic_period = 28,
  half_life_acute = 3.5,
  half_life_chronic = 14,
  start_date = NULL,
  end_date = NULL,
  user_ftp = NULL,
  user_max_hr = NULL,
  user_resting_hr = NULL,
  smoothing_period = 7,
  ci = FALSE,
  B = 200,
  block_len = 7,
  conf_level = 0.95
)
```

## Arguments

- activities_data:

  A data frame of activities from
  [`load_local_activities()`](https://hzacode.github.io/Athlytics/reference/load_local_activities.md).

- activity_type:

  Optional. Filter activities by type. Default NULL includes all.

- load_metric:

  Method for calculating daily load. Default "duration_mins".

- method:

  ACWR calculation method: "ra" (rolling average) or "ewma". Default
  "ra".

- acute_period:

  Days for acute window (for RA method). Default 7.

- chronic_period:

  Days for chronic window (for RA method). Default 28.

- half_life_acute:

  Half-life for acute EWMA in days. Default 3.5.

- half_life_chronic:

  Half-life for chronic EWMA in days. Default 14.

- start_date:

  Optional. Analysis start date. Defaults to one year ago.

- end_date:

  Optional. Analysis end date. Defaults to today.

- user_ftp:

  Required if `load_metric = "tss"`.

- user_max_hr:

  Required if `load_metric = "hrss"`.

- user_resting_hr:

  Required if `load_metric = "hrss"`.

- smoothing_period:

  Days for smoothing ACWR. Default 7.

- ci:

  Logical. Whether to calculate confidence bands (EWMA only). Default
  FALSE.

- B:

  Number of bootstrap iterations (if ci = TRUE). Default 200.

- block_len:

  Block length for moving-block bootstrap (days). Default 7.

- conf_level:

  Confidence level (0-1). Default 0.95 (95\\ CI).

## Value

A data frame with columns: `date`, `atl`, `ctl`, `acwr`, `acwr_smooth`,
and if `ci = TRUE` and `method = "ewma"`: `acwr_lower`, `acwr_upper`.

## Details

This function extends the basic ACWR calculation with two methods:

- **RA (Rolling Average)**: Traditional rolling mean approach (default).

- **EWMA (Exponentially Weighted Moving Average)**: Uses exponential
  decay with configurable half-lives. More responsive to recent changes.

**EWMA Formula**: The smoothing parameter alpha is calculated from
half-life: `alpha = ln(2) / half_life`. The EWMA update is:
`E_t = alpha * L_t + (1-alpha) * E_{t-1}` where L_t is daily load and
E_t is the exponentially weighted average.

**Confidence Bands**: When `ci = TRUE` and `method = "ewma"`, uses
**moving-block bootstrap** to estimate uncertainty. The daily load
sequence is resampled in weekly blocks (preserving within-week
correlation), ACWR is recalculated, and percentiles form the confidence
bands. This accounts for temporal correlation in training load patterns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load local activities
activities <- load_local_activities("export_12345678.zip")

# Calculate ACWR using Rolling Average (RA)
acwr_ra <- calculate_acwr_ewma(activities, method = "ra")

# Calculate ACWR using EWMA with confidence bands
acwr_ewma <- calculate_acwr_ewma(
  activities,
  method = "ewma",
  half_life_acute = 3.5,
  half_life_chronic = 14,
  ci = TRUE,
  B = 200
)

# Compare both methods
head(acwr_ewma)
} # }
```
