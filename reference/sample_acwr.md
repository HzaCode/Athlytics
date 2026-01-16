# Sample ACWR Data for Athlytics

A dataset containing pre-calculated Acute:Chronic Workload Ratio (ACWR)
and related metrics, derived from simulated Strava data. Used in
examples and tests.

## Usage

``` r
sample_acwr
```

## Format

A tibble with 365 rows and 5 variables:

- date:

  Date of the metrics, as a Date object.

- atl:

  Acute Training Load, as a numeric value.

- ctl:

  Chronic Training Load, as a numeric value.

- acwr:

  Acute:Chronic Workload Ratio, as a numeric value.

- acwr_smooth:

  Smoothed ACWR, as a numeric value.

## Source

Simulated data generated for package examples.
