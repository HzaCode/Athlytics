# Sample Training Load Exposure Data for Athlytics

This dataset contains daily training load, ATL, CTL, and ACWR, derived
from simulated Strava data. Used in examples and tests, particularly for
`plot_exposure`.

## Usage

``` r
sample_exposure
```

## Format

A tibble with 365 rows and 5 variables:

- date:

  Date of the metrics, as a Date object.

- daily_load:

  Calculated daily training load, as a numeric value.

- ctl:

  Chronic Training Load, as a numeric value.

- atl:

  Acute Training Load, as a numeric value.

- acwr:

  Acute:Chronic Workload Ratio, as a numeric value.

## Source

Simulated data generated for package examples.
