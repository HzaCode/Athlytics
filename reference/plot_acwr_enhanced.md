# Enhanced ACWR Plot with Confidence Bands and Reference

Creates a comprehensive ACWR visualization with optional confidence
bands and cohort reference percentiles.

## Usage

``` r
plot_acwr_enhanced(
  acwr_data,
  reference_data = NULL,
  show_ci = TRUE,
  show_reference = TRUE,
  reference_bands = c("p25_p75", "p05_p95", "p50"),
  highlight_zones = TRUE,
  title = NULL,
  subtitle = NULL,
  method_label = NULL
)
```

## Arguments

- acwr_data:

  A data frame from
  [`calculate_acwr_ewma()`](https://hezhiang.com/Athlytics/reference/calculate_acwr_ewma.md)
  containing ACWR values.

- reference_data:

  Optional. A data frame from
  [`cohort_reference()`](https://hezhiang.com/Athlytics/reference/cohort_reference.md)
  for adding cohort reference bands.

- show_ci:

  Logical. Whether to show confidence bands (if available in data).
  Default TRUE.

- show_reference:

  Logical. Whether to show cohort reference bands (if provided). Default
  TRUE.

- reference_bands:

  Which reference bands to show. Default c("p25_p75", "p05_p95", "p50").

- highlight_zones:

  Logical. Whether to highlight ACWR risk zones. Default TRUE.

- title:

  Plot title. Default NULL (auto-generated).

- subtitle:

  Plot subtitle. Default NULL (auto-generated).

- method_label:

  Optional label for the method used (e.g., "RA", "EWMA"). Default NULL.

## Value

A ggplot object.

## Details

This enhanced plot function combines multiple visualization layers:

- Risk zone shading (sweet spot: 0.8-1.3, caution: 1.3-1.5, high risk:
  \>1.5)

- Cohort reference percentile bands (if provided)

- Bootstrap confidence bands (if available in data)

- Individual ACWR trend line

The layering order (bottom to top):

1.  Risk zones (background)

2.  Cohort reference bands (P5-P95, then P25-P75)

3.  Confidence intervals (individual uncertainty)

4.  ACWR line (individual trend)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load activities
activities <- load_local_activities("export.zip")

# Calculate ACWR with EWMA and confidence bands
acwr <- calculate_acwr_ewma(
  activities,
  method = "ewma",
  ci = TRUE,
  B = 200
)

# Basic enhanced plot
plot_acwr_enhanced(acwr)

# With cohort reference
reference <- cohort_reference(cohort_data, metric = "acwr_smooth")
plot_acwr_enhanced(acwr, reference_data = reference)
} # }
```
