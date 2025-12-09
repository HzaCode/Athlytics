# ACWR Zone Colors

Standardized colors for ACWR risk zones following sports science
conventions.

## Usage

``` r
athlytics_colors_acwr_zones()
```

## Value

A named list with four color codes for ACWR zones:

- undertraining:

  Light blue for low load

- safe:

  Green for optimal training zone

- caution:

  Orange for moderate risk

- high_risk:

  Red for high injury risk

## Examples

``` r
# Get ACWR zone colors
colors <- athlytics_colors_acwr_zones()
colors$safe  # Returns green color code
#> [1] "#A9DFBF"
```
