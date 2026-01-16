# Efficiency Factor Colors

Colors for efficiency factor trends by activity type.

## Usage

``` r
athlytics_colors_ef()
```

## Value

A named list with four color codes by sport:

- run:

  Navy blue for running

- ride:

  Coral for cycling

- swim:

  Cyan for swimming

- other:

  Slate for other activities

## Examples

``` r
# Get EF colors by sport
colors <- athlytics_colors_ef()
colors$run # Navy for running
#> [1] "#3C5488"
```
