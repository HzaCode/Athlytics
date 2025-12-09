# Get Athlytics Theme

Publication-ready ggplot2 theme with sensible defaults for scientific
figures.

## Usage

``` r
theme_athlytics(base_size = 12, base_family = "")
```

## Arguments

- base_size:

  Base font size (default: 12)

- base_family:

  Font family (default: "")

## Value

A ggplot2 theme object that can be added to plots

## Examples

``` r
library(ggplot2)
# Apply theme to a plot
ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  theme_athlytics()

```
