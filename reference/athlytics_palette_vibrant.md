# Vibrant High-Contrast Palette

High-saturation palette optimized for presentations and posters. Maximum
visual impact while maintaining colorblind accessibility.

## Usage

``` r
athlytics_palette_vibrant()
```

## Value

A character vector of 8 hex color codes

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(data, aes(x, y, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = athlytics_palette_vibrant())
} # }
```
