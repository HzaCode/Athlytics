# Nature-Inspired Color Palette

Professional, colorblind-friendly palette based on Nature journal's
visualization guidelines. Suitable for multi-series plots.

## Usage

``` r
athlytics_palette_nature()
```

## Value

A character vector of 9 hex color codes

## Examples

``` r
# View the palette colors
athlytics_palette_nature()
#> [1] "#E64B35" "#4DBBD5" "#00A087" "#3C5488" "#F39B7F" "#8491B4" "#91D1C2"
#> [8] "#DC0000" "#7E6148"

# Display as color swatches
barplot(rep(1, 9), col = athlytics_palette_nature(), border = NA)
```
