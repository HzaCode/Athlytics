# R/color_palettes.R
# Internal color palettes for Athlytics
# Default colors inspired by Nature, Science, and Cell journal guidelines
# All plots use these colors automatically - no user configuration needed

#' @keywords internal
NULL

#' Nature-Inspired Color Palette
#'
#' Professional, colorblind-friendly palette based on Nature journal's
#' visualization guidelines. Suitable for multi-series plots.
#'
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x, y, color = group)) +
#'   geom_line() +
#'   scale_color_manual(values = athlytics_palette_nature())
#' }
athlytics_palette_nature <- function() {
  c(
    "#E64B35",  # Red (primary data)
    "#4DBBD5",  # Cyan (secondary data)
    "#00A087",  # Teal (tertiary)
    "#3C5488",  # Navy blue
    "#F39B7F",  # Coral
    "#8491B4",  # Slate blue
    "#91D1C2",  # Mint green
    "#DC0000",  # Dark red
    "#7E6148"   # Brown
  )
}

#' Academic Muted Color Palette
#'
#' Low-saturation, elegant palette suitable for formal publications and
#' technical reports. Emphasizes clarity over visual impact.
#'
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x, y, color = group)) +
#'   geom_line() +
#'   scale_color_manual(values = athlytics_palette_academic())
#' }
athlytics_palette_academic <- function() {
  c(
    "#8C7A6B",  # Taupe
    "#A8968E",  # Warm gray
    "#C4B5A8",  # Light beige
    "#7B9FA3",  # Steel blue
    "#9DB4B8",  # Powder blue
    "#B8A68E",  # Sand
    "#8B9E9F",  # Sage
    "#B0A698"   # Stone (different shade)
  )
}

#' Vibrant High-Contrast Palette
#'
#' High-saturation palette optimized for presentations and posters.
#' Maximum visual impact while maintaining colorblind accessibility.
#'
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x, y, fill = category)) +
#'   geom_bar(stat = "identity") +
#'   scale_fill_manual(values = athlytics_palette_vibrant())
#' }
athlytics_palette_vibrant <- function() {
  c(
    "#FF6B6B",  # Coral red
    "#4ECDC4",  # Turquoise
    "#45B7D1",  # Sky blue
    "#FFA07A",  # Light salmon
    "#98D8C8",  # Seafoam
    "#FFE66D",  # Yellow
    "#A8E6CF",  # Mint
    "#FF8B94"   # Pink
  )
}

#' Science Magazine Palette
#'
#' Classic palette inspired by Science magazine's figure guidelines.
#' Conservative and widely accepted in scientific community.
#'
#' @export
athlytics_palette_science <- function() {
  c(
    "#003F5C",  # Dark blue
    "#58508D",  # Purple
    "#BC5090",  # Magenta
    "#FF6361",  # Coral
    "#FFA600",  # Orange
    "#7A5195",  # Plum
    "#EF5675",  # Rose
    "#FFC300"   # Gold
  )
}

#' Cell Journal Palette
#'
#' Modern palette based on Cell Press visualization standards.
#' Balances professional appearance with visual clarity.
#'
#' @export
athlytics_palette_cell <- function() {
  c(
    "#0173B2",  # Blue
    "#DE8F05",  # Orange
    "#029E73",  # Green
    "#CC78BC",  # Purple
    "#CA9161",  # Tan
    "#949494",  # Gray
    "#ECE133",  # Yellow
    "#56B4E9"   # Light blue
  )
}

#' ACWR Zone Colors
#'
#' Standardized colors for ACWR risk zones following sports science conventions.
#'
#' @export
athlytics_colors_acwr_zones <- function() {
  list(
    undertraining = "#AED6F1",  # Light blue
    safe = "#A9DFBF",           # Green
    caution = "#FAD7A0",        # Orange
    high_risk = "#F5B7B1"       # Red
  )
}

#' Training Load Colors
#'
#' Colors for acute and chronic training load visualization.
#'
#' @export
athlytics_colors_training_load <- function() {
  list(
    acute = "#E64B35",    # Red (short-term)
    chronic = "#4DBBD5",  # Blue (long-term)
    ratio = "#00A087"     # Teal (ACWR)
  )
}

#' Efficiency Factor Colors
#'
#' Colors for efficiency factor trends by activity type.
#'
#' @export
athlytics_colors_ef <- function() {
  list(
    run = "#3C5488",      # Navy
    ride = "#F39B7F",     # Coral
    swim = "#4DBBD5",     # Cyan
    other = "#8491B4"     # Slate
  )
}

#' Get Athlytics Theme
#'
#' Publication-ready ggplot2 theme with sensible defaults for scientific figures.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Font family (default: "")
#' @export
#' @import ggplot2
theme_athlytics <- function(base_size = 12, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Plot
      plot.title = ggplot2::element_text(
        face = "bold", 
        size = base_size * 1.2,
        hjust = 0,
        margin = ggplot2::margin(b = base_size * 0.5)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 0.9,
        color = "gray40",
        hjust = 0,
        margin = ggplot2::margin(b = base_size * 0.5)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.8,
        color = "gray50",
        hjust = 1,
        margin = ggplot2::margin(t = base_size * 0.5)
      ),
      
      # Axes
      axis.title = ggplot2::element_text(size = base_size, face = "bold"),
      axis.text = ggplot2::element_text(size = base_size * 0.9, color = "gray20"),
      axis.line = ggplot2::element_line(color = "gray40", linewidth = 0.5),
      
      # Legend
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size * 0.9),
      legend.key.size = ggplot2::unit(1, "lines"),
      
      # Panel
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      
      # Strip (for facets)
      strip.text = ggplot2::element_text(
        size = base_size,
        face = "bold",
        color = "gray20"
      ),
      strip.background = ggplot2::element_rect(
        fill = "gray95",
        color = NA
      )
    )
}

#' Apply Color Palette to ggplot
#'
#' Helper function to apply Athlytics color palettes to existing plots.
#'
#' @param palette_name Name of palette: "nature", "academic", "vibrant", "science", or "cell"
#' @param type Either "color" or "fill"
#' @export
scale_athlytics <- function(palette_name = "nature", type = "color") {
  palette_func <- switch(
    palette_name,
    nature = athlytics_palette_nature,
    academic = athlytics_palette_academic,
    vibrant = athlytics_palette_vibrant,
    science = athlytics_palette_science,
    cell = athlytics_palette_cell,
    athlytics_palette_nature  # default
  )
  
  colors <- palette_func()
  
  if (type == "color") {
    ggplot2::scale_color_manual(values = colors)
  } else {
    ggplot2::scale_fill_manual(values = colors)
  }
}


