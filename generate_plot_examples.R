# Generate ALL example plots for documentation, README, and analysis_output
# This script creates images for all plotting functions using the current API
# Run after any visual changes to regenerate every figure in the project

devtools::load_all(".")
library(ggplot2)
library(dplyr)

# Create output directories
dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("analysis_output", showWarnings = FALSE, recursive = TRUE)

# Set consistent plot dimensions
plot_width <- 10
plot_height <- 6
dpi <- 300

# Load all sample data
data("sample_acwr")
data("sample_ef")
data("sample_decoupling")
data("sample_exposure")
data("sample_pbs")

set.seed(42) # Reproducible jitter for simulated multi-group data

# ============================================================
# Helper: create synthetic multi-group data from sample data
# ============================================================
make_multi_group <- function(df, id_col = "athlete_id", date_col = "date",
                             value_col = NULL, n_groups = 3) {
  groups <- paste0("Athlete ", LETTERS[seq_len(n_groups)])
  # Center noise around 1.0 so cohort straddles the original data
  offsets <- seq(-0.15, 0.15, length.out = n_groups)
  result <- lapply(seq_len(n_groups), function(i) {
    d <- df
    if (!is.null(value_col)) {
      noise <- runif(nrow(d), 0.95 + offsets[i], 1.05 + offsets[i])
      d[[value_col]] <- d[[value_col]] * noise
    }
    d[[id_col]] <- groups[i]
    d
  })
  dplyr::bind_rows(result)
}

# ============================================================
# 1. ACWR - Single (man/figures)
# ============================================================
message("1. Generating ACWR single plot...")
p_acwr <- plot_acwr(sample_acwr, highlight_zones = TRUE)
ggsave("man/figures/example_plot_acwr.png", p_acwr,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")

# ============================================================
# 2. ACWR - Multi-group (README + man/figures + analysis_output)
# ============================================================
message("2. Generating ACWR multi-group plot...")
acwr_multi <- make_multi_group(sample_acwr, value_col = "acwr_smooth")
p_acwr_multi <- plot_acwr(acwr_multi,
  group_var = "athlete_id",
  highlight_zones = TRUE,
  title = "ACWR: Multi-Athlete Comparison"
)
ggsave("man/figures/01b_acwr_multi_group.png", p_acwr_multi,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")
file.copy("man/figures/01b_acwr_multi_group.png",
          "analysis_output/01b_acwr_multi_group.png", overwrite = TRUE)

# ============================================================
# 3. ACWR Enhanced (man/figures)
# ============================================================
message("3. Generating ACWR enhanced plot...")
p_acwr_enh <- plot_acwr_enhanced(sample_acwr, show_ci = FALSE)
ggsave("man/figures/example_plot_acwr_enhanced.png", p_acwr_enh,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")

# ============================================================
# 4. ACWR Comparison (man/figures)
# ============================================================
message("4. Generating ACWR comparison plot...")
acwr_ewma_sim <- sample_acwr
acwr_ewma_sim$acwr_smooth <- acwr_ewma_sim$acwr_smooth *
  runif(nrow(acwr_ewma_sim), 0.95, 1.05)
p_acwr_cmp <- plot_acwr_comparison(sample_acwr, acwr_ewma_sim)
ggsave("man/figures/example_plot_acwr_comparison.png", p_acwr_cmp,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")

# ============================================================
# 5. EF - Single (man/figures)
# ============================================================
message("5. Generating EF single plot...")
p_ef <- plot_ef(sample_ef, add_trend_line = TRUE)
ggsave("man/figures/example_plot_ef.png", p_ef,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")

# ============================================================
# 6. EF - Multi-group (README + man/figures + analysis_output)
# ============================================================
message("6. Generating EF multi-group plot...")
ef_multi <- make_multi_group(sample_ef, value_col = "ef_value")
p_ef_multi <- plot_ef(ef_multi,
  group_var = "athlete_id",
  add_trend_line = TRUE,
  title = "Efficiency Factor: Multi-Athlete Comparison"
)
ggsave("man/figures/02b_ef_multi_group.png", p_ef_multi,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")
file.copy("man/figures/02b_ef_multi_group.png",
          "analysis_output/02b_ef_multi_group.png", overwrite = TRUE)

# ============================================================
# 7. Decoupling - Single (man/figures)
# ============================================================
message("7. Generating decoupling single plot...")
p_decouple <- plot_decoupling(sample_decoupling, add_trend_line = TRUE)
ggsave("man/figures/example_plot_decoupling.png", p_decouple,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")

# ============================================================
# 8. Decoupling - Multi-group (README + man/figures + analysis_output)
#    Note: plot_decoupling doesn't have group_var, so we facet manually
# ============================================================
message("8. Generating decoupling multi-group plot...")
decouple_multi <- make_multi_group(sample_decoupling, value_col = "decoupling")
p_decouple_multi <- ggplot2::ggplot(decouple_multi,
    ggplot2::aes(x = .data$date, y = .data$decoupling, fill = .data$athlete_id)) +
  ggplot2::geom_point(shape = 21, colour = "white", stroke = 0.5, alpha = 0.8, size = 2.2) +
  ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "#D9534F", alpha = 0.5) +
  ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.3) +
  ggplot2::geom_smooth(ggplot2::aes(color = .data$athlete_id),
    method = "loess", se = FALSE, linewidth = 0.8) +
  ggplot2::scale_fill_manual(values = athlytics_palette_nature()[1:3], name = "Athlete") +
  ggplot2::scale_color_manual(values = athlytics_palette_nature()[1:3], name = "Athlete") +
  ggplot2::scale_x_date(labels = english_month_year) +
  ggplot2::labs(
    title = "Aerobic Decoupling: Multi-Athlete Comparison",
    x = "Date", y = "Decoupling (%)"
  ) +
  theme_athlytics()
ggsave("man/figures/05b_decoupling_multi_group.png", p_decouple_multi,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")
file.copy("man/figures/05b_decoupling_multi_group.png",
          "analysis_output/05b_decoupling_multi_group.png", overwrite = TRUE)

# ============================================================
# 9. Exposure (man/figures)
# ============================================================
message("9. Generating exposure plot...")
p_exposure <- plot_exposure(sample_exposure, risk_zones = TRUE, show_date_color = TRUE)
ggsave("man/figures/example_plot_exposure.png", p_exposure,
       width = plot_width, height = plot_height + 2, dpi = dpi, bg = "white")

# ============================================================
# 10. PBs (man/figures)
# ============================================================
message("10. Generating PBs plot...")
if (!is.null(sample_pbs) && nrow(sample_pbs) > 0) {
  p_pbs <- plot_pbs(sample_pbs, add_trend_line = FALSE)
  ggsave("man/figures/example_plot_pbs.png", p_pbs,
         width = 12, height = 8, dpi = dpi, bg = "white")
}

# ============================================================
# 11. Cohort reference (man/figures + analysis_output)
# ============================================================
message("11. Generating cohort reference plot...")
acwr_cohort <- make_multi_group(sample_acwr, value_col = "acwr_smooth", n_groups = 5)
ref <- calculate_cohort_reference(acwr_cohort, metric = "acwr_smooth")
individual <- sample_acwr
p_ref <- plot_acwr_enhanced(individual,
  reference_data = ref,
  show_ci = FALSE,
  show_reference = TRUE,
  title = "Individual vs Cohort Reference"
)
ggsave("man/figures/advanced_cohort_reference.png", p_ref,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")
file.copy("man/figures/advanced_cohort_reference.png",
          "analysis_output/advanced_cohort_reference.png", overwrite = TRUE)

# ============================================================
# 12. Reference example with borderline (man/figures)
# ============================================================
message("12. Generating reference example plot...")
p_ref_example <- ggplot2::ggplot(sample_acwr, ggplot2::aes(x = date, y = acwr_smooth)) +
  plot_lines(color = "#c00000", linewidth = 1) +
  ggplot2::geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", alpha = 0.4) +
  ggplot2::scale_x_date(labels = english_month_year) +
  ggplot2::labs(
    title = "Individual Metric with Cohort Reference",
    subtitle = "Example showing reference bands",
    x = "Date", y = "ACWR"
  ) +
  theme_athlytics()
ggsave("man/figures/example_plot_with_reference.png", p_ref_example,
       width = plot_width, height = plot_height, dpi = dpi, bg = "white")

message("\nAll plots generated successfully!")
message("Locations: man/figures/ and analysis_output/")
