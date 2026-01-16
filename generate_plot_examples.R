# Generate example plots for documentation
# This script creates example images for all plotting functions

library(Athlytics)
library(ggplot2)

# Create output directory
dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)

# Set consistent plot dimensions
plot_width <- 8
plot_height <- 5
dpi <- 150

# 1. plot_acwr() example
message("Generating plot_acwr example...")
data("sample_acwr")
if (!is.null(sample_acwr) && nrow(sample_acwr) > 0) {
  p1 <- plot_acwr(acwr_df = sample_acwr)
  ggsave("man/figures/example_plot_acwr.png", p1, 
         width = plot_width, height = plot_height, dpi = dpi)
  message("✓ Saved: man/figures/example_plot_acwr.png")
}

# 2. plot_acwr_enhanced() example
message("Generating plot_acwr_enhanced example...")
if (!is.null(sample_acwr) && nrow(sample_acwr) > 0) {
  p2 <- plot_acwr_enhanced(acwr_df = sample_acwr)
  ggsave("man/figures/example_plot_acwr_enhanced.png", p2, 
         width = plot_width, height = plot_height, dpi = dpi)
  message("✓ Saved: man/figures/example_plot_acwr_enhanced.png")
}

# 3. plot_acwr_comparison() example
message("Generating plot_acwr_comparison example...")
if (!is.null(sample_acwr) && nrow(sample_acwr) > 0) {
  # Create EWMA version for comparison
  acwr_ewma <- sample_acwr
  acwr_ewma$acwr_smooth <- acwr_ewma$acwr_smooth * runif(nrow(acwr_ewma), 0.95, 1.05)
  
  p3 <- plot_acwr_comparison(
    acwr_ra = sample_acwr,
    acwr_ewma = acwr_ewma
  )
  ggsave("man/figures/example_plot_acwr_comparison.png", p3, 
         width = plot_width, height = plot_height, dpi = dpi)
  message("✓ Saved: man/figures/example_plot_acwr_comparison.png")
}

# 4. plot_ef() example
message("Generating plot_ef example...")
data("sample_ef")
if (!is.null(sample_ef) && nrow(sample_ef) > 0) {
  p4 <- plot_ef(ef_df = sample_ef)
  ggsave("man/figures/example_plot_ef.png", p4, 
         width = plot_width, height = plot_height, dpi = dpi)
  message("✓ Saved: man/figures/example_plot_ef.png")
}

# 5. plot_decoupling() example
message("Generating plot_decoupling example...")
data("sample_decoupling")
if (!is.null(sample_decoupling) && nrow(sample_decoupling) > 0) {
  p5 <- plot_decoupling(decoupling_df = sample_decoupling)
  ggsave("man/figures/example_plot_decoupling.png", p5, 
         width = plot_width, height = plot_height, dpi = dpi)
  message("✓ Saved: man/figures/example_plot_decoupling.png")
}

# 6. plot_exposure() example
message("Generating plot_exposure example...")
data("sample_exposure")
if (!is.null(sample_exposure) && nrow(sample_exposure) > 0) {
  p6 <- plot_exposure(exposure_df = sample_exposure, risk_zones = TRUE)
  ggsave("man/figures/example_plot_exposure.png", p6, 
         width = plot_width, height = plot_height, dpi = dpi)
  message("✓ Saved: man/figures/example_plot_exposure.png")
}

# 7. plot_pbs() example
message("Generating plot_pbs example...")
data("sample_pbs")
if (!is.null(sample_pbs) && nrow(sample_pbs) > 0) {
  # Prepare data
  sample_pbs_for_plot <- sample_pbs
  if ("date" %in% names(sample_pbs_for_plot) && !"activity_date" %in% names(sample_pbs_for_plot)) {
    names(sample_pbs_for_plot)[names(sample_pbs_for_plot) == "date"] <- "activity_date"
  }
  if ("activity_date" %in% names(sample_pbs_for_plot)) {
    sample_pbs_for_plot$activity_date <- as.Date(sample_pbs_for_plot$activity_date)
  }
  
  req_dist_meters <- NULL
  if ("distance" %in% names(sample_pbs_for_plot)) {
    req_dist_meters <- unique(sample_pbs_for_plot$distance)
  } else if ("distance_target_m" %in% names(sample_pbs_for_plot)) {
    req_dist_meters <- unique(sample_pbs_for_plot$distance_target_m)
  }
  
  if (!is.null(req_dist_meters) && length(req_dist_meters) > 0) {
    p7 <- plot_pbs(pbs_df = sample_pbs_for_plot, 
                   activity_type = "Run", 
                   distance_meters = req_dist_meters)
    ggsave("man/figures/example_plot_pbs.png", p7, 
           width = plot_width, height = plot_height * 1.5, dpi = dpi)
    message("✓ Saved: man/figures/example_plot_pbs.png")
  }
}

# 8. plot_with_reference() example
message("Generating plot_with_reference example...")
if (!is.null(sample_acwr) && nrow(sample_acwr) > 0) {
  # Create a simple reference band
  reference_data <- data.frame(
    lower = 0.8,
    upper = 1.3,
    label = "Sweet Spot"
  )
  
  p8 <- ggplot(sample_acwr, aes(x = date, y = acwr_smooth)) +
    geom_line(color = athlytics_palette_nature()[1], linewidth = 1.2) +
    geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", alpha = 0.5) +
    labs(
      title = "Individual Metric with Cohort Reference",
      subtitle = "Example showing reference bands",
      x = "Date",
      y = "ACWR"
    ) +
    theme_athlytics()
  
  ggsave("man/figures/example_plot_with_reference.png", p8, 
         width = plot_width, height = plot_height, dpi = dpi)
  message("✓ Saved: man/figures/example_plot_with_reference.png")
}

message("\n✅ All example plots generated successfully!")
message("📁 Location: man/figures/")
