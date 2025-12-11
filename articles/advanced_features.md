# Advanced Features: EWMA, Quality Control, and Cohort Analysis

## Introduction

This vignette demonstrates the advanced features of Athlytics for sports
physiology analysis:

1.  **Data Quality Control**: Detecting and flagging HR/power spikes,
    GPS drift
2.  **ACWR with EWMA**: Exponentially weighted moving average with
    confidence bands
3.  **Cohort Reference Analysis**: Multi-athlete percentile comparisons

These features make Athlytics suitable for both individual training
analysis and multi-athlete research studies.

------------------------------------------------------------------------

## 1. Data Quality Control

Before calculating physiological metrics, it’s crucial to ensure data
quality. The
[`flag_quality()`](https://hezhiang.com/Athlytics/reference/flag_quality.md)
function detects common issues in activity stream data.

### Basic Quality Checking

``` r
library(Athlytics)
library(dplyr)

# Load a single activity with stream data
activities <- load_local_activities("export_12345678.zip")

# Parse stream data from a FIT/TCX file
# (Assuming you have stream data - this is a conceptual example)
stream_data <- parse_activity_file("path/to/activity.fit")

# Flag quality issues
flagged_data <- flag_quality(
  stream_data,
  sport = "Run",
  hr_range = c(30, 220),           # Valid HR range
  pw_range = c(0, 1500),            # Valid power range
  max_run_speed = 7.0,              # Max speed (m/s) ≈ 2:23/km pace
  max_hr_jump = 10,                 # Max HR change per second
  max_pw_jump = 300,                # Max power change per second
  min_steady_minutes = 20,          # Min duration for steady-state
  steady_cv_threshold = 8           # CV% threshold for steady-state
)

# View quality summary
summary_stats <- quality_summary(flagged_data)
print(summary_stats)
```

**Output interpretation:** - `quality_score`: Overall quality (0-1),
where 1 = perfect - `flagged_pct`: Percentage of data points with
issues - `steady_state_pct`: Percentage suitable for EF/decoupling
calculations

### Why Quality Control Matters

**Without quality control:** - HR spikes from sensor errors inflate HRSS
calculations - GPS drift creates artificially high speeds, affecting
pace metrics - Non-steady-state segments contaminate Efficiency Factor
(EF) calculations

**With quality control:** - Only clean data contributes to metrics -
Steady-state detection ensures valid EF/decoupling calculations -
Researchers can report data quality in publications

------------------------------------------------------------------------

## 2. ACWR with EWMA Method

Traditional ACWR uses rolling averages (RA). The EWMA method offers: -
**Greater sensitivity** to recent training changes - **Customizable
decay rates** via half-life parameters - **Confidence bands** via
bootstrap to quantify uncertainty

### Comparing RA vs EWMA

``` r
# Load activities
activities <- load_local_activities("export_12345678.zip")

# Calculate ACWR using Rolling Average (traditional)
acwr_ra <- calculate_acwr_ewma(
  activities,
  activity_type = "Run",
  method = "ra",
  acute_period = 7,
  chronic_period = 28,
  load_metric = "duration_mins"
)

# Calculate ACWR using EWMA
acwr_ewma <- calculate_acwr_ewma(
  activities,
  activity_type = "Run",
  method = "ewma",
  half_life_acute = 3.5,    # Acute load half-life (days)
  half_life_chronic = 14,   # Chronic load half-life (days)
  load_metric = "duration_mins"
)

# Compare the two methods
library(ggplot2)
ggplot() +
  geom_line(data = acwr_ra, aes(x = date, y = acwr_smooth), 
            color = "blue", size = 1, linetype = "solid") +
  geom_line(data = acwr_ewma, aes(x = date, y = acwr_smooth), 
            color = "red", size = 1, linetype = "dashed") +
  labs(title = "ACWR: Rolling Average vs EWMA",
       subtitle = "Blue = RA (7:28d) | Red = EWMA (3.5:14d half-life)",
       x = "Date", y = "ACWR") +
  theme_minimal()
```

**Key differences:** - **RA**: Equal weight to all days in window;
sudden jumps when old workouts exit window - **EWMA**: Exponential
decay; smooth transitions; more responsive to recent changes

### Adding Confidence Bands

Confidence bands quantify uncertainty in ACWR estimates, essential for
research reporting.

``` r
# Calculate EWMA with 95% confidence bands
acwr_ewma_ci <- calculate_acwr_ewma(
  activities,
  activity_type = "Run",
  method = "ewma",
  half_life_acute = 3.5,
  half_life_chronic = 14,
  load_metric = "duration_mins",
  ci = TRUE,                # Enable confidence intervals
  B = 200,                  # Bootstrap iterations
  block_len = 7,            # Block length (days) for bootstrap
  conf_level = 0.95         # 95% CI
)

# Plot with confidence bands
ggplot(acwr_ewma_ci, aes(x = date)) +
  geom_ribbon(aes(ymin = acwr_lower, ymax = acwr_upper), 
              fill = "gray70", alpha = 0.5) +
  geom_line(aes(y = acwr_smooth), color = "black", size = 1) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dotted", color = "green") +
  geom_hline(yintercept = 1.5, linetype = "dotted", color = "red") +
  labs(title = "ACWR with 95% Confidence Bands",
       subtitle = "Gray band = bootstrap confidence interval",
       x = "Date", y = "ACWR") +
  theme_minimal()
```

**Interpretation:** - **Gray band**: 95% confidence interval from
moving-block bootstrap - **Wider bands** = more uncertainty (e.g.,
during high load variability) - **Narrower bands** = more stable
estimates

#### Technical Note: Bootstrap Method

The confidence bands use **moving-block bootstrap**: 1. Daily load
sequence is resampled in weekly blocks (preserves within-week
correlation) 2. ACWR is recalculated for each bootstrap sample (B = 200
iterations) 3. 2.5th and 97.5th percentiles form the 95% confidence band

This approach is computationally efficient and doesn’t require
distributional assumptions.

------------------------------------------------------------------------

## 3. Cohort Reference Analysis

When analyzing multiple athletes (teams, research cohorts), it’s
valuable to compare individuals to group norms.

### Multi-Athlete Setup

``` r
# Load data for multiple athletes
athlete1 <- load_local_activities("athlete1_export.zip") %>%
  mutate(athlete_id = "athlete1", sex = "M", age_band = "25-35")

athlete2 <- load_local_activities("athlete2_export.zip") %>%
  mutate(athlete_id = "athlete2", sex = "F", age_band = "25-35")

athlete3 <- load_local_activities("athlete3_export.zip") %>%
  mutate(athlete_id = "athlete3", sex = "M", age_band = "35-45")

# Combine
cohort_data <- bind_rows(athlete1, athlete2, athlete3)

# Calculate ACWR for each athlete
cohort_acwr <- cohort_data %>%
  group_by(athlete_id) %>%
  group_modify(~ calculate_acwr_ewma(.x, method = "ewma")) %>%
  ungroup() %>%
  left_join(
    cohort_data %>% select(athlete_id, sex, age_band) %>% distinct(),
    by = "athlete_id"
  )
```

### Calculate Reference Percentiles

``` r
# Calculate cohort reference by sport and sex
reference <- cohort_reference(
  cohort_acwr,
  metric = "acwr_smooth",
  by = c("sport", "sex"),           # Group by sport and sex
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),  # Percentiles
  min_athletes = 3                  # Minimum athletes per group
)

# View reference structure
head(reference)
```

**Output:**

    # A tibble: 6 × 5
      date       sport sex   percentile value n_athletes
      <date>     <chr> <chr> <chr>      <dbl>      <int>
    1 2024-01-01 Run   M     p05        0.65          5
    2 2024-01-01 Run   M     p25        0.82          5
    3 2024-01-01 Run   M     p50        0.98          5
    4 2024-01-01 Run   M     p75        1.15          5
    5 2024-01-01 Run   M     p95        1.38          5

### Plot Individual vs Cohort

``` r
# Extract one athlete's data
individual <- cohort_acwr %>% 
  filter(athlete_id == "athlete1")

# Plot with cohort reference
p <- plot_with_reference(
  individual = individual,
  reference = reference %>% filter(sex == "M"),  # Match athlete's group
  metric = "acwr_smooth",
  bands = c("p25_p75", "p05_p95", "p50")
)

print(p)
```

**Interpretation:** - **Black line**: Individual athlete’s ACWR trend -
**Dark shaded area (P25-P75)**: “Normal” range (50% of cohort) - **Light
shaded area (P5-P95)**: Extended range (90% of cohort) - **Dashed line
(P50)**: Median (cohort center)

**Use cases:** - **Outlier detection**: Athletes consistently above P95
or below P5 - **Load matching**: Ensure similar load profiles when
comparing injury rates - **Benchmarking**: Compare individual’s
trajectory to team norms

------------------------------------------------------------------------

## 4. Integrated Workflow Example

Here’s a complete research workflow combining all features:

``` r
library(Athlytics)
library(dplyr)
library(ggplot2)

# --- Step 1: Load multi-athlete cohort ---
cohort_files <- c("athlete1_export.zip", "athlete2_export.zip", "athlete3_export.zip")
cohort_data <- lapply(cohort_files, function(file) {
  load_local_activities(file) %>%
    mutate(athlete_id = tools::file_path_sans_ext(basename(file)))
}) %>% bind_rows()

# --- Step 2: Quality control (conceptual - for stream data) ---
# If you have stream data, flag quality before calculating metrics
# cohort_data <- cohort_data %>%
#   group_by(athlete_id) %>%
#   mutate(quality_score = calculate_quality_score(streams))

# --- Step 3: Calculate ACWR with EWMA + CI ---
cohort_acwr <- cohort_data %>%
  group_by(athlete_id) %>%
  group_modify(~ calculate_acwr_ewma(
    .x,
    method = "ewma",
    half_life_acute = 3.5,
    half_life_chronic = 14,
    load_metric = "duration_mins",
    ci = TRUE,
    B = 100  # Reduced for speed in example
  )) %>%
  ungroup()

# --- Step 4: Calculate cohort reference ---
reference <- cohort_reference(
  cohort_acwr,
  metric = "acwr_smooth",
  by = c("sport"),
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  min_athletes = 3
)

# --- Step 5: Visualize individual with confidence + cohort ---
individual <- cohort_acwr %>% filter(athlete_id == "athlete1")

p <- plot_with_reference(individual, reference, metric = "acwr_smooth") +
  # Add individual's confidence bands
  geom_ribbon(data = individual, 
              aes(x = date, ymin = acwr_lower, ymax = acwr_upper),
              fill = "blue", alpha = 0.2)

print(p)

# --- Step 6: Export for statistical analysis ---
write.csv(cohort_acwr, "cohort_acwr_with_ci.csv", row.names = FALSE)
write.csv(reference, "cohort_reference.csv", row.names = FALSE)
```

------------------------------------------------------------------------

## 5. Methodological Notes for JOSS/Research

### ACWR Method Selection

| Method                   | Pros                                            | Cons                                    | Recommended Use                                 |
|--------------------------|-------------------------------------------------|-----------------------------------------|-------------------------------------------------|
| **RA (Rolling Average)** | Stable, well-established, simple interpretation | Sudden jumps, equal weight to all days  | **Descriptive studies**, replicating prior work |
| **EWMA**                 | Smooth, responsive, customizable decay          | More parameters, less established norms | **Monitoring**, sensitive change detection      |

**Default parameters:** - RA: 7-day acute, 28-day chronic (most common
in literature) - EWMA: 3.5-day acute half-life, 14-day chronic half-life
(equivalent decay)

### Confidence Bands Limitations

- **Not causal inference**: Bands reflect sampling variability, not
  prediction of injury
- **Assumes stationarity**: If training pattern changes fundamentally,
  past data may not be representative
- **Block length matters**: Default 7-day blocks preserve weekly
  structure; adjust if training cycle differs

### Cohort Reference Interpretation

⚠️ **Critical distinction**: Percentile bands are **descriptive
population variability**, NOT **confidence intervals** for an
individual’s “true” value.

**Correct interpretation:** - “Athlete was above the 75th percentile of
the cohort” ✅ - “Athlete’s ACWR was significantly higher than cohort”
❌ (requires statistical test)

**Minimum sample sizes:** - P25-P75: n ≥ 5 athletes - P5-P95: n ≥ 20
athletes (for stable extremes)

------------------------------------------------------------------------

## 6. FAQ for Advanced Features

**Q: How do I choose EWMA half-life?**  
A: Start with defaults (3.5:14d ≈ 7:28d RA). For more responsive
monitoring, reduce both proportionally (e.g., 2.5:10d). For smoother
trends, increase (e.g., 5:20d).

**Q: Bootstrap is slow. Can I reduce B?**  
A: Yes. B = 50–100 is often sufficient for visual inspection. For
publication, use B = 200–500.

**Q: What if my cohort has \< 5 athletes?**  
A: Set `min_athletes = 3`, but report this limitation. Percentiles with
n \< 5 are unstable.

**Q: Can I use these methods for injury prediction?**  
A: **No**. These are **descriptive monitoring tools**. Injury prediction
requires proper case-control studies, controlling for confounders, and
validation. See Carey et al. (2018) for discussion of ACWR limitations.

**Q: How do I cite Athlytics?**  
A: Use the standard software citation format with version number and
repository URL. See the main tutorial for the complete citation.

------------------------------------------------------------------------

## Session Info

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.54         cachem_1.1.0      knitr_1.50        htmltools_0.5.9  
#>  [9] rmarkdown_2.30    lifecycle_1.0.4   cli_3.6.5         sass_0.4.10      
#> [13] pkgdown_2.2.0     textshaping_1.0.4 jquerylib_0.1.4   systemfonts_1.3.1
#> [17] compiler_4.5.2    tools_4.5.2       ragg_1.5.0        bslib_0.9.0      
#> [21] evaluate_1.0.5    yaml_2.3.12       jsonlite_2.0.0    rlang_1.1.6      
#> [25] fs_1.6.6          htmlwidgets_1.6.4
```
