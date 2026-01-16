# Calculate Cohort Reference Percentiles

Calculates reference percentiles for a metric across a cohort of
athletes, stratified by specified grouping variables (e.g., sport, sex,
age band).

## Usage

``` r
calculate_cohort_reference(
  data,
  metric = "acwr_smooth",
  by = c("sport"),
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  min_athletes = 5,
  date_col = "date"
)

cohort_reference(
  data,
  metric = "acwr_smooth",
  by = c("sport"),
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  min_athletes = 5,
  date_col = "date"
)
```

## Arguments

- data:

  A data frame containing metric values for multiple athletes. Must
  include columns: `date`, `athlete_id`, and the metric column.

- metric:

  Name of the metric column to calculate percentiles for (e.g., "acwr",
  "acwr_smooth", "ef", "decoupling"). Default "acwr_smooth".

- by:

  Character vector of grouping variables. Options: "sport", "sex",
  "age_band", "athlete_id". Default c("sport").

- probs:

  Numeric vector of probabilities for percentiles (0-1). Default c(0.05,
  0.25, 0.50, 0.75, 0.95) for 5th, 25th, 50th, 75th, 95th percentiles.

- min_athletes:

  Minimum number of athletes required per group to calculate valid
  percentiles. Default 5.

- date_col:

  Name of the date column. Default "date".

## Value

A long-format data frame with columns:

- date:

  Date

- ...:

  Grouping variables (as specified in `by`)

- percentile:

  Percentile label (e.g., "p05", "p25", "p50", "p75", "p95")

- value:

  Metric value at that percentile

- n_athletes:

  Number of athletes contributing to this percentile

## Details

This function creates cohort-level reference bands for comparing
individual athlete metrics to their peers. Common use cases:

- Compare an athlete's ACWR trend to team averages

- Identify outliers (athletes outside P5-P95 range)

- Track team-wide trends over time

**Important**: Percentile bands represent **population variability**,
not statistical confidence intervals for individual values.

## Examples

``` r
# Example using sample data to create a mock cohort
data("sample_acwr", package = "Athlytics")

# Simulate a cohort by duplicating with different athlete IDs
cohort_mock <- dplyr::bind_rows(
  dplyr::mutate(sample_acwr, athlete_id = "A1", sport = "Run"),
  dplyr::mutate(sample_acwr,
    athlete_id = "A2", sport = "Run",
    acwr_smooth = acwr_smooth * runif(nrow(sample_acwr), 0.9, 1.1)
  ),
  dplyr::mutate(sample_acwr,
    athlete_id = "A3", sport = "Run",
    acwr_smooth = acwr_smooth * runif(nrow(sample_acwr), 0.85, 1.15)
  )
)

# Calculate reference percentiles (min_athletes = 2 for demo)
reference <- calculate_cohort_reference(cohort_mock,
  metric = "acwr_smooth",
  by = "sport", min_athletes = 2
)
#> Calculating percentiles for metric 'acwr_smooth' grouped by: sport
#> Reference calculated for 332 date-group combinations.
head(reference)
#> # A tibble: 6 × 5
#>   date       sport n_athletes percentile value
#>   <date>     <chr>      <int> <chr>      <dbl>
#> 1 2023-02-03 Run            3 p05        0.859
#> 2 2023-02-03 Run            3 p25        0.872
#> 3 2023-02-03 Run            3 p50        0.888
#> 4 2023-02-03 Run            3 p75        0.931
#> 5 2023-02-03 Run            3 p95        0.965
#> 6 2023-02-04 Run            3 p05        0.775

if (FALSE) { # \dontrun{
# Full workflow with real data - Load activities for multiple athletes
athlete1 <- load_local_activities("athlete1_export.zip") %>%
  mutate(athlete_id = "athlete1")
athlete2 <- load_local_activities("athlete2_export.zip") %>%
  mutate(athlete_id = "athlete2")
athlete3 <- load_local_activities("athlete3_export.zip") %>%
  mutate(athlete_id = "athlete3")

# Combine data
cohort_data <- bind_rows(athlete1, athlete2, athlete3)

# Calculate ACWR for each athlete
cohort_acwr <- cohort_data %>%
  group_by(athlete_id) %>%
  group_modify(~ calculate_acwr_ewma(.x))

# Calculate reference percentiles
reference <- calculate_cohort_reference(
  cohort_acwr,
  metric = "acwr_smooth",
  by = c("sport"),
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95)
)

# Plot individual against cohort
plot_with_reference(
  individual = cohort_acwr %>% filter(athlete_id == "athlete1"),
  reference = reference
)
} # }
```
