# Script to generate sample_ef data
# This script creates a simulated Efficiency Factor dataset for package examples and testing

library(dplyr)
library(tibble)

set.seed(789)

# Generate EF data for runs and rides
n_runs <- 30
n_rides <- 20

start_date <- as.Date("2023-01-01")

# Generate run dates (2-3 times per week)
run_intervals <- round(runif(n_runs, 2, 4))
run_dates <- start_date + cumsum(c(0, run_intervals[-n_runs]))

# Generate ride dates (1-2 times per week)
ride_intervals <- round(runif(n_rides, 4, 8))
ride_dates <- start_date + cumsum(c(0, ride_intervals[-n_rides]))

# Simulate EF values
# Running EF typically ranges from 1.0 to 2.0 (pace per HR)
# Improving fitness shows increasing EF over time
run_base_ef <- seq(1.2, 1.6, length.out = n_runs)
run_ef <- run_base_ef + rnorm(n_runs, 0, 0.08)

# Cycling EF (power per HR) - different scale
ride_base_ef <- seq(1.8, 2.3, length.out = n_rides)
ride_ef <- ride_base_ef + rnorm(n_rides, 0, 0.1)

# Create the dataset
sample_ef <- bind_rows(
  tibble(
    date = run_dates,
    activity_type = "Run",
    ef_value = round(run_ef, 3)
  ),
  tibble(
    date = ride_dates,
    activity_type = "Ride",
    ef_value = round(ride_ef, 3)
  )
) %>%
  arrange(date)

# Save to data directory
save(sample_ef, file = "data/sample_ef.rda", compress = "xz")

message("sample_ef.rda generated with ", nrow(sample_ef), " rows.")
print(table(sample_ef$activity_type))
