# Script to generate sample_decoupling data
# This script creates a simulated decoupling dataset for package examples and testing

library(dplyr)
library(tibble)

set.seed(456)

# Generate ~1 year of data (not every day has a qualifying workout)
n_workouts <- 52 # Approximately weekly long runs
start_date <- as.Date("2023-01-01")

# Generate workout dates (roughly weekly with some variation)
workout_intervals <- round(rnorm(n_workouts, mean = 7, sd = 1.5))
workout_intervals[workout_intervals < 4] <- 4 # Minimum 4 days between
dates <- cumsum(c(0, workout_intervals[-n_workouts]))
dates <- start_date + dates

# Simulate decoupling values
# Good aerobic fitness: <5% decoupling
# Moderate: 5-10%
# Poor: >10%

# Simulate improving fitness over the year (decoupling decreasing)
base_decoupling <- seq(12, 4, length.out = n_workouts)
noise <- rnorm(n_workouts, mean = 0, sd = 2)
decoupling <- base_decoupling + noise

# Add some outliers (heat, illness, etc.)
outlier_idx <- sample(1:n_workouts, size = 5)
decoupling[outlier_idx] <- decoupling[outlier_idx] + runif(5, 5, 10)

# Ensure values are reasonable
decoupling <- pmax(decoupling, -2) # Can be slightly negative (rare)
decoupling <- pmin(decoupling, 25) # Cap at 25%

# Create the dataset
sample_decoupling <- tibble(
  date = dates,
  decoupling = round(decoupling, 2)
)

# Save to data directory
save(sample_decoupling, file = "data/sample_decoupling.rda", compress = "xz")

message("sample_decoupling.rda generated with ", nrow(sample_decoupling), " rows.")
