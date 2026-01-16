# Script to generate sample_exposure data
# This script creates a simulated training exposure dataset for package examples and testing

library(dplyr)
library(tibble)

set.seed(321)

# Generate 365 days of data
n_days <- 365
start_date <- as.Date("2023-01-01")
dates <- seq(start_date, by = "day", length.out = n_days)

# Simulate daily training load (duration in minutes)
day_of_week <- as.POSIXlt(dates)$wday

# Training pattern: Mon/Wed/Fri = moderate, Tue/Thu = easy, Sat = long, Sun = rest
daily_load <- case_when(
  day_of_week == 0 ~ runif(n_days, 0, 20), # Sunday: rest/easy
  day_of_week == 1 ~ runif(n_days, 45, 75), # Monday: moderate
  day_of_week == 2 ~ runif(n_days, 30, 50), # Tuesday: easy
  day_of_week == 3 ~ runif(n_days, 50, 80), # Wednesday: moderate
  day_of_week == 4 ~ runif(n_days, 30, 50), # Thursday: easy
  day_of_week == 5 ~ runif(n_days, 45, 70), # Friday: moderate
  day_of_week == 6 ~ runif(n_days, 80, 150), # Saturday: long run
  TRUE ~ 0
)

# Add some complete rest days
rest_days <- sample(1:n_days, size = 40)
daily_load[rest_days] <- 0

# Add seasonal build-up (increasing towards spring/summer)
day_of_year <- as.numeric(format(dates, "%j"))
seasonal_factor <- 0.8 + 0.4 * sin(2 * pi * (day_of_year - 60) / 365)
daily_load <- daily_load * seasonal_factor

# Calculate rolling metrics
acute_window <- 7
chronic_window <- 28

atl <- zoo::rollmean(daily_load, k = acute_window, fill = NA, align = "right")
ctl <- zoo::rollmean(daily_load, k = chronic_window, fill = NA, align = "right")
acwr <- ifelse(ctl > 0, atl / ctl, NA)

# Create the dataset
sample_exposure <- tibble(
  date = dates,
  daily_load = round(daily_load, 1),
  ctl = round(ctl, 2),
  atl = round(atl, 2),
  acwr = round(acwr, 3)
)

# Keep all rows (NAs are expected for early dates)
# Save to data directory
save(sample_exposure, file = "data/sample_exposure.rda", compress = "xz")

message("sample_exposure.rda generated with ", nrow(sample_exposure), " rows.")
