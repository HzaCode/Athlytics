# Script to regenerate sample_pbs data
library(dplyr)
library(lubridate)

set.seed(42)

start_date <- as.Date("2023-01-01")
dates <- seq(start_date, by = "1 month", length.out = 12)

# Base paces (min/km) — longer distance = slower pace
base_pace_1k <- 4.2 # 4:12/km for 1k → 252s total
base_pace_5k <- 4.5 # 4:30/km for 5k → 1350s total
base_pace_10k <- 4.8 # 4:48/km for 10k → 2880s total
base_pace_hm <- 5.1 # 5:06/km for HM → ~6444s (~1:47:24) total

# Improvement over year: ~8% (realistic for amateur)
improvement_rate <- 0.08

# Generate times with small noise (ensuring pace relationship preserved)
generate_times <- function(base_pace, distance_m, dates, noise_pct = 0.02) {
  n <- length(dates)
  # Improvement trend
  pace_trend <- base_pace * (1 - seq(0, improvement_rate, length.out = n))
  # Small noise (max 2% variation)
  noise <- runif(n, -noise_pct, noise_pct)
  paces <- pace_trend * (1 + noise)
  # Convert pace (min/km) to total seconds
  times <- paces * (distance_m / 1000) * 60
  round(times)
}

# 1k Data - monthly
times_1k <- generate_times(base_pace_1k, 1000, dates)
df_1k <- data.frame(
  activity_date = as.POSIXct(dates),
  distance = 1000,
  time_seconds = times_1k,
  distance_label = "1k"
)

# 5k Data - bi-monthly (6 points)
dates_5k <- seq(start_date, by = "2 months", length.out = 6)
times_5k <- generate_times(base_pace_5k, 5000, dates_5k)
df_5k <- data.frame(
  activity_date = as.POSIXct(dates_5k),
  distance = 5000,
  time_seconds = times_5k,
  distance_label = "5k"
)

# 10k Data - quarterly (4 points)
dates_10k <- seq(start_date, by = "3 months", length.out = 4)
times_10k <- generate_times(base_pace_10k, 10000, dates_10k)
df_10k <- data.frame(
  activity_date = as.POSIXct(dates_10k),
  distance = 10000,
  time_seconds = times_10k,
  distance_label = "10k"
)

# Half Marathon Data - quarterly (4 points, realistic race frequency)
dates_hm <- seq(start_date, by = "3 months", length.out = 4)
times_hm <- generate_times(base_pace_hm, 21097.5, dates_hm)
df_hm <- data.frame(
  activity_date = as.POSIXct(dates_hm),
  distance = 21097.5,
  time_seconds = times_hm,
  distance_label = "21.1k"
)

# Combine all data
all_data <- rbind(df_1k, df_5k, df_10k, df_hm) %>%
  arrange(activity_date) %>%
  mutate(
    activity_id = paste0("activity_", row_number()),
    elapsed_time = time_seconds,
    moving_time = time_seconds,
    time_period = seconds_to_period(time_seconds)
  )

# Calculate cumulative PBs and is_pb flag
sample_pbs <- all_data %>%
  group_by(distance) %>%
  arrange(activity_date) %>%
  mutate(
    cumulative_pb_seconds = cummin(time_seconds),
    is_pb = time_seconds == cumulative_pb_seconds,
    # Ensure distance_label is a factor with correct levels
    distance_label = factor(distance_label, levels = c("1k", "5k", "10k", "21.1k"))
  ) %>%
  ungroup() %>%
  # Select and order columns to match package documentation
  select(
    activity_id, activity_date, distance, elapsed_time, moving_time,
    time_seconds, cumulative_pb_seconds, is_pb, distance_label, time_period
  )

# Convert to tibble for consistency
sample_pbs <- as_tibble(sample_pbs)

# Save to data directory
save(sample_pbs, file = "data/sample_pbs.rda", compress = "xz")

# Verify pace consistency: longer distance must have slower pace per km
# Check 1: Average pace per distance
pace_check <- sample_pbs %>%
  group_by(distance) %>%
  summarise(
    avg_pace_per_km = mean(time_seconds / (distance / 1000)),
    .groups = "drop"
  ) %>%
  arrange(distance)

stopifnot(
  "Pace must increase with distance (longer = slower per km)" =
    all(diff(pace_check$avg_pace_per_km) > 0)
)

# Check 2: Every individual data point must also satisfy the pace ordering
# On any given date, if multiple distances are recorded, longer distance must
# have a slower per-km pace
dates_with_multi <- sample_pbs %>%
  group_by(activity_date) %>%
  filter(n_distinct(distance) > 1) %>%
  ungroup()

if (nrow(dates_with_multi) > 0) {
  per_point_check <- dates_with_multi %>%
    mutate(pace_per_km = time_seconds / (distance / 1000)) %>%
    group_by(activity_date) %>%
    arrange(distance) %>%
    mutate(pace_ok = pace_per_km >= cummax(pace_per_km)) %>%
    ungroup()

  stopifnot(
    "Individual data points must also satisfy pace ordering" =
      all(per_point_check$pace_ok)
  )
}

message("sample_pbs.rda regenerated with ", nrow(sample_pbs), " rows.")
message("Pace verification passed: longer distances have slower per-km pace.")
message("Individual data point verification passed.")
print(pace_check)
print(table(sample_pbs$distance_label))
print(table(sample_pbs$is_pb))
