## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# # CRAN (stable)
# install.packages("Athlytics")
# 
# # GitHub (latest features)
# remotes::install_github('HzaCode/Athlytics')

## ----eval=FALSE---------------------------------------------------------------
# library(Athlytics)
# library(dplyr)  # For data manipulation
# 
# # Load your activities
# activities <- load_local_activities("path/to/export_12345678.zip")
# 
# # View the first few rows
# head(activities)

## ----eval=FALSE---------------------------------------------------------------
# # How many activities do you have?
# nrow(activities)
# # Example output: [1] 847
# 
# # What sports are in your data?
# table(activities$sport)
# # Example output:
# #   Ride  Run  Swim
# #    312  498    37
# 
# # Date range
# range(activities$date, na.rm = TRUE)
# # Example output: [1] "2018-01-05" "2024-12-20"
# 
# # Key columns in the dataset
# names(activities)

## ----eval=FALSE---------------------------------------------------------------
# # Summary statistics
# summary(activities %>% select(distance_km, duration_mins, avg_hr))
# 
# # Check for missing heart rate data
# sum(!is.na(activities$avg_hr)) / nrow(activities) * 100
# # Shows % of activities with HR data
# 
# # Activities without HR data
# activities %>%
#   filter(is.na(avg_hr)) %>%
#   count(sport)

## ----eval=FALSE---------------------------------------------------------------
# # Only running activities
# runs <- activities %>%
#   filter(sport == "Run")
# 
# # Recent activities (last 6 months)
# recent <- activities %>%
#   filter(date >= Sys.Date() - 180)
# 
# # Runs with heart rate data from 2024
# runs_2024_hr <- activities %>%
#   filter(sport == "Run",
#          !is.na(avg_hr),
#          lubridate::year(date) == 2024)
# 
# # Long runs only (> 15 km)
# long_runs <- activities %>%
#   filter(sport == "Run", distance_km > 15)

## ----eval=FALSE---------------------------------------------------------------
# # Calculate ACWR for all running activities
# acwr_data <- calculate_acwr(
#   activities_data = runs,
#   activity_type = "Run",        # Filter by sport
#   load_metric = "duration_mins", # Can also be "distance_km" or "hrss"
#   acute_period = 7,              # 7-day rolling average
#   chronic_period = 28            # 28-day rolling average
# )
# 
# # View results
# head(acwr_data)

## ----eval=FALSE---------------------------------------------------------------
# # Basic plot
# plot_acwr(acwr_data)
# 
# # With risk zones highlighted (recommended)
# plot_acwr(acwr_data, highlight_zones = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # Identify high-risk periods
# high_risk <- acwr_data %>%
#   filter(acwr_smooth > 1.5) %>%
#   select(date, daily_load, acwr_smooth)
# 
# print(high_risk)
# 
# # Check recent trend
# recent_acwr <- acwr_data %>%
#   filter(date >= Sys.Date() - 60) %>%
#   arrange(desc(date))
# 
# head(recent_acwr, 10)

## ----eval=FALSE---------------------------------------------------------------
# # Calculate using HRSS (heart rate stress score)
# acwr_hrss <- calculate_acwr(
#   activities_data = runs,
#   load_metric = "hrss"  # Automatically calculated if avg_hr available
# )

## ----eval=FALSE---------------------------------------------------------------
# # For running (Pace/HR)
# ef_runs <- calculate_ef(
#   activities_data = runs,
#   activity_type = "Run",
#   ef_metric = "Pace_HR"  # Pace divided by HR
# )
# 
# # For cycling (Power/HR)
# rides <- activities %>% filter(sport == "Ride")
# ef_cycling <- calculate_ef(
#   activities_data = rides,
#   activity_type = "Ride",
#   ef_metric = "Power_HR"  # Power divided by HR
# )
# 
# # View results
# head(ef_runs)

## ----eval=FALSE---------------------------------------------------------------
# # Basic plot
# plot_ef(ef_runs)
# 
# # With smoothing line to see trend (recommended)
# plot_ef(ef_runs, add_trend_line = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # Calculate monthly average EF
# library(lubridate)
# 
# ef_monthly <- ef_runs %>%
#   mutate(month = floor_date(date, "month")) %>%
#   group_by(month) %>%
#   summarise(
#     mean_ef = mean(ef_value, na.rm = TRUE),
#     n_activities = n()
#   ) %>%
#   arrange(desc(month))
# 
# print(ef_monthly)
# 
# # Compare first vs last 3 months
# recent_ef <- ef_runs %>% filter(date >= Sys.Date() - 90) %>% pull(ef_value)
# baseline_ef <- ef_runs %>% filter(date < Sys.Date() - 90, date >= Sys.Date() - 180) %>% pull(ef_value)
# 
# cat(sprintf("Recent EF: %.2f\nBaseline EF: %.2f\nChange: %.1f%%\n",
#             mean(recent_ef, na.rm = TRUE),
#             mean(baseline_ef, na.rm = TRUE),
#             (mean(recent_ef, na.rm = TRUE) / mean(baseline_ef, na.rm = TRUE) - 1) * 100))

## ----eval=FALSE---------------------------------------------------------------
# # For running
# decoupling_runs <- calculate_decoupling(
#   activities_data = runs,
#   activity_type = "Run",
#   decouple_metric = "Pace_HR",
#   min_duration_mins = 60  # Only analyze runs ≥ 60 minutes
# )
# 
# # For cycling
# decoupling_rides <- calculate_decoupling(
#   activities_data = rides,
#   activity_type = "Ride",
#   decouple_metric = "Power_HR",
#   min_duration_mins = 90  # Longer threshold for cycling
# )
# 
# # View results
# head(decoupling_runs)

## ----eval=FALSE---------------------------------------------------------------
# # Basic plot
# plot_decoupling(decoupling_runs)
# 
# # With metric specification
# plot_decoupling(decoupling_runs, decouple_metric = "Pace_HR")

## ----eval=FALSE---------------------------------------------------------------
# # Recent decoupling average
# recent_decouple <- decoupling_runs %>%
#   filter(date >= Sys.Date() - 60) %>%
#   summarise(avg_decouple = mean(decoupling_pct, na.rm = TRUE))
# 
# if (recent_decouple$avg_decouple < 5) {
#   cat("Excellent aerobic base! Ready for higher intensity.\n")
# } else if (recent_decouple$avg_decouple < 10) {
#   cat("Good base, continue building aerobic foundation.\n")
# } else {
#   cat("High decoupling—focus on more easy, long runs.\n")
# }

## ----eval=FALSE---------------------------------------------------------------
# # Compare decoupling over time
# library(ggplot2)
# 
# decoupling_runs %>%
#   ggplot(aes(x = date, y = decoupling_pct)) +
#   geom_point(alpha = 0.6) +
#   geom_smooth(method = "loess", se = TRUE) +
#   geom_hline(yintercept = 5, linetype = "dashed", color = "green") +
#   geom_hline(yintercept = 10, linetype = "dashed", color = "orange") +
#   labs(title = "Decoupling Trend Over Time",
#        subtitle = "Lower values = better aerobic endurance",
#        x = "Date", y = "Decoupling (%)") +
#   theme_minimal()

## ----eval=FALSE---------------------------------------------------------------
# # Extract personal bests
# pbs <- calculate_pbs(
#   activities_data = runs,
#   activity_type = "Run"
# )
# 
# # View all PRs
# print(pbs)

## ----eval=FALSE---------------------------------------------------------------
# # Plot PR progression
# plot_pbs(pbs)
# 
# # Filter to specific distance
# pbs_5k <- pbs %>% filter(distance == "5k")
# print(pbs_5k)

## ----eval=FALSE---------------------------------------------------------------
# # Calculate exposure
# exposure <- calculate_exposure(
#   activities_data = runs,
#   activity_type = "Run",
#   load_metric = "duration_mins"
# )
# 
# # Plot with risk zones
# plot_exposure(exposure, highlight_zones = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# library(Athlytics)
# library(dplyr)
# library(ggplot2)
# 
# # ---- 1. Load and Filter Data ----
# activities <- load_local_activities("my_strava_export.zip")
# 
# # Focus on running activities with HR data
# runs <- activities %>%
#   filter(sport == "Run", !is.na(avg_hr))
# 
# cat(sprintf("Loaded %d running activities with HR data\n", nrow(runs)))
# 
# # ---- 2. Training Load Monitoring ----
# acwr_data <- calculate_acwr(
#   activities_data = runs,
#   load_metric = "duration_mins"
# )
# 
# # Check current training status
# current_acwr <- acwr_data %>%
#   filter(date >= Sys.Date() - 30) %>%
#   tail(1) %>%
#   pull(acwr_smooth)
# 
# cat(sprintf("Current ACWR: %.2f\n", current_acwr))
# 
# # Visualize
# p1 <- plot_acwr(acwr_data, highlight_zones = TRUE) +
#   labs(title = "6-Month Training Load Progression")
# print(p1)
# 
# # ---- 3. Aerobic Fitness Tracking ----
# ef_data <- calculate_ef(
#   activities_data = runs,
#   ef_metric = "Pace_HR"
# )
# 
# # Calculate fitness trend
# ef_trend <- ef_data %>%
#   mutate(month = lubridate::floor_date(date, "month")) %>%
#   group_by(month) %>%
#   summarise(mean_ef = mean(ef_value, na.rm = TRUE))
# 
# p2 <- plot_ef(ef_data, add_trend_line = TRUE) +
#   labs(title = "Aerobic Efficiency Trend")
# print(p2)
# 
# # ---- 4. Endurance Assessment ----
# # Only for long runs (> 60 min)
# decoupling_data <- calculate_decoupling(
#   activities_data = runs,
#   min_duration_mins = 60
# )
# 
# avg_decouple <- mean(decoupling_data$decoupling_pct, na.rm = TRUE)
# cat(sprintf("Average decoupling: %.1f%% (%s aerobic base)\n",
#             avg_decouple,
#             ifelse(avg_decouple < 5, "excellent",
#                    ifelse(avg_decouple < 10, "good", "needs work"))))
# 
# p3 <- plot_decoupling(decoupling_data) +
#   labs(title = "Cardiovascular Drift in Long Runs")
# print(p3)
# 
# # ---- 5. Export Results ----
# # Save plots
# ggsave("acwr_analysis.png", plot = p1, width = 10, height = 6, dpi = 300)
# ggsave("ef_trend.png", plot = p2, width = 10, height = 6, dpi = 300)
# ggsave("decoupling.png", plot = p3, width = 10, height = 6, dpi = 300)
# 
# # Export data for further analysis
# write.csv(acwr_data, "acwr_results.csv", row.names = FALSE)
# write.csv(ef_data, "ef_results.csv", row.names = FALSE)
# write.csv(decoupling_data, "decoupling_results.csv", row.names = FALSE)
# 
# cat("\nAnalysis complete! Results saved.\n")

## ----eval=FALSE---------------------------------------------------------------
# # Check activity types in your data
# table(activities$sport)
# 
# # Check for HR data availability
# sum(!is.na(activities$avg_hr))
# 
# # Verify date range
# range(activities$date, na.rm = TRUE)
# 
# # Try without filtering first
# test <- calculate_acwr(activities_data = activities, activity_type = NULL)

## ----eval=FALSE---------------------------------------------------------------
# # How much data do you have?
# date_span <- as.numeric(max(activities$date) - min(activities$date))
# cat(sprintf("Your data spans %d days\n", date_span))
# 
# # If < 28 days, you need more data or use shorter periods

## ----eval=FALSE---------------------------------------------------------------
# # Filter before calculating
# runs_with_hr <- runs %>% filter(!is.na(avg_hr))
# ef_data <- calculate_ef(runs_with_hr, ef_metric = "Pace_HR")

## ----eval=FALSE---------------------------------------------------------------
# # Check if data exists
# nrow(acwr_data)
# summary(acwr_data$acwr_smooth)
# 
# # Try basic R plot first
# plot(acwr_data$date, acwr_data$acwr_smooth, type = "l")

## -----------------------------------------------------------------------------
sessionInfo()

