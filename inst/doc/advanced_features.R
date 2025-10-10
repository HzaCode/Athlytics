## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# library(Athlytics)
# library(dplyr)
# 
# # Load a single activity with stream data
# activities <- load_local_activities("export_12345678.zip")
# 
# # Parse stream data from a FIT/TCX file
# # (Assuming you have stream data - this is a conceptual example)
# stream_data <- parse_activity_file("path/to/activity.fit")
# 
# # Flag quality issues
# flagged_data <- flag_quality(
#   stream_data,
#   sport = "Run",
#   hr_range = c(30, 220),           # Valid HR range
#   pw_range = c(0, 1500),            # Valid power range
#   max_run_speed = 7.0,              # Max speed (m/s) ≈ 2:23/km pace
#   max_hr_jump = 10,                 # Max HR change per second
#   max_pw_jump = 300,                # Max power change per second
#   min_steady_minutes = 20,          # Min duration for steady-state
#   steady_cv_threshold = 8           # CV% threshold for steady-state
# )
# 
# # View quality summary
# summary_stats <- quality_summary(flagged_data)
# print(summary_stats)

## ----eval=FALSE---------------------------------------------------------------
# # Load activities
# activities <- load_local_activities("export_12345678.zip")
# 
# # Calculate ACWR using Rolling Average (traditional)
# acwr_ra <- calculate_acwr_ewma(
#   activities,
#   activity_type = "Run",
#   method = "ra",
#   acute_period = 7,
#   chronic_period = 28,
#   load_metric = "duration_mins"
# )
# 
# # Calculate ACWR using EWMA
# acwr_ewma <- calculate_acwr_ewma(
#   activities,
#   activity_type = "Run",
#   method = "ewma",
#   half_life_acute = 3.5,    # Acute load half-life (days)
#   half_life_chronic = 14,   # Chronic load half-life (days)
#   load_metric = "duration_mins"
# )
# 
# # Compare the two methods
# library(ggplot2)
# ggplot() +
#   geom_line(data = acwr_ra, aes(x = date, y = acwr_smooth),
#             color = "blue", size = 1, linetype = "solid") +
#   geom_line(data = acwr_ewma, aes(x = date, y = acwr_smooth),
#             color = "red", size = 1, linetype = "dashed") +
#   labs(title = "ACWR: Rolling Average vs EWMA",
#        subtitle = "Blue = RA (7:28d) | Red = EWMA (3.5:14d half-life)",
#        x = "Date", y = "ACWR") +
#   theme_minimal()

## ----eval=FALSE---------------------------------------------------------------
# # Calculate EWMA with 95% confidence bands
# acwr_ewma_ci <- calculate_acwr_ewma(
#   activities,
#   activity_type = "Run",
#   method = "ewma",
#   half_life_acute = 3.5,
#   half_life_chronic = 14,
#   load_metric = "duration_mins",
#   ci = TRUE,                # Enable confidence intervals
#   B = 200,                  # Bootstrap iterations
#   block_len = 7,            # Block length (days) for bootstrap
#   conf_level = 0.95         # 95% CI
# )
# 
# # Plot with confidence bands
# ggplot(acwr_ewma_ci, aes(x = date)) +
#   geom_ribbon(aes(ymin = acwr_lower, ymax = acwr_upper),
#               fill = "gray70", alpha = 0.5) +
#   geom_line(aes(y = acwr_smooth), color = "black", size = 1) +
#   geom_hline(yintercept = c(0.8, 1.3), linetype = "dotted", color = "green") +
#   geom_hline(yintercept = 1.5, linetype = "dotted", color = "red") +
#   labs(title = "ACWR with 95% Confidence Bands",
#        subtitle = "Gray band = bootstrap confidence interval",
#        x = "Date", y = "ACWR") +
#   theme_minimal()

## ----eval=FALSE---------------------------------------------------------------
# # Load data for multiple athletes
# athlete1 <- load_local_activities("athlete1_export.zip") %>%
#   mutate(athlete_id = "athlete1", sex = "M", age_band = "25-35")
# 
# athlete2 <- load_local_activities("athlete2_export.zip") %>%
#   mutate(athlete_id = "athlete2", sex = "F", age_band = "25-35")
# 
# athlete3 <- load_local_activities("athlete3_export.zip") %>%
#   mutate(athlete_id = "athlete3", sex = "M", age_band = "35-45")
# 
# # Combine
# cohort_data <- bind_rows(athlete1, athlete2, athlete3)
# 
# # Calculate ACWR for each athlete
# cohort_acwr <- cohort_data %>%
#   group_by(athlete_id) %>%
#   group_modify(~ calculate_acwr_ewma(.x, method = "ewma")) %>%
#   ungroup() %>%
#   left_join(
#     cohort_data %>% select(athlete_id, sex, age_band) %>% distinct(),
#     by = "athlete_id"
#   )

## ----eval=FALSE---------------------------------------------------------------
# # Calculate cohort reference by sport and sex
# reference <- cohort_reference(
#   cohort_acwr,
#   metric = "acwr_smooth",
#   by = c("sport", "sex"),           # Group by sport and sex
#   probs = c(0.05, 0.25, 0.5, 0.75, 0.95),  # Percentiles
#   min_athletes = 3                  # Minimum athletes per group
# )
# 
# # View reference structure
# head(reference)

## ----eval=FALSE---------------------------------------------------------------
# # Extract one athlete's data
# individual <- cohort_acwr %>%
#   filter(athlete_id == "athlete1")
# 
# # Plot with cohort reference
# p <- plot_with_reference(
#   individual = individual,
#   reference = reference %>% filter(sex == "M"),  # Match athlete's group
#   metric = "acwr_smooth",
#   bands = c("p25_p75", "p05_p95", "p50")
# )
# 
# print(p)

## ----eval=FALSE---------------------------------------------------------------
# library(Athlytics)
# library(dplyr)
# library(ggplot2)
# 
# # --- Step 1: Load multi-athlete cohort ---
# cohort_files <- c("athlete1_export.zip", "athlete2_export.zip", "athlete3_export.zip")
# cohort_data <- lapply(cohort_files, function(file) {
#   load_local_activities(file) %>%
#     mutate(athlete_id = tools::file_path_sans_ext(basename(file)))
# }) %>% bind_rows()
# 
# # --- Step 2: Quality control (conceptual - for stream data) ---
# # If you have stream data, flag quality before calculating metrics
# # cohort_data <- cohort_data %>%
# #   group_by(athlete_id) %>%
# #   mutate(quality_score = calculate_quality_score(streams))
# 
# # --- Step 3: Calculate ACWR with EWMA + CI ---
# cohort_acwr <- cohort_data %>%
#   group_by(athlete_id) %>%
#   group_modify(~ calculate_acwr_ewma(
#     .x,
#     method = "ewma",
#     half_life_acute = 3.5,
#     half_life_chronic = 14,
#     load_metric = "duration_mins",
#     ci = TRUE,
#     B = 100  # Reduced for speed in example
#   )) %>%
#   ungroup()
# 
# # --- Step 4: Calculate cohort reference ---
# reference <- cohort_reference(
#   cohort_acwr,
#   metric = "acwr_smooth",
#   by = c("sport"),
#   probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
#   min_athletes = 3
# )
# 
# # --- Step 5: Visualize individual with confidence + cohort ---
# individual <- cohort_acwr %>% filter(athlete_id == "athlete1")
# 
# p <- plot_with_reference(individual, reference, metric = "acwr_smooth") +
#   # Add individual's confidence bands
#   geom_ribbon(data = individual,
#               aes(x = date, ymin = acwr_lower, ymax = acwr_upper),
#               fill = "blue", alpha = 0.2)
# 
# print(p)
# 
# # --- Step 6: Export for statistical analysis ---
# write.csv(cohort_acwr, "cohort_acwr_with_ci.csv", row.names = FALSE)
# write.csv(reference, "cohort_reference.csv", row.names = FALSE)

## -----------------------------------------------------------------------------
sessionInfo()

