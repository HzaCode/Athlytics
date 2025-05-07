## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5,
  warning = FALSE, # Suppress warnings in output
  message = FALSE  # Suppress messages in output
)

# Set eval=FALSE for examples requiring Strava API interaction
# Users should run these interactively with a valid stoken
EVAL_EXAMPLES <- FALSE 

## ----setup--------------------------------------------------------------------
library(Athlytics)
library(rStrava) # Required for authentication

## ----load_exposure_example, eval=EVAL_EXAMPLES--------------------------------
# # Calculate using approximate TSS for Rides (Requires FTP)
# exposure_data_tss <- calculate_exposure(
#     stoken = stoken_placeholder,
#     activity_type = "Ride",
#     load_metric = "tss",
#     user_ftp = 280, # Example FTP, replacew ith yours
#     acute_period = 7,
#     chronic_period = 28
# )
# 
# # Plot the result
# plot_exposure(exposure_data = exposure_data_tss, risk_zones = TRUE)
# 
# # Calculate using approximate HRSS for Runs (Requires Max & Resting HR)
# hrss_data <- calculate_exposure(
#     stoken = stoken_placeholder,
#     activity_type = "Run",
#     load_metric = "hrss",
#     user_max_hr = 190,     # Example Max HR
#     user_resting_hr = 50, # Example Resting HR
#     acute_period = 7,
#     chronic_period = 42
# )
# 
# plot_exposure(exposure_data = hrrss_data, risk_zones = TRUE)

## ----acwr_trend_example, eval=EVAL_EXAMPLES-----------------------------------
# # Calculate ACWR using duration for Runs
# acwr_data_run <- calculate_acwr(
#     stoken = stoken_placeholder,
#     activity_type = "Run",
#     load_metric = "duration_mins",
#     acute_period = 7,
#     chronic_period = 28
# )
# 
# # Plot the trend
# plot_acwr(acwr_data = acwr_data_run, highlight_zones = TRUE)

## ----ef_trend_example, eval=EVAL_EXAMPLES-------------------------------------
# # Calculate EF (Pace/HR) for Runs and Rides
# ef_data_pacehr <- calculate_ef(
#     stoken = stoken_placeholder,
#     activity_type = c("Run", "Ride"),
#     ef_metric = "Pace_HR"
# )
# 
# # Plot the trend
# plot_ef(ef_data = ef_data_pacehr, add_trend_line = TRUE)

## ----pbs_example, eval=EVAL_EXAMPLES------------------------------------------
# # Calculate PBs for 1k, 5k, 10k Runs
# # Limit activities checked for speed
# pb_data_run <- calculate_pbs(
#     stoken = stoken_placeholder,
#     distance_meters = c(1000, 5000, 10000),
#     activity_type = "Run",
#     max_activities = 50 # Limit for example
# )
# 
# # Plot the progression, highlighting new PBs
# plot_pbs(pb_data = pb_data_run)

## ----decoupling_example, eval=EVAL_EXAMPLES-----------------------------------
# # Calculate Pace/HR decoupling for Runs
# # Limit activities checked for speed
# decoupling_data_run <- calculate_decoupling(
#     stoken = stoken_placeholder,
#     activity_type = "Run",
#     decouple_metric = "Pace_HR",
#     max_activities = 20 # Limit for example
# )
# 
# # Plot the trend
# plot_decoupling(decoupling_data = decoupling_data_run, add_trend_line = TRUE)

