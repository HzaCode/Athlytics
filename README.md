Athlytics <img src="https://github.com/HzaCode/Athlytics/blob/main/image.png?raw=true" align="right" width="200"/>
=======================================================


[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version/Athlytics)](https://CRAN.R-project.org/package=Athlytics)
[![R-CMD-check](https://github.com/HzaCode/Athlytics/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/HzaCode/Athlytics/actions/workflows/R-CMD-check.yml)
[![codecov](https://codecov.io/gh/HzaCode/Athlytics/graph/badge.svg)](https://app.codecov.io/gh/HzaCode/Athlytics)
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) 
[![Website](https://img.shields.io/badge/website-Athlytics-blue)](https://hezhiang.com/Athlytics/)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](CONTRIBUTING.md)
[![bioRxiv](https://img.shields.io/badge/bioRxiv-Preprint-F88A0B)](https://www.biorxiv.org/content/10.1101/2025.05.01.651597v2)


**Athlytics** is an R package that provides a computational framework for the longitudinal analysis of exercise physiology metrics. It is specifically designed to streamline the acquisition, processing, and visualization of data directly from the Strava API. The package addresses a critical need in sports science research by providing a standardized, reproducible workflow to quantify key indicators of training load, performance, and physiological adaptation, such as the Acute:Chronic Workload Ratio (ACWR), Efficiency Factor (EF), and cardiovascular decoupling. By interfacing directly with Strava—a nearly ubiquitous platform for athletes—`Athlytics` enables researchers to efficiently analyze rich, real-world data to investigate the dynamic interplay between training stimuli and human physiology.

### Installation

The stable version of `Athlytics` is available on CRAN and can be installed with:
```r
install.packages("Athlytics")
```

Alternatively, the latest development version can be installed from GitHub:
```r
# install.packages('remotes')
remotes::install_github('HzaCode/Athlytics')
```

### Authentication Protocol

`Athlytics` leverages the `rStrava` package for handling the OAuth2.0 authentication process with the Strava API. This requires a one-time setup of a Strava API application to obtain a Client ID and Secret. The resulting token object is then passed to `Athlytics` functions to authorize data access.

1.  **Create a Strava API Application:** Navigate to your Strava settings under "My API Application" ([https://www.strava.com/settings/api](https://www.strava.com/settings/api)). Set the "Authorization Callback Domain" to `localhost` and note your **Client ID** and **Client Secret**.
2.  **Authenticate in R:** Use `rStrava::strava_oauth()` to generate the authentication token. It is highly recommended to use `cache = TRUE` to store the token securely, which ensures persistent authentication across sessions—a key feature for reproducible research workflows.

```r
library(Athlytics)
library(rStrava)

# --- Authentication Step ---
# Recommended: Store credentials as environment variables for security
# Sys.setenv(STRAVA_CLIENT_ID = "YOUR_CLIENT_ID")
# Sys.setenv(STRAVA_CLIENT_SECRET = "YOUR_SECRET")

stoken <- strava_oauth(
  app_name = "ResearchAnalysisApp", 
  app_client_id = Sys.getenv("STRAVA_CLIENT_ID"),
  app_secret = Sys.getenv("STRAVA_CLIENT_SECRET"),
  app_scope = "activity:read_all",
  cache = TRUE # Caches the token for future use
)
```

### User-Defined Physiological Parameters

For certain advanced metrics, user-specific physiological data is required. These parameters can be supplied directly to the relevant `Athlytics` functions:

*   `user_ftp` (Numeric): Functional Threshold Power (Watts), required for calculating Training Stress Score (TSS).
*   `user_max_hr` (Numeric): Maximum Heart Rate, required for calculating Heart Rate Stress Score (HRSS).
*   `user_resting_hr` (Numeric): Resting Heart Rate, also required for HRSS calculation.

### Core Scientific Workflows & Visualizations

The following examples illustrate the core analytical capabilities of the package. Each function is designed to quantify a specific physiological or performance concept.

**Note on Computational and API Constraints:** Functions that require detailed activity-level data, such as `calculate_pbs` and `calculate_decoupling`, can be computationally intensive and may be subject to Strava API rate limits. For large-scale analyses, consider using the `max_activities` parameter or analyzing data in smaller date-range batches.

#### 1. Quantifying Training Stress Balance (Load Exposure)

This analysis operationalizes the fitness-fatigue model by plotting acute training load (ATL) against chronic training load (CTL). It provides a quantitative method to visualize an athlete's state of preparedness, identifying periods of excessive training load, optimal adaptation, or tapering. The calculation of TSS or HRSS is an approximation based on summary activity data.

**Data Calculation:**
```r
exposure_data <- calculate_exposure(
    stoken = stoken,
    activity_type = "Ride",
    load_metric = "tss",
    user_ftp = 280,         # Required for TSS
    acute_period = 7,
    chronic_period = 28
)
```

**Visualization:**
```r
plot_exposure(
    stoken = stoken,
    activity_type = "Ride",
    load_metric = "tss",
    user_ftp = 280,
    acute_period = 7,
    chronic_period = 28
)
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3Ada869625-0481-4b1d-af1a-a1785add2962%3Aimage.png?table=block&id=1c9fc401-a191-8045-aadf-cc29956870ef&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

#### 2. Monitoring Training Load Ramping (ACWR Trend)

This module implements the Acute:Chronic Workload Ratio (ACWR) methodology, a widely used metric in sports science for monitoring injury risk associated with rapid changes in training load. The function calculates and visualizes the rolling ACWR over time, allowing researchers to identify periods where load progression may have been too aggressive.

**Data Calculation:**
```r
acwr_data <- calculate_acwr(
    stoken = stoken,
    load_metric = "duration_mins",
    acute_period = 7, 
    chronic_period = 28
)
```

**Visualization:**
```r
plot_acwr(
    stoken = stoken,
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = 28,
    highlight_zones = TRUE
)
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3A3b50a271-b755-4eb5-9108-34f97e68b58b%3Aimage.png?table=block&id=1cafc401-a191-80e8-967a-fc60f6946af5&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

#### 3. Assessing Longitudinal Aerobic Adaptations (Efficiency Factor)

The Efficiency Factor (EF) quantifies aerobic efficiency by relating mechanical output (power or pace) to its physiological cost (heart rate). A positive trend in EF over time is indicative of improved aerobic fitness. This analysis provides a method to track these long-term adaptations.

**Data Calculation:**
```r
ef_data <- calculate_ef(
    stoken = stoken,
    activity_type = c("Run", "Ride"),
    ef_metric = "Pace_HR"
)
```

**Visualization:**
```r
plot_ef(
    stoken = stoken,
    activity_type = c("Run", "Ride"),
    ef_metric = "Pace_HR",
    add_trend_line = TRUE
)
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3A20a75be7-f255-43ce-a848-ad8e4213858e%3Aimage.png?table=block&id=1cdfc401-a191-806a-986a-d49ee4389a08&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

#### 4. Tracking Performance Outcomes (Personal Bests)

This module provides functionality to systematically extract and track personal bests (PBs) over user-specified distances. PBs serve as a direct measure of performance changes, allowing for the empirical validation of training effectiveness and the identification of performance plateaus.

**Data Calculation:**
```r
pb_data <- calculate_pbs(
    stoken = stoken,
    distance_meters = c(1000, 5000, 10000),
    activity_type = "Run"
)
```

**Visualization:**
```r
plot_pbs(
    stoken = stoken,
    distance_meters = c(1000, 5000, 10000),
    activity_type = "Run"
)
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3Af5624d35-ad3d-4242-aefc-7cf49881b777%3Aimage.png?table=block&id=1cbfc401-a191-808d-a62b-faa76e4beb5f&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

#### 5. Quantifying Cardiovascular Drift (Decoupling)

This function calculates cardiovascular decoupling, the phenomenon where physiological cost (e.g., heart rate) drifts upwards relative to a constant mechanical output (e.g., power or pace) during prolonged exercise. A higher decoupling rate can indicate accumulating fatigue or insufficient aerobic fitness.

**Data Calculation:**
```r
decoupling_data <- calculate_decoupling(
    stoken = stoken,
    activity_type = "Run",
    decouple_metric = "Pace_HR",
    max_activities = 20 # Recommended to limit due to API constraints
)
```

**Visualization:**
```r
plot_decoupling(
    stoken = stoken,
    activity_type = "Run",
    decouple_metric = "Pace_HR",
    max_activities = 20
)
```
![](https://gaudy-pipe-239.notion.site/image/attachment%3A13491597-6762-4ea3-843d-13005cf21e8a%3Aimage.png?table=block&id=1cbfc401-a191-80b5-8f1a-efda0eddf069&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2)

### Development Status & Contribution

Athlytics is under active development. We adhere to a code of conduct and encourage community contributions. Please see our [Contributing Guidelines](CONTRIBUTING.md) for more information. Bug reports and feature requests are welcome via GitHub Issues.
