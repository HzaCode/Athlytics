# Athlytics <img src="https://github.com/HzaCode/athlytics/blob/main/image.png?raw=true" align="right" width="180"/>
![CRAN Version](https://img.shields.io/cran/v/Athlytics?style=for-the-badge&color=276DC3&logo=r&logoColor=white)

[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/HzaCode/athlytics/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/HzaCode/athlytics/actions/workflows/R-CMD-check.yml)
[![codecov](https://codecov.io/gh/HzaCode/athlytics/graph/badge.svg)](https://app.codecov.io/gh/HzaCode/athlytics)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Website](https://img.shields.io/badge/website-athlytics-blue)](https://hezhiang.com/athlytics/)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](CONTRIBUTING.md)
[![bioRxiv](https://img.shields.io/badge/bioRxiv-Preprint-F88A0B)](https://www.biorxiv.org/content/10.1101/2025.05.01.651597v3)

---

`Athlytics` is an R framework for the longitudinal analysis of exercise physiology using Strava data. It is designed to remove the primary bottleneck for sports scientists: the programmatic acquisition and consistent processing required to turn raw, high-frequency ecological data into quantitative insights.

---
### ✨ Core Features

*   **📈 Longitudinal Quantification**
    Automated calculation of configurable metrics (ACWR, EF). Enables longitudinal tracking of physiological adaptation.

*   **💔 High-Resolution Decoupling**
    Robust cardiovascular drift analysis from raw stream data. Methodologically superior to metrics from simple averages.

*   **🧹 Tidy & Modeling-Ready Outputs**
    Clean `tibbles` from every function. Eliminates data cleaning. Primed for `ggplot2` visualization and statistical modeling.

*   **🔌 Reproducible Data Acquisition**
    Programmatic Strava API workflow. Guarantees full methodological reproducibility and eliminates manual download errors.
---
## 🚀 Quick Start

**1. Installation**

```r
# CRAN (stable)
install.packages("Athlytics")

# GitHub (development)
# install.packages("remotes")
remotes::install_github("HzaCode/Athlytics")
```

<details>
  <summary><strong>2. Authentication with Strava (click to expand)</strong></summary>

`athlytics` leverages the `rStrava` package for handling the OAuth 2.0 authentication process with the Strava API. This requires a one-time setup of a Strava API application to obtain a **Client ID** and **Client Secret**.

**Steps**

1. **Create a Strava API Application:** Go to your Strava settings → **My API Application** ([https://www.strava.com/settings/api](https://www.strava.com/settings/api)). Set the **Authorization Callback Domain** to `localhost`.
2. **Authenticate in R:** Use `rStrava::strava_oauth()` to generate the token. Using `cache = TRUE` is highly recommended for reproducible workflows.

```r
library(athlytics)
library(rStrava)

# Recommended: keep secrets out of scripts
# Sys.setenv(STRAVA_CLIENT_ID = "YOUR_CLIENT_ID")
# Sys.setenv(STRAVA_CLIENT_SECRET = "YOUR_CLIENT_SECRET")

stoken <- rStrava::strava_oauth(
  app_name      = "MyResearchApp",
  app_client_id = Sys.getenv("STRAVA_CLIENT_ID"),
  app_secret    = Sys.getenv("STRAVA_CLIENT_SECRET"),
  app_scope     = "activity:read_all",
  cache         = TRUE
)
```

</details>

---

## 📊 Compute → Plot

>**Note on Workflow:** Computation is decoupled from plotting. This design reduces API calls and accelerates analysis by allowing a single, computed data object to be used for multiple visualizations.

### 1) ACWR — Load Ramp Monitoring

```r
acwr_data <- calculate_acwr(stoken = stoken, load_metric = "duration_mins")
plot_acwr(acwr_df = acwr_data, highlight_zones = TRUE)
```

<p align="center">
  <img src="https://gaudy-pipe-239.notion.site/image/attachment%3A3b50a271-b755-4eb5-9108-34f97e68b58b%3Aimage.png?table=block&id=1cafc401-a191-80e8-967a-fc60f6946af5&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2" width="700">
</p>

### 2) Exposure — ATL vs. CTL

```r
expo_data <- calculate_exposure(stoken = stoken, activity_type = "Ride", load_metric = "tss", user_ftp = 280)
plot_exposure(exposure_df = expo_data, risk_zones = TRUE)
```

<p align="center">
  <img src="https://gaudy-pipe-239.notion.site/image/attachment%3Ada869625-0481-4b1d-af1a-a1785add2962%3Aimage.png?table=block&id=1c9fc401-a191-8045-aadf-cc29956870ef&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2" width="700">
</p>

### 3) Efficiency Factor — Aerobic Adaptation

```r
ef_data <- calculate_ef(stoken = stoken, activity_type = c("Run", "Ride"), ef_metric = "Pace_HR")
plot_ef(ef_df = ef_data, activity_type = c("Run", "Ride"), add_trend_line = TRUE)
```

<p align="center">
  <img src="https://gaudy-pipe-239.notion.site/image/attachment%3A20a75be7-f255-43ce-a848-ad8e4213858e%3Aimage.png?table=block&id=1cdfc401-a191-806a-986a-d49ee4389a08&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2" width="700">
</p>

### 4) Personal Bests — Peak Performance

```r
pbs_data <- calculate_pbs(stoken = stoken, activity_type = "Run", distance_meters = c(1000, 5000, 10000))
plot_pbs(pbs_df = pbs_data, activity_type = "Run", distance_meters = c(1000, 5000, 10000))
```

<p align="center">
  <img src="https://gaudy-pipe-239.notion.site/image/attachment%3Af5624d35-ad3d-4242-aefc-7cf49881b777%3Aimage.png?table=block&id=1cbfc401-a191-808d-a62b-faa76e4beb5f&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2" width="700">
</p>

### 5) Aerobic Decoupling — Cardiovascular Drift

```r
decoupling_data <- calculate_decoupling(stoken = stoken, activity_type = "Run", max_activities = 20)
plot_decoupling(decoupling_df = decoupling_data, activity_type = "Run")
```

<p align="center">
  <img src="https://gaudy-pipe-239.notion.site/image/attachment%3A13491597-6762-4ea3-843d-13005cf21e8a%3Aimage.png?table=block&id=1cbfc401-a191-80b5-8f1a-efda0eddf069&spaceId=1d079353-f9e2-45ba-8b15-cf2f96e168c5&width=1420&userId=&cache=v2" width="700">
</p>

## 🤝 Contributing

Pull requests are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details and follow the code of conduct in `CODE_OF_CONDUCT.md`.

---

