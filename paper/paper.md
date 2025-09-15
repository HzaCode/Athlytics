---
title: 'Athlytics: A Computational Framework for Longitudinal Analysis of Exercise Physiology Metrics from Wearable Sensor Data'
tags:
  - R
  - Strava
  - exercise physiology
  - physiological monitoring
  - computational physiology
  - bioinformatics
  - wearable sensors
  - longitudinal data analysis
  - training load
  - ACWR
  - efficiency factor
  - decoupling
  - reproducibility
  - sports science
authors:
  - name: Zhiang He
    affiliation: 1
affiliations:
  - name: Independent researcher
    index: 1  
date: 15 May 2025 
bibliography: paper.bib
repository: https://github.com/HzaCode/Athlytics 
version: 1.0.0 
license: MIT 
doi: null 
---

## Summary

The proliferation of wearable sensors and consumer platforms like Strava has generated vast longitudinal data streams, creating unprecedented opportunities to investigate dynamic human physiological responses to exercise. However, translating this raw data into scientifically rigorous insights for exercise physiology research faces considerable methodological and computational challenges. *Athlytics* is an R \[@R-base] package designed as a computational framework to streamline the acquisition, analysis, and visualization of key exercise physiology metrics directly from Strava API data. It enables researchers and practitioners to systematically quantify indicators such as Acute:Chronic Workload Ratio (ACWR) \[@Gabbett2016; @Hulin2016], Efficiency Factor (EF) \[@allen2019training], cardiovascular decoupling \[@Maunder2021], and track personal bests (PBs). By providing standardized function interfaces adhering to `tidyverse` principles \[@tidyverse-joss], *Athlytics* significantly lowers the technical barrier for conducting complex longitudinal analyses, thereby enhancing the feasibility, efficiency, and reproducibility of quantitative research utilizing widely available physiological sensor data.

## Statement of Need

*Athlytics* targets sports scientists, physiologists, and applied researchers studying longitudinal adaptations to endurance training using consumer wearables and Strava as a data hub. Despite the ubiquity of Strava data access via `rStrava` \[@R-rStrava], there is no standardized, open-source workflow in R that transforms raw API outputs into core physiological indicators (e.g., ACWR, EF, decoupling) with tidy, reproducible interfaces. This methodological gap forces many researchers to spend substantial effort on bespoke scripts for data retrieval, metric calculation, and visualization, limiting analytical scale, efficiency, and methodological transparency \[@Sanders2017].

*Athlytics* addresses this gap by providing an integrated framework from data acquisition to the calculation and visualization of multiple physiological metrics, using paired `calculate_*` and `plot_*` functions built on the `tidyverse` \[@tidyverse-joss]. The package is designed to lower the technical barrier for such analyses while maintaining scientific rigor.

## Key Functionalities

*Athlytics* offers a modular structure, generally featuring paired `calculate_*` and `plot_*` functions for each analytical task, built upon `dplyr` \[@R-dplyr], `ggplot2` \[@ggplot2], and other `tidyverse` packages.

* **Data Acquisition and Preprocessing:** Leverages `rStrava` \[@R-rStrava] for Strava API authentication and data retrieval. Handles date/time operations using `lubridate` \[@lubridate-jss] and rolling calculations with `zoo` \[@zoo-jss].
* **ACWR Trend Analysis:**
  * `calculate_acwr`: Computes daily acute (e.g., 7-day) and chronic (e.g., 28-day) rolling loads and their ratio (ACWR) based on user-specified load metrics (duration, distance, or approximated TSS/HRSS).
  * `plot_acwr`: Visualizes the ACWR time series, optionally highlighting risk zones.
* **Load Exposure Analysis:**
  * `calculate_exposure`: Computes daily acute and chronic loads to facilitate a 2D visualization of training state.
  * `plot_exposure`: Generates a scatter plot of acute vs. chronic load, often with ACWR-derived risk zones.
* **Personal Bests (PBs) Tracking:**
  * `calculate_pbs`: Identifies and records best performance times for user-specified distances from Strava activity data (relying on Strava's `best_efforts` field).
  * `plot_pbs`: Visualizes PB progression over time.
* **Efficiency Factor (EF) Analysis:**
  * `calculate_ef`: Computes EF (e.g., average speed/HR or average power/HR) from activity summary data.
  * `plot_ef`: Visualizes EF trends over time, optionally with a smoothing line.
* **Decoupling Analysis:**
  * `calculate_decoupling`: Calculates heart rate/power (or pace/heart rate) decoupling by comparing the efficiency ratio of the first half of an activity to the second half, using detailed activity stream data.
  * `plot_decoupling`: Visualizes decoupling percentages over time.

## Comparison with Similar Software

Within the R ecosystem for sports science analytics, `Athlytics` occupies a distinct niche through its integrated analytical workflow designed for Strava API data and a curated set of physiological metrics. The comparison with related tools is summarized below:

**Athlytics** provides direct Strava API integration with comprehensive physiological metrics (ACWR, Efficiency Factor, aerobic decoupling, personal best tracking) and integrated calculation plus visualization capabilities for reproducible research workflows.

**rStrava** offers Strava API access only for raw activities and segments, with no built-in analysis layer or physiological metrics.

**trackeR** handles local files (GPX/TCX/JSON/DB3) without Strava API integration, providing generic performance summaries and basic data structuring with limited visualization.

**Single-metric packages** (e.g., ACWR packages) focus on individual metrics only, requiring users to combine multiple tools and provide their own data tables.

This design eliminates the need for researchers to piece together multiple single-function packages and supports reproducible longitudinal research directly from a widely-used data source.

## Interactive Example

This interactive example demonstrates how to use *Athlytics* to calculate ACWR for multiple athletes and integrate it into a modeling workflow.

```R
library(Athlytics)             
library(dplyr); library(purrr); library(tidyr); library(lme4)

mod <- read.csv("tokens_access.csv") %>%          # 1. Read athlete tokens

  # 2. Calculate ACWR for each athlete using their token
  mutate(acw = map(access_token, calculate_acwr,
                   activity_type = "Run",
                   load_metric   = "duration_mins",
                   acute_period  = 7,
                   chronic_period= 28)) %>%
  unnest(acw) %>%
  
  # 3. Prepare data for modeling
  group_by(athlete_id) %>%
  arrange(date) %>%
  mutate(lag_ACWR = lag(acwr),       # Predictor: lag-1 ACWR
         perf = run_distance_m) %>%   # Response: run distance
  drop_na(perf, lag_ACWR) %>%

  # 4. Fit a mixed-effects model
  lmer(perf ~ lag_ACWR + (1 | athlete_id), data = .)

# 5. Extract and print model coefficients
print(summary(mod)$coefficients)
````

## Acknowledgments

The development of *Athlytics* \[@R-Athlytics], which is now available from the Comprehensive R Archive Network (CRAN), relied upon the R programming language \[@R-base] and benefited from numerous open-source R packages, including `rStrava` \[@R-rStrava], `ggplot2` \[@ggplot2], `dplyr` \[@R-dplyr], `tidyr` \[@tidyr], `lubridate` \[@lubridate-jss], `zoo` \[@zoo-jss], `purrr` \[@R-purrr], and `rlang` \[@R-rlang]. Access to data was made possible by the Strava API. We also acknowledge the preprint of this work on bioRxiv \[@He2025AthlyticsPreprint].

## References

