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

The proliferation of wearable sensors and consumer platforms like Strava has generated vast longitudinal data streams, creating unprecedented opportunities to investigate dynamic human physiological responses to exercise. However, translating this raw data into scientifically rigorous insights for exercise physiology research faces considerable methodological and computational challenges. `Athlytics` is an R [@R-base] package designed as a computational framework to streamline the acquisition, analysis, and visualization of key exercise physiology metrics directly from Strava API data. It enables researchers and practitioners to systematically quantify indicators such as Acute:Chronic Workload Ratio (ACWR) [@Gabbett2016; @Hulin2016], Efficiency Factor (EF) [@allen2019training], cardiovascular decoupling [@Maunder2021], and track personal bests (PBs). By providing standardized function interfaces adhering to `tidyverse` principles [@tidyverse-joss], `Athlytics` significantly lowers the technical barrier for conducting complex longitudinal analyses, thereby enhancing the feasibility, efficiency, and reproducibility of quantitative research utilizing widely available physiological sensor data.

## Statement of Need

Understanding dynamic physiological responses to exercise is central to exercise science and personalized health [@Bourdon2017]. While wearable sensors generate rich data streams [@Hicks2019analyzing], and platforms like Strava serve as a de facto data repository for a vast population of athletes, established open-source workflows in R for systematically quantifying key physiological indicators (e.g., ACWR, EF, decoupling) from its API are lacking. This methodological gap often forces researchers to expend considerable effort on custom programming for data retrieval, metric calculation, and longitudinal visualization, limiting analytical scale, efficiency, and reproducibility [@Sanders2017]. `Athlytics` addresses this critical methodological gap by providing a dedicated R framework. A key feature is its streamlined data access protocol: by requiring only a one-time authentication token per athlete, it grants researchers programmatic and continuous access to longitudinal data, thereby removing a primary logistical and technical hurdle in study execution. The package seamlessly integrates data acquisition (via the `rStrava` package [@R-rStrava]) with the calculation and visualization of these exercise physiology metrics. This empowers broader research applications by enabling researchers to efficiently test hypotheses regarding the dynamic interplay between training stimuli, physiological efficiency, and stress responses using ubiquitous data sources, while carefully considering the necessary approximations for certain composite load metrics.

## Key Functionalities

`Athlytics` offers a modular structure, generally featuring paired `calculate_*` and `plot_*` functions for each analytical task, built upon `dplyr` [@R-dplyr], `ggplot2` [@ggplot2], and other `tidyverse` packages.

*   **Data Acquisition and Preprocessing:** Leverages `rStrava` [@R-rStrava] for Strava API authentication and data retrieval. Handles date/time operations using `lubridate` [@lubridate-jss] and rolling calculations with `zoo` [@zoo-jss].
*   **ACWR Trend Analysis:**
    *   `calculate_acwr`: Computes daily acute (e.g., 7-day) and chronic (e.g., 28-day) rolling loads and their ratio (ACWR) based on user-specified load metrics (duration, distance, or approximated TSS/HRSS).
    *   `plot_acwr`: Visualizes the ACWR time series, optionally highlighting risk zones.
*   **Load Exposure Analysis:**
    *   `calculate_exposure`: Computes daily acute and chronic loads to facilitate a 2D visualization of training state.
    *   `plot_exposure`: Generates a scatter plot of acute vs. chronic load, often with ACWR-derived risk zones.
*   **Personal Bests (PBs) Tracking:**
    *   `calculate_pbs`: Identifies and records best performance times for user-specified distances from Strava activity data (relying on Strava's `best_efforts` field).
    *   `plot_pbs`: Visualizes PB progression over time.
*   **Efficiency Factor (EF) Analysis:**
    *   `calculate_ef`: Computes EF (e.g., average speed/HR or average power/HR) from activity summary data.
    *   `plot_ef`: Visualizes EF trends over time, optionally with a smoothing line.
*   **Decoupling Analysis:**
    *   `calculate_decoupling`: Calculates heart rate/power (or pace/heart rate) decoupling by comparing the efficiency ratio of the first half of an activity to the second half, using detailed activity stream data.
    *   `plot_decoupling`: Visualizes decoupling percentages over time.

## Comparison with Similar Software

Within the R ecosystem for sports science analytics, `Athlytics` carves out a distinct niche through its integrated analytical workflow specifically designed for Strava API data and a curated set of physiological metrics. While the `rStrava` package [@R-rStrava] provides the essential channel for researchers to access Strava data, `Athlytics` builds a critical analytical layer upon it, directly transforming raw activity data into physiological insights suitable for longitudinal research. Compared to more broadly functional R packages like `trackeR` [@trackeR-jss]—which excels in handling diverse local sports tracking files (e.g., .tcx, .gpx) and offering general-purpose sports science analytics—`Athlytics` focuses on the direct utilization of data from the Strava API, a platform that serves as the primary training log for a massive population of amateur and professional athletes. This design provides ready-to-use modules for core physiological indicators of interest to researchers (such as ACWR, EF, and decoupling). This design obviates the need for researchers working with Strava data to undertake cumbersome data integration and workflow concatenation, as might be required when using multiple single-function R packages (e.g., a dedicated package for ACWR calculation like `ACWR` [@R-ACWR], or custom scripts). `Athlytics` offers a coherent framework from data acquisition to the calculation and visualization of multi-dimensional physiological metrics. Adhering to `tidyverse` design principles, `Athlytics` not only lowers the technical barrier for such analyses but, more importantly, enhances the **reproducibility and methodological transparency** of research in this domain. By providing an open-source, standardized computational environment, it directly embodies the core values of modern scientific software.

## Interactive Example

This interactive example demonstrates how to use `Athlytics` to calculate ACWR for multiple athletes and how it interacts with other specialized packages in a scientific analysis workflow.

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
print(summary(mod)$coefficients)      # Show β, SE, t, p-values
```

##  Acknowledgments

The development of `Athlytics` [@R-Athlytics], which is now available from the Comprehensive R Archive Network (CRAN), relied upon the R programming language [@R-base] and benefited from numerous open-source R packages, including `rStrava` [@R-rStrava], `ggplot2` [@ggplot2], `dplyr` [@R-dplyr], `tidyr` [@tidyr], `lubridate` [@lubridate-jss], `zoo` [@zoo-jss], `purrr` [@R-purrr], and `rlang` [@R-rlang]. Access to data was made possible by the Strava API. We also acknowledge the preprint of this work on bioRxiv [@He2025AthlyticsPreprint].
