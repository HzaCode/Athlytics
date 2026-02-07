---
title: 'Athlytics: Reproducible Scientific Workflows for Cohort Analysis of Endurance Training using Local Strava Data'
tags:
  - R
  - sports science
  - endurance
  - Strava
  - cohort analysis
  - reproducibility
  - ACWR
  - cardiac drift
authors:
  - name: Zhiang He
    orcid: 0009-0009-0171-4578
    affiliation: 1
affiliations:
  - name: Independent Researcher
    index: 1
date: "2025-10-11"
bibliography: paper.bib
version: 1.0.0
license: MIT
---

# Summary

**Athlytics** is an R package that delivers a **complete, API-free workflow for the reproducible analysis of endurance-training data**. Working entirely from **local Strava archives** (ZIP files or CSVs), it provides an integrated pipeline from import and quality control to key physiological metrics, including **acute-to-chronic workload ratios (ACWR)** [@gabbett2016], aerobic efficiency, and cardiac decoupling. Unlike API-dependent approaches, Athlytics is **offline and cohort-first**, integrating quality control, physiological guardrails, and uncertainty reporting into a single, auditable workflow.

# Statement of Need

Analyzing endurance training data in R often requires stitching together API clients, file parsers, and custom scripts. This creates workflows that are fragile and difficult to reproduce, especially for **cohort-scale** research. **Athlytics** addresses this gap by providing a **single, research-oriented pipeline** that works offline with local archives, offers an integrated suite of physiological models, and is built from the ground up for multi-athlete analysis. This design **reduces "glue code"** and makes cohort-scale analyses auditable and easy to reproduce.

# Related Work 

We provide a direct feature comparison to highlight the capabilities essential for reproducible, cohort-scale research.

| Feature (research-relevant) | **Athlytics** | rStrava [@rStrava] | trackeR [@trackeR_jss] | activatr [@activatr] | ACWR [@ACWR] | injurytools [@injurytools] |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| **Offline archives; No OAuth/tokens/quotas** | ✓ | ✕ (API) | ✓ | ✓ | ✓ (tabular) | ✓ (tabular) |
| **API-limited (OAuth, scope, rate-limits)** | ✕ | ✓ | ✕ | ✕ | ✕ | ✕ |
| **End-to-end pipeline (Import→QC→Models→Plot)** | ✓ | ✕ | **Partial** (parsing/viz) | **Partial** (parsing/pace) | ✕ | ✕ |
| **Built-in metrics (ACWR/EF/Decoupling)** | ✓ | ✕ | ✕ | ✕ | **Partial** (ACWR only) | ✕ |
| **Steady-state guards & HR-coverage checks** | ✓ | ✕ | ✕ | ✕ | ✕ | ✕ |
| **Uncertainty (ACWR-EWMA confidence bands)** | ✓ | ✕ | ✕ | ✕ | ✕ | ✕ |
| **Cohort benchmarking (percentile bands)** | ✓ | ✕ | **Partial** (summaries only) | ✕ | ✕ | **Partial** (for injury/exposure) |
| **Diagnostic outputs (status/reason)** | ✓ | ✕ | ✕ | ✕ | ✕ | ✕ |

Athlytics is unique in providing an API-free, end-to-end workflow that integrates a full suite of physiological models, uncertainty quantification, and built-in cohort benchmarking features essential for reproducible research.

# Software Description

-   **Inputs & Data Model:** Reads Strava ZIP archives or `activities.csv`. Activity streams (FIT/TCX/GPX) are loaded **on demand** from the archive, optionally using `FITfileR` for FIT files [@FITfileR]. The core function `load_local_activities()` produces a standardized tibble.
-   **Core Metrics:** `calculate_acwr()`, `calculate_ef()`, and `calculate_decoupling()` compute key summaries with sensible, research-oriented defaults. The implementation of ACWR acknowledges its conceptual issues and is presented as a monitoring tool [@impellizzeri2020acwr; @impellizzeri2021dismiss].
-   **Uncertainty Quantification:** Provides confidence intervals for EWMA-based ACWR using a moving-block bootstrap [@kunsch1989; @politis1994], a key feature for research applications.
-   **Cohort Benchmarking:** `calculate_cohort_reference()` computes percentile bands, which can be layered onto individual plots using `plot_with_reference()`.
-   **Plotting & Diagnostics:** Visualization functions follow a **data-first API**. Functions return **diagnostic fields** (e.g., `status`, `reason`) when inputs are insufficient, making the workflow transparent.

# Example

The following example demonstrates a common cohort analysis workflow: loading data from multiple athletes, calculating ACWR for each, and plotting an individual against cohort-wide reference bands.

```r
library(Athlytics)
library(dplyr)

# 1. Load and combine data for a cohort of athletes, adding identifiers
athlete1 <- load_local_activities("athlete1_export.zip") %>% mutate(athlete_id = "A1")
athlete2 <- load_local_activities("athlete2_export.zip") %>% mutate(athlete_id = "A2")
cohort_data <- bind_rows(athlete1, athlete2)

# 2. Calculate ACWR across the entire cohort using a modern dplyr workflow
cohort_acwr <- cohort_data %>%
  group_by(athlete_id) %>%
  group_modify(~ calculate_acwr(.x, load_metric = "duration_mins")) %>%
  ungroup()

# 3. Generate cohort-wide percentile reference bands
reference_bands <- calculate_cohort_reference(cohort_acwr, metric = "acwr_smooth")

# 4. Plot an individual's data against the cohort reference
individual_acwr <- cohort_acwr %>% filter(athlete_id == "A1")
plot_with_reference(individual = individual_acwr, reference = reference_bands)
```

# Acknowledgements

The author acknowledges the helpful feedback from the pyOpenSci community, and the constructive suggestions provided by Professors Benjamin S. Baumer and Iztok Fister Jr., as well as the developers of the referenced R packages.

# References
