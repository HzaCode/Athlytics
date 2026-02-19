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
version: 1.0.4
license: MIT
---

# Summary

**Athlytics** is an R package that delivers a **complete, API-free workflow for the reproducible analysis of endurance-training data**. Working entirely from **local Strava archives** (ZIP files or CSVs), it provides an integrated pipeline from import and quality control to key physiological metrics, including **acute-to-chronic workload ratios (ACWR)** [@gabbett2016], aerobic efficiency, and cardiac decoupling. Unlike API-dependent approaches, Athlytics is **offline and cohort-first**, integrating quality control, physiological guardrails, and uncertainty reporting into a single, auditable workflow.

# Statement of Need

Analyzing endurance training data in R often requires stitching together API clients, file parsers, and custom scripts. This creates workflows that are fragile and difficult to reproduce, especially for **cohort-scale** research. **Athlytics** addresses this gap by providing a **single, research-oriented pipeline** that works offline with local archives, offers an integrated suite of physiological models, and is built from the ground up for multi-athlete analysis. This design **reduces "glue code"** and makes cohort-scale analyses auditable and easy to reproduce. The primary audience for Athlytics includes **sports scientists, sports epidemiologists, and endurance coaches** who need a reliable, programmatic way to process cohort-scale Strava data reproducibly, bridging the gap between raw XML/FIT files and advanced physiological models.

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

-   **Offline Data Parsing:** Operates directly on local Strava ZIP exports. Using `.fit`, `.tcx`, and `.gpx` parsers via underlying XML and binary decoders (with optional `FITfileR` [@FITfileR] support), activity streams are loaded on demand.
-   **Physiological & Load Metrics:** Supports multiple load tracking algorithms including HRSS (TRIMP-based) and TSS approximations. Calculates core metrics such as **Aerobic Decoupling**, **Efficiency Factor (EF)**, and automatically tracks **Personal Bests (PBs)** using sliding-window spatial algorithms.
-   **Signal Processing & Quality Control:** Automatically filters non-steady-state segments using a rolling coefficient of variation (CV) algorithm to ensure valid physiological comparisons, discarding activities with excessive HR/power drift or GPS anomalies.
-   **Uncertainty Quantification:** Provides confidence intervals for EWMA-based ACWR models using a moving-block bootstrap [@kunsch1989; @politis1994], effectively handling temporal autocorrelation in training loads. This rigorous uncertainty reporting acknowledges the ongoing conceptual debates surrounding ACWR as a predictive tool [@impellizzeri2020acwr; @impellizzeri2021dismiss].
-   **Cohort Benchmarking & Visualization:** Generates population-level percentile reference bands (`calculate_cohort_reference()`) layered via an elegant, Nature-journal-inspired plotting API (e.g., `plot_acwr_enhanced()`).
-   **Diagnostics & Transparency:** Functions return **diagnostic fields** (e.g., `status`, `reason`) when inputs are insufficient, making the workflow transparent and debuggable.

# Example

The following example demonstrates a common cohort analysis workflow: loading data from multiple athletes, calculating ACWR for each, and plotting an individual against cohort-wide reference bands.

```r
library(Athlytics)
library(dplyr)

# 1. Use built-in sample data to simulate a cohort of athletes
data("sample_acwr", package = "Athlytics")
cohort_acwr <- bind_rows(
  sample_acwr %>% mutate(athlete_id = "A1"),
  sample_acwr %>% mutate(
    athlete_id = "A2", 
    acwr_smooth = acwr_smooth * runif(n(), 0.9, 1.1)
  ),
  sample_acwr %>% mutate(
    athlete_id = "A3", 
    acwr_smooth = acwr_smooth * runif(n(), 0.85, 1.15)
  )
)

# 2. Generate cohort-wide percentile reference bands
reference_bands <- calculate_cohort_reference(
  cohort_acwr, 
  metric = "acwr_smooth", 
  min_athletes = 2
)

# 3. Extract individual data
individual_acwr <- cohort_acwr %>% filter(athlete_id == "A1")

# 4. Plot an individual's ACWR against the cohort reference using the enhanced plotting API
plot_acwr_enhanced(
  acwr_data = individual_acwr,
  reference_data = reference_bands,
  show_reference = TRUE,
  highlight_zones = TRUE
)
```

# Acknowledgements

I would like to thank Benjamin S. Baumer and Iztok Fister Jr. for their insightful feedback and constructive suggestions during the development of this package.

# References
