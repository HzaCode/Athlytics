# Athlytics 1.0.1

## Code Quality Improvements

* **Reduced Cyclomatic Complexity**: Refactored `calculate_acwr()` and `calculate_exposure()` by extracting shared load calculation logic into internal helper functions (`calculate_daily_load_internal()`, `compute_single_load()`, `validate_load_metric_params()`). This improves code maintainability and testability without changing the public API.

* **Dependency Cleanup**: Removed unused `viridis` package from Imports. The package was declared as a dependency but never actually called (ggplot2's built-in `scale_color_viridis_d()` was used instead).

* **Documentation Fixes**: Fixed Rd line width issues in `plot_with_reference()` examples.

* **Build Configuration**: Updated `.Rbuildignore` to properly exclude development files.

---

# Athlytics 1.0.0

This major release transitions from Strava API to **local data export processing**, prioritizing user privacy and data ownership while eliminating API rate limits and authentication requirements.

## Breaking Changes & New Features

* **Privacy-First Architecture**: Complete shift from Strava API to local ZIP file processing
  - New `load_local_activities()` function supports direct ZIP file loading (no manual extraction needed)
  - Removed API dependencies and authentication requirements
  - All data processing happens locally - no cloud services involved
  
* **Enhanced Documentation**: Comprehensive documentation improvements across all core functions
  - Added detailed parameter explanations with recommended values
  - Included interpretation guidelines and typical value ranges
  - Added algorithm descriptions and best practices
  - Expanded with academic references and cross-function links
  - Enhanced `calculate_acwr()`, `calculate_ef()`, and `calculate_decoupling()` documentation

* **Multi-Athlete Cohort Analysis**: Improved support for research and team analytics
  - Better documentation for `cohort_reference()` and multi-athlete workflows
  - Examples updated to show intervention/control group comparisons
  - Proper use of `group_modify()` for batch processing

* **README & Package Updates**
  - Updated all code examples to reflect local data processing workflow
  - Corrected function parameter names throughout (e.g., `activities_df` → `activities_data`)
  - Added installation guidance emphasizing GitHub v1.0.0 as latest version
  - Removed outdated API-related content

## Academic Paper

* Added comprehensive JOSS-style paper (`paper/paper.md` and `paper/paper.bib`)
  - Detailed comparison with related R packages (rStrava, trackeR, activatr, FITfileR, ACWR, injurytools)
  - Updated methodology to reflect local data processing approach
  - Enhanced reproducible examples using local exports

## Technical Improvements

* Fixed encoding issues in BibTeX references
* Updated `.gitignore` to track README.md and paper/ directory
* Synchronized all documentation with actual function implementations
* Improved pkgdown website generation

## Migration Guide

For users upgrading from 0.1.x:

1. Download your Strava bulk data export (Settings > My Account > Download Request)
2. Replace `fetch_strava_activities()` calls with `load_local_activities("export.zip")`
3. Update function calls: `activities_df` → `activities_data`, `plot_*()` now accepts data directly
4. Remove Strava API authentication code

---

# Athlytics 0.1.2

*   **CRAN Resubmission**: Carefully addressed feedback from CRAN by making detailed updates and modifications for package resubmission. This primarily involved refining examples (e.g., consistently using `\dontrun{}` as advised) and ensuring metadata files meet all CRAN standards.

---

*   **Testing**: Focused on increasing test coverage towards the goal of 85% across the package. Integrated Codecov for ongoing coverage monitoring.
*   **Bug Fixes**

---

# Athlytics 0.1.1

## Core Improvement: Enhanced Reliability & Testing with Simulated Data

This significant update enhances package reliability and ease of use by integrating `athlytics_sample_data`. This enables all examples to run offline and ensures core functionalities have undergone more rigorous, reproducible testing.

## Key Changes

*   **Examples & Vignettes**: All Roxygen examples and key vignette examples now primarily use `athlytics_sample_data` for offline execution and clarity. Network-dependent examples are clearly separated in `\donttest{}` blocks.
*   **Test Suite**: Fundamentally refactored the test suite to extensively use `athlytics_sample_data` and `mockery`, improving test robustness and parameter coverage.
*   **Strengthened Package Quality & Compliance**: Undertook thorough package validation, leading to key enhancements for overall robustness and adherence to R packaging standards. This involved: ensuring all **function examples** are correct and reliably executable (notably addressing `strava_oauth(...)` scenarios for offline/testing contexts); providing accurate and **refined documentation for data objects** in `R/data.R`; fixing **Roxygen import directives** for precise namespace definition; improving **help file readability** through Rd line width adjustments; and optimizing package data loading by adding `LazyData: true` to `DESCRIPTION`.
*   **Documentation**: Minor improvements to documentation clarity and consistency (e.g., date formatting in plots, explicit naming of data frame arguments in examples).


---
# Athlytics 0.1.0

## Major Changes

*   **Decoupling Calculation**: Switched from `rStrava::get_activity_streams` to direct Strava API calls using `httr` and `jsonlite` for fetching activity streams in `calculate_decoupling`. This aims to resolve previous errors but might impact performance and rate limiting.

## Bug Fixes & Improvements

*   Fixed `calculate_acwr` error (`condition has length > 1`) by forcing evaluation before the dplyr pipe.
*   Corrected `plot_pbs` usage in examples and test scripts to include the required `distance_meters` argument.
*   Added missing dependencies (`httr`, `jsonlite`) to `DESCRIPTION` file.
*   Improved error handling and messages in several functions.
*   Simplified Roxygen documentation for core functions.
*   Updated README examples and descriptions for clarity and consistency with code.

## Initial Release

* Initial release.
* Added core functions for calculating and plotting:
    * Load Exposure (Acute vs. Chronic Load)
    * ACWR Trend
    * Efficiency Factor Trend
    * Personal Bests (PBs)
    * Decoupling Trend
* Added Strava authentication helper based on `rStrava`.
