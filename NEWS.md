# Athlytics 1.0.5

## Major analysis-quality fixes

* **`calculate_ef()` stream path now uses continuous-block steady-state
  detection.** Previously EF was computed as the median ratio across every
  scattered point whose rolling CV cleared `steady_cv_threshold`, so an
  interval workout with three short "steady islands" could silently pass
  the `min_steady_minutes` gate. The stream path now mirrors
  `calculate_decoupling()`: it uses `rle()` to find contiguous steady
  runs and only accepts activities whose *longest* run lasts at least
  `min_steady_minutes`. New output columns `steady_duration_minutes`,
  `n_steady_blocks`, and `sampling_interval_seconds` make the accepted
  block auditable; new status code `"insufficient_steady_duration"`
  distinguishes this from the old `"non_steady"`.

* **`gap_hr` stream-path fallback is now explicit.** When the stream has
  no grade-adjusted channel the function still falls back to plain
  speed/HR, but the returned row now records this via
  `ef_metric_requested = "gap_hr"`, `ef_metric_used = "speed_hr"`, and
  the new status string `"gap_stream_unavailable_fallback_to_speed"`.
  Downstream consumers that need strict GAP semantics can now filter on
  these columns instead of assuming the `ef_metric` argument equals the
  metric actually computed.

* **Rolling windows are now time-based, not row-based.** `calculate_ef()`,
  `calculate_decoupling()`, and `flag_quality()` previously assumed 1 Hz
  sampling (`window_size <- 300` rows). The effective window silently
  rescaled on 0.5 Hz smart-recording or multi-Hz streams. A new internal
  `estimate_sampling_interval()` helper reads `diff(time)` and the three
  functions now target wall-clock windows; the observed interval is
  exposed as `sampling_interval_seconds` on EF/decoupling output and as
  `attr(result, "sampling_interval_seconds")` on flag_quality output.

* **`flag_quality()` HR / power jump thresholds honour their "per-second"
  documentation.** `max_hr_jump = 10` is now compared against
  `|dHR/dt|` (bpm / s) rather than raw sample-to-sample differences; the
  same change applies to `max_pw_jump`. Streams that are not exactly
  1 Hz are therefore judged consistently across recording devices.

* **Training-load calculation now distinguishes rest days from
  missing-data training days.** `compute_single_load()` returns
  `list(value, status)` instead of collapsing every non-computable case
  to `0`. `calculate_acwr()`, `calculate_acwr_ewma()`, and
  `calculate_exposure()` gain a `missing_load = c("zero", "na")`
  argument: `"zero"` (default) preserves the historical behaviour,
  `"na"` keeps `NA` on days whose load could not be computed so the
  rolling means visibly propagate the gap.

## Transparency additions

* **`calculate_decoupling(stream_df = ..., return_diagnostics = TRUE)`**
  now returns a one-row data frame exposing `decoupling`, `status`,
  `quality_score`, `hr_coverage`, `steady_duration_minutes`, and
  `sampling_interval_seconds`, so callers can distinguish the reason an
  `NA` decoupling was produced. The default still returns a numeric for
  backward compatibility.

* **`calculate_pbs()` adds a `time_basis` column** (always `"moving"` in
  the current implementation) and clarifies in roxygen that PBs are
  moving-time best efforts because `find_best_effort()` operates on the
  strictly-monotonic-distance subset (paused / plateau samples are
  excluded from the candidate window).

* **`flag_quality()` surfaces the activity-level score as an attribute.**
  The per-row `quality_score` column is preserved for backward
  compatibility but the same value is now also attached as
  `attr(result, "activity_quality_score")` and its activity-level
  semantics are documented.

* **`calculate_cohort_reference()` requires `athlete_id` by default.**
  Passing a frame with no `athlete_id` column previously silently
  injected a synthetic `"unknown"` athlete and produced a band that
  looked cohort-derived but was actually within-athlete. The function
  now `stop()`s unless the caller explicitly opts in via
  `allow_unknown_athlete = TRUE`.

## Transparency additions (continued)

* **`missing_load = "zero"` now warns when it silently absorbs data
  gaps.** `calculate_acwr()`, `calculate_acwr_ewma()` and
  `calculate_exposure()` invoke a shared helper that scans the
  per-activity `load_status` column produced by
  `compute_single_load()`; if any activity returned a `missing_*` /
  `hr_out_of_range` status and the caller kept the historical `"zero"`
  default, the function now emits a one-shot `warning()` listing the
  top statuses and recommending `missing_load = "na"`. The default is
  still `"zero"` for backward compatibility.

## Code cleanup

* **`calculate_pbs()` now uses the same stream-parse call pattern as
  `calculate_decoupling()`.** Previously PB processing called
  `parse_activity_file(file.path(export_dir, activity$filename),
  export_dir)` which relied on the zip resolver stripping the
  `export_dir/` prefix internally. The new call passes
  `activity$filename` and `export_dir` directly, matching the
  decoupling path and removing the redundant prefix hop.

* **`calculate_decoupling()` `stream_df` roxygen corrected.** Earlier
  drafts said "If provided, calculates decoupling for this data
  directly, ignoring other parameters". The implementation has not
  ignored those parameters since 1.0.4; the documentation now matches
  the behaviour and explicitly lists which arguments `stream_df`
  honours.

## Documentation

* Expanded the EF and decoupling roxygen `@return` tables and the
  `Status vocabulary` sections with every status string the two
  functions can emit.
* Added a dedicated `PB time semantics` section to `calculate_pbs()`
  explaining elapsed-vs-moving interpretation; clarified that
  `elapsed_time` and `moving_time` are *compatibility columns* that
  carry the same `time_seconds` value and that `time_basis` is the
  authoritative field.
* Documented the `missing_load` knob on every training-load entry
  point.
* README: installation section now prominently flags the
  CRAN / GitHub version mismatch (CRAN `0.1.2` vs GitHub `1.0.5`) and
  recommends r-universe or GitHub for the current offline workflow.
  Citation `version` updated from 1.0.4 to 1.0.5. The Quick Start
  ZIP note now reflects `calculate_pbs()` and `calculate_decoupling()`
  being zip-aware in 1.0.5.

---

# Athlytics 1.0.4

* **Test suite cleanup**: Further streamlined from ~600 to 373 assertions. All tests now pass locally with zero warnings and zero skips.

* **Test idiom improvements**: Replaced `expect_true(is.data.frame())` with `expect_s3_class()`, `expect_equal(length())` with `expect_length()`. Removed redundant `gg`-class checks already covered by vdiffr snapshots.

* **Test file consolidation**: Deleted fragmented files (`test-smoke-and-errors.R`, `test-uncovered-branches.R`, etc.) and merged relevant tests into per-feature files.

* **Dependency cleanup**: Removed `purrr` entirely (only used once via superseded `purrr::transpose()`). R CMD check now passes with 0 errors and 0 warnings.

* **Packaging**: Removed `CITATION.cff` (was flagged by R CMD check as non-standard), cleaned up NAMESPACE and `.Rbuildignore`.

# Athlytics 1.0.3

* **Test suite rewrite**: Reduced from ~1500 to ~200 focused tests with meaningful value assertions; added vdiffr snapshot testing for all plot functions (15 visual regression tests).

* **Bug fixes**:
  - Fixed FIT import when `FITfileR::records()` returns a list
  - Fixed `calculate_ef_from_stream()` column name mismatch (`power` vs `watts`)
  - Fixed `calculate_decoupling()` column name mismatch (`heart_rate` vs `heartrate`)
  - Empty data now returns an error instead of a warning with an empty plot

* **Analysis–plotting separation**: All plot functions now require pre-computed data; passing analysis arguments emits a deprecation warning.

* **Custom S3 classes**: All calculation functions return dedicated classes (`athlytics_acwr`, `athlytics_ef`, `athlytics_decoupling`, `athlytics_pbs`, `athlytics_exposure`).

* **Scientific references**: Added references with DOIs throughout vignettes and roxygen documentation (Gabbett, Hulin, Impellizzeri, Coyle, Allen, Williams, etc.).

* **ACWR caveats**: Added "Important Caveats" section discussing scientific debate on ACWR predictive validity.

* **New features**:
  - Grade Adjusted Pace (`gap_hr`) support for EF calculation
  - `smooth_per_activity_type` option in `plot_ef()`
  - Configurable risk zone thresholds in `plot_acwr()`
  - `plot_exposure()` risk zones no longer require an ACWR column
  - Custom distances in `calculate_pbs()`

* **Documentation improvements**:
  - Strava language setting requirement documented
  - Volume vs load terminology clarified
  - Pace vs speed distinction corrected (`pace_hr` deprecated in favor of `speed_hr`)
  - Native pipe `|>` used in vignettes
  - Roxygen markdown formatting enabled

* **Code cleanup**: Removed `zzz.R`, unused color palette functions, `rStrava`/`mockery` from Suggests, Strava API references; cleaned NAMESPACE imports.

* **CI**: R CMD check now runs on 3 R versions (devel, release, oldrel-1) across 3 OS.

* **Shipped example files**: `inst/extdata/` contains `example.fit`, `.gpx`, `.tcx`.

* **Code style**: Applied `styler::style_pkg()` formatting.

---

# Athlytics 1.0.2
 
## Documentation & Review Fixes
 
* **Runnable vignettes**: Added executable demo chunks using built-in sample datasets so key plots render during `build_vignettes()`.
 
* **Sample data naming**: Renamed built-in datasets from `athlytics_sample_*` to `sample_*` and updated docs/examples accordingly.
 
* **Styling**: Ran `styler::style_pkg()` to improve formatting consistency.

---

# Athlytics 1.0.1

## Code Quality Improvements

* **Reduced Cyclomatic Complexity**: Refactored `calculate_acwr()` and `calculate_exposure()` by extracting shared load calculation logic into internal helper functions (`calculate_daily_load_internal()`, `compute_single_load()`, `validate_load_metric_params()`). This improves code maintainability and testability without changing the public API.

* **Dependency Cleanup**: Removed unused `viridis` package from Imports. The package was declared as a dependency but never actually called (ggplot2's built-in `scale_color_viridis_d()` was used instead).

* **Documentation Fixes**: Fixed Rd line width issues in `plot_with_reference()` examples.

* **API Naming Consistency**: Added verb-first primary APIs and kept previous names as deprecated wrappers for backward compatibility.
  - New: `calculate_cohort_reference()` (replaces `cohort_reference()`)
  - New: `summarize_quality()` (replaces `quality_summary()`)
  - Old names remain available but emit a deprecation warning to guide migration.

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

This significant update enhances package reliability and ease of use by integrating sample datasets. This enables all examples to run offline and ensures core functionalities have undergone more rigorous, reproducible testing.

## Key Changes

*   **Examples & Vignettes**: All Roxygen examples and key vignette examples now primarily use sample datasets for offline execution and clarity. Network-dependent examples are clearly separated in `\donttest{}` blocks.
*   **Test Suite**: Fundamentally refactored the test suite to extensively use sample datasets and `mockery`, improving test robustness and parameter coverage.
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
