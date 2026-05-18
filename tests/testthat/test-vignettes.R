test_that("intro vignette examples match current API contracts", {
  intro_path <- test_path("../../vignettes/athlytics_introduction.Rmd")
  skip_if_not(file.exists(intro_path))

  intro <- readLines(intro_path, warn = FALSE)
  intro_text <- paste(intro, collapse = "\n")

  expect_no_match(intro_text, "activity_type = NULL", fixed = TRUE)
  expect_no_match(intro_text, 'distance == "5k"', fixed = TRUE)
  expect_no_match(intro_text, "CRAN \\(stable\\)")
  expect_match(intro_text, "export_dir = export_path", fixed = TRUE)
  expect_match(intro_text, "user_max_hr", fixed = TRUE)
  expect_match(intro_text, "user_resting_hr", fixed = TRUE)
  expect_match(intro_text, 'activity_type = "Run"', fixed = TRUE)
  expect_match(intro_text, "Distance in meters", fixed = TRUE)
  expect_match(intro_text, 'distance_label == "5k"', fixed = TRUE)
  expect_match(intro_text, "insufficient_steady_duration", fixed = TRUE)
  expect_match(intro_text, "missing_velocity_data", fixed = TRUE)
  expect_match(intro_text, "missing_power_data", fixed = TRUE)
  expect_no_match(intro_text, "insufficient_duration", fixed = TRUE)
  expect_no_match(intro_text, "no_steady_state", fixed = TRUE)
})

test_that("public export_dir docs match calculate_ef stream support", {
  docs <- c(
    test_path("../../README.md"),
    test_path("../../vignettes/athlytics_introduction.Rmd"),
    test_path("../../R/calculate_ef.R"),
    test_path("../../man/calculate_ef.Rd")
  )
  docs <- docs[file.exists(docs)]
  skip_if_not(length(docs) > 0)

  docs_text <- paste(unlist(lapply(docs, readLines, warn = FALSE)), collapse = "\n")

  expect_no_match(
    docs_text,
    paste(
      "calculate_ef()",
      "still",
      "operates",
      "on activity summaries",
      "and",
      "therefore",
      "does",
      "not need",
      "`export_dir`"
    ),
    fixed = TRUE
  )
  expect_match(docs_text, "calculate_ef()", fixed = TRUE)
  expect_match(docs_text, "export_dir", fixed = TRUE)
  expect_match(docs_text, "stream-based", fixed = TRUE)
  expect_match(docs_text, "activity-summary averages", fixed = TRUE)
})

test_that("public export_dir docs match calculate_pbs zip support", {
  docs <- c(
    test_path("../../R/calculate_pbs.R"),
    test_path("../../man/calculate_pbs.Rd"),
    test_path("../../README.md"),
    test_path("../../vignettes/athlytics_introduction.Rmd")
  )
  docs <- docs[file.exists(docs)]
  skip_if_not(length(docs) > 0)

  docs_text <- paste(unlist(lapply(docs, readLines, warn = FALSE)), collapse = "\n")

  expect_no_match(docs_text, "Base directory of the Strava export", fixed = TRUE)
  expect_match(docs_text, "directory or ZIP", fixed = TRUE)
  expect_match(docs_text, "calculate_pbs()", fixed = TRUE)
  expect_match(docs_text, "export_dir", fixed = TRUE)
})

test_that("public ACWR wording remains descriptive", {
  files <- c(
    test_path("../../vignettes/athlytics_introduction.Rmd"),
    test_path("../../R/calculate_acwr.R"),
    test_path("../../R/plot_acwr.R"),
    test_path("../../R/plot_acwr_enhanced.R"),
    test_path("../../R/plot_exposure.R"),
    test_path("../../man/calculate_acwr.Rd"),
    test_path("../../man/plot_acwr.Rd"),
    test_path("../../man/plot_acwr_enhanced.Rd")
  )
  files <- files[file.exists(files)]
  skip_if_not(length(files) > 0)
  public_text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")

  blocked_phrases <- c(
    paste("###", "Risk", "Zones"),
    paste0("Sweet", " Spot"),
    paste0("sweet", " spot"),
    paste("Moderate", "risk"),
    paste("Warning", "signs"),
    paste("Optimal", "training", "stimulus"),
    paste("may", "warrant", "closer", "load", "management"),
    paste("consider", "load", "management"),
    paste0("de", "training"),
    paste("monitor", "for", "fatigue"),
    paste0("low", " load)"),
    paste0("risky", ")"),
    paste0("less", " risky", ")"),
    paste0("pace", "/power"),
    paste("pace", "or", "power")
  )

  for (phrase in blocked_phrases) {
    expect_no_match(public_text, phrase, fixed = TRUE)
  }

  expect_match(public_text, "Reference Band", fixed = TRUE)
  expect_match(public_text, "Elevated ACWR", fixed = TRUE)
  expect_match(public_text, "High ACWR", fixed = TRUE)
})
