# Strava API v3 Base URL: https://www.strava.com/api/v3

library(Athlytics) # Load package using library()
library(rStrava)   # Load rStrava for authentication and API functions
library(httr)      # Load httr for potential OOB auth / lower-level control

rm(list = ls(pattern = "^calculate_")) # Remove potential conflicts
app_name    = "api_testing"    # Match the name set in Strava settings
app_client_id   = "118977"     # User-provided Client ID
app_secret = "c13f03c13dc139e11b8a67c186ee055d98b04ce0" # 请妥善保管你的 Secret!

# --- Strava Authentication ---
# Strava API Application settings page (to get Client ID/Secret):
# https://www.strava.com/settings/api
# IMPORTANT: Replace placeholders with your actual Strava API credentials if different!

message("\n--- Attempting Strava Authentication (will use cache if available) ---")

# 尝试使用缓存加载或进行认证
stoken <- tryCatch({
  rStrava::strava_oauth(
    app_name = app_name,
    app_client_id = app_client_id,
    app_secret = app_secret,
    app_scope = "activity:read_all",
    cache = TRUE # <-- 设置为 TRUE 以启用缓存加载/保存
  )
}, error = function(e) {
  message("ERROR during Strava authentication: ", e$message)
  message("Please ensure your Strava API credentials (Client ID, Secret) are correct and you complete the browser authentication if prompted.")
  NULL # 返回 NULL 表示失败
})

# Check if token was successfully obtained
if (is.null(stoken) || !inherits(stoken, "Token2.0")) {
  stop("Failed to obtain a valid Strava token. Please check credentials and authentication process.")
} else {
  message("Successfully obtained Strava token.")
}

# --- Define a global limit for max activities in tests ---
# Adjust this value to control how many recent activities are fetched for testing
# Increase this to improve chances of getting 10 successful decoupling results
test_max_activities <- 50 # Increase attempt count, e.g., to 50
message(sprintf("\n--- Using a maximum of %d activities for relevant tests --- \n", test_max_activities))

# ============================================================
#  Test: calculate_exposure (using duration_mins)
# ============================================================
# --- Calculation ---
exposure_data_duration <- tryCatch({
  calculate_exposure(stoken,
    activity_type = c("Run", "Ride"), # Test with multiple types
    load_metric = "duration_mins",
    acute_period = 7,
    chronic_period = 42,
    end_date = Sys.Date() # Use today as end date
  )
}, error = function(e) {
  message("ERROR during calculate_exposure (duration): ", e$message)
  NULL
})

# --- Results & Plotting ---
if (exists("exposure_data_duration") && !is.null(exposure_data_duration) && nrow(exposure_data_duration) > 0) {
  print("Exposure data (duration):")
  print(head(exposure_data_duration)) # Print head for brevity
  message("Plotting Exposure (Duration)...")
  tryCatch({
    plot_exposure(exposure_df = exposure_data_duration) # Pass the calculated df
  }, error = function(e) {
    message("Error plotting exposure (duration): ", e$message)
  })
} else {
  message("Skipping Exposure (Duration) plot: Calculation failed or data not available.")
}

# ============================================================
#  Test: calculate_acwr (using duration_mins for Runs)
# ============================================================
# --- Calculation ---
acwr_data_runs <- tryCatch({
  calculate_acwr(
    stoken = stoken,
    activity_type = "Run",         # Example: Focus on Runs
    load_metric = "duration_mins", # Example: Use duration
    acute_period = 7,
    chronic_period = 28
    # , end_date = Sys.Date()      # Optional
    # , smoothing_period = 7       # Optional
  )
}, error = function(e) {
  message("ERROR during calculate_acwr (duration, runs): ", e$message)
  NULL
})

# --- Results & Plotting ---
if (exists("acwr_data_runs") && !is.null(acwr_data_runs) && nrow(acwr_data_runs) > 0) {
  print("ACWR data (duration, runs):")
  print(tail(acwr_data_runs)) # Print tail to see recent trend
  message("Plotting ACWR (Duration, Runs)...")
   tryCatch({
    # plot_acwr needs to know the params used OR receive the df
    # Assuming plot_acwr can take acwr_df directly if it follows expected structure
    # If plot_acwr MUST recalculate, pass parameters instead:
    plot_acwr(
        stoken = stoken, # plot_acwr might recalculate if df not passed explicitly
        activity_type = "Run",
        load_metric = "duration_mins",
        acute_period = 7,
        chronic_period = 28
        # acwr_df = acwr_data_runs # Ideally, pass the df directly if plot_acwr supports it
    )
   }, error = function(e) {
    message("Error plotting ACWR (Duration, Runs): ", e$message)
   })
} else {
  message("Skipping ACWR (Duration, Runs) plot: Calculation failed or data not available.")
}

# ============================================================
#  Test: calculate_ef (Pace_HR for Runs)
# ============================================================
# --- Calculation ---
ef_data_runs <- tryCatch({
  calculate_ef(
    stoken = stoken,
    activity_type = "Run",    # Example: Focus on Runs
    ef_metric = "Pace_HR"     # Example: Pace / Heart Rate EF
    # , min_duration_mins = 20 # Optional
    # , start_date = ...       # Optional
    # , end_date = ...         # Optional
  )
}, error = function(e) {
  message("ERROR during calculate_ef (Pace_HR, runs): ", e$message)
  NULL
})

# --- Results & Plotting ---
if (exists("ef_data_runs") && !is.null(ef_data_runs) && nrow(ef_data_runs) > 0) {
  print("Efficiency Factor data (Pace/HR, runs):")
  print(tail(ef_data_runs)) # Print tail to see recent trend
  message("Plotting Efficiency Factor (Pace/HR, Runs)...")
   tryCatch({
    # Assuming plot_ef can take ef_df directly if it follows expected structure
    # If plot_ef MUST recalculate, pass parameters instead:
     plot_ef(
         stoken = stoken, # plot_ef might recalculate if df not passed explicitly
         activity_type = "Run",
         ef_metric = "Pace_HR"
         # ef_df = ef_data_runs # Ideally, pass the df directly if plot_ef supports it
     )
   }, error = function(e) {
    message("Error plotting Efficiency Factor (Pace/HR, Runs): ", e$message)
   })
} else {
  message("Skipping Efficiency Factor (Pace/HR, Runs) plot: Calculation failed or data not available.")
}

# ============================================================
#  Test: calculate_pbs (for Runs)
# ============================================================
# --- Preparation ---
# Define PB distances before calling calculate_pbs
pb_distances_run <- c(1000, 1609, 5000, 10000, 21097, 42195) # Example: 1k, 1 mile, 5k, 10k, HM, Marathon

# --- Calculation ---
# Add tryCatch block
pb_data_run <- tryCatch({
    calculate_pbs(stoken,
    activity_type = "Run",
    distance_meters = pb_distances_run, # Corrected parameter name
    max_activities = test_max_activities  # Use defined limit
  )
}, error = function(e) {
    message("ERROR during calculate_pbs: ", e$message)
    NULL # Return NULL on error
})

# --- Results & Plotting ---
# Add plotting call immediately after calculation
if (exists("pb_data_run") && !is.null(pb_data_run) && nrow(pb_data_run) > 0) {
  print("PB data (Run):")
  print(pb_data_run)
  message("Plotting PBs (Run)...")
   tryCatch({
     plot_pbs(pb_data_run)
   }, error = function(e) {
     message("Error plotting PBs: ", e$message)
   })
} else {
  print("Skipping PB data output and plot: Calculation failed or yielded no results.")
}

# ============================================================
#  Test: calculate_decoupling (Pace_HR for Runs)
# ============================================================
# --- Calculation ---
# Add tryCatch block for robustness
decoupling_data_runs <- tryCatch({
    calculate_decoupling(stoken,
        activity_type = "Run",       # Focus on Runs
        decouple_metric = "Pace_HR",
        max_activities = test_max_activities # Use defined (potentially increased) limit
        # min_duration_mins can be left as default or specified
    )
}, error = function(e) {
    message("ERROR during calculate_decoupling: ", e$message)
    NULL # Return NULL on error
})

# --- Results & Plotting ---
# Check the results and limit to 10 if more were found
if (exists("decoupling_data_runs") && !is.null(decoupling_data_runs) && nrow(decoupling_data_runs) > 0) {
  message(sprintf("calculate_decoupling returned %d results.", nrow(decoupling_data_runs)))
  if (nrow(decoupling_data_runs) > 10) {
    message("Limiting decoupling data to the first 10 results for testing.")
    decoupling_data_runs_limited <- head(decoupling_data_runs, 10)
  } else {
    decoupling_data_runs_limited <- decoupling_data_runs
  }
  print("Decoupling data (up to 10 results):")
  print(decoupling_data_runs_limited)
  # Potentially add print(head(decoupling_data_runs)) and uncomment plot call using decoupling_data_runs_limited
  # --- UNCOMMENT PLOTTING ---
  message("Plotting Decoupling (Recent Runs)...")
  tryCatch({ plot_decoupling(decoupling_df = decoupling_data_runs_limited) }, error = function(e) { message("Error plotting decoupling: ", e$message) })
} else {
  print("Skipping Decoupling data output and plot: Calculation failed or yielded no results.")
}

# ============================================================
#                      End of Tests
# ============================================================
message("\n--- End of Test Script ---")

# ============================================================
#                      Save Sample Data
# ============================================================
message("\n--- Attempting to save sample data ---")

# Define the objects to save and their names in the .RData file
# These should be the final, processed data frames from the tests above

# 1. Exposure Data
if (exists("exposure_data_duration") && !is.null(exposure_data_duration) && nrow(exposure_data_duration) > 0) {
  athlytics_sample_exposure <- exposure_data_duration
  message("Exposure data will be saved.")
} else {
  athlytics_sample_exposure <- NULL
  message("Exposure data (exposure_data_duration) not available or empty, will not be saved.")
}

# 2. ACWR Data
if (exists("acwr_data_runs") && !is.null(acwr_data_runs) && nrow(acwr_data_runs) > 0) {
  athlytics_sample_acwr <- acwr_data_runs
  message("ACWR data will be saved.")
} else {
  athlytics_sample_acwr <- NULL
  message("ACWR data (acwr_data_runs) not available or empty, will not be saved.")
}

# 3. Efficiency Factor Data
if (exists("ef_data_runs") && !is.null(ef_data_runs) && nrow(ef_data_runs) > 0) {
  athlytics_sample_ef <- ef_data_runs
  message("EF data will be saved.")
} else {
  athlytics_sample_ef <- NULL
  message("EF data (ef_data_runs) not available or empty, will not be saved.")
}

# 4. Personal Bests Data
if (exists("pb_data_run") && !is.null(pb_data_run) && nrow(pb_data_run) > 0) {
  athlytics_sample_pbs <- pb_data_run
  message("PBs data will be saved.")
} else {
  athlytics_sample_pbs <- NULL
  message("PBs data (pb_data_run) not available or empty, will not be saved.")
}

# 5. Decoupling Data
if (exists("decoupling_data_runs_limited") && !is.null(decoupling_data_runs_limited) && nrow(decoupling_data_runs_limited) > 0) {
  athlytics_sample_decoupling <- decoupling_data_runs_limited
  message("Decoupling data will be saved.")
} else {
  athlytics_sample_decoupling <- NULL
  message("Decoupling data (decoupling_data_runs_limited) not available or empty, will not be saved.")
}

# It's also highly recommended to have a sample activity stream.
# The current script doesn't explicitly save a single activity's stream.
# For package examples, you might want to use `mock_activity_streams`
# from `tests/testthat/helpetxt-mockdata.R` or fetch and save a specific one.
# For now, we'll save `mock_activity_streams` if it's loaded by sourcing `helpetxt-mockdata.R`
# (Assuming `helpetxt-mockdata.R` is sourced if this script is run in an environment where it's available)
# Or, more robustly, load it from the testthat helper if this script is being adapted for data-raw.

# For simplicity, let's assume mock_activity_streams is available or you'll add it:
# If you have tests/testthat/helpetxt-mockdata.R, you can source it:
# source("../testthat/helpetxt-mockdata.R") # Adjust path if needed
if (exists("mock_activity_streams") && !is.null(mock_activity_streams) && nrow(mock_activity_streams) > 0) {
    athlytics_sample_activity_stream <- mock_activity_streams
    message("Sample activity stream (mock_activity_streams) will be saved.")
} else {
    athlytics_sample_activity_stream <- NULL
    message("mock_activity_streams not found or empty. Consider adding it for complete sample data.")
}


# List of objects to save
objects_to_save <- character(0)
if (!is.null(athlytics_sample_exposure)) objects_to_save <- c(objects_to_save, "athlytics_sample_exposure")
if (!is.null(athlytics_sample_acwr)) objects_to_save <- c(objects_to_save, "athlytics_sample_acwr")
if (!is.null(athlytics_sample_ef)) objects_to_save <- c(objects_to_save, "athlytics_sample_ef")
if (!is.null(athlytics_sample_pbs)) objects_to_save <- c(objects_to_save, "athlytics_sample_pbs")
if (!is.null(athlytics_sample_decoupling)) objects_to_save <- c(objects_to_save, "athlytics_sample_decoupling")
if (!is.null(athlytics_sample_activity_stream)) objects_to_save <- c(objects_to_save, "athlytics_sample_activity_stream")


if (length(objects_to_save) > 0) {
  # Create data directory if it doesn't exist (relative to project root)
  if (!dir.exists("data")) {
    message("Creating 'data/' directory in the project root.")
    dir.create("data")
  }

  save_file_path <- "data/Athlytics_sample_data.RData"

  # --- 修改保存方式 ---
  # 直接将要保存的对象名（作为字符串）传递给 save() 的 list 参数
  # 或者直接将对象名作为参数传递
  tryCatch({
    # 构建要传递给 save() 的参数列表，其中每个元素都是实际的对象
    # save(athlytics_sample_exposure, athlytics_sample_acwr, ..., file = save_file_path)
    # 为了动态构建这个调用，我们可以这样做：

    # 确保只保存实际存在的对象
    existing_objects_to_save <- Filter(function(x) !is.null(get0(x)), objects_to_save)

    if (length(existing_objects_to_save) > 0) {
        command_to_run <- paste0(
            "save(",
            paste(existing_objects_to_save, collapse = ", "),
            ", file = '", save_file_path, "')"
        )
        message("Executing: ", command_to_run)
        eval(parse(text = command_to_run))

        message(sprintf("Successfully saved sample data to: %s", save_file_path))
        message("Objects saved: ", paste(existing_objects_to_save, collapse = ", "))
        message("\nRECOMMENDATION: ")
        message("1. Move this script (manual_test_script.R) to a 'data-raw' directory in your project root.")
        message("2. Run it from the project root to generate 'data/Athlytics_sample_data.RData'.")
        message("3. Add 'data/Athlytics_sample_data.RData' to your .Rbuildignore if you haven't already (though data/ is usually included).")
        message("4. Your package can then use `data(Athlytics_sample_data)` to load these datasets for examples.")
    } else {
        message("No non-null objects were available to save.")
    }
  }, error = function(e) {
    message("ERROR saving sample data: ", e$message)
  })
} else {
  message("No data objects were designated for saving.") # 修改了此消息
}

message("\n--- Sample Data Saving Process Complete ---")

# ============================================================
#                      使用模拟数据测试
# ============================================================
message("\n--- 开始使用模拟数据测试 ---")

# 加载模拟数据
if (file.exists("data/Athlytics_sample_data.RData")) {
  load("data/Athlytics_sample_data.RData")
  message("成功加载模拟数据")
} else {
  stop("未找到模拟数据文件：data/Athlytics_sample_data.RData")
}

# 测试1：使用模拟数据测试ACWR计算和绘图
if (exists("athlytics_sample_acwr") && !is.null(athlytics_sample_acwr)) {
  message("\n测试1：使用模拟数据测试ACWR")
  print("ACWR模拟数据预览：")
  print(head(athlytics_sample_acwr))

  message("绘制ACWR图表...")
  tryCatch({
    p <- plot_acwr(acwr_df = athlytics_sample_acwr)
    print(p)  # 显示图表
    message("ACWR图表绘制成功")
  }, error = function(e) {
    message("ACWR图表绘制失败：", e$message)
  })
}

# 测试2：使用模拟数据测试Exposure计算和绘图
if (exists("athlytics_sample_exposure") && !is.null(athlytics_sample_exposure)) {
  message("\n测试2：使用模拟数据测试Exposure")
  print("Exposure模拟数据预览：")
  print(head(athlytics_sample_exposure))

  message("绘制Exposure图表...")
  tryCatch({
    p <- plot_exposure(exposure_df = athlytics_sample_exposure)
    print(p)  # 显示图表
    message("Exposure图表绘制成功")
  }, error = function(e) {
    message("Exposure图表绘制失败：", e$message)
  })
}

# 测试3：使用模拟数据测试Efficiency Factor计算和绘图
if (exists("athlytics_sample_ef") && !is.null(athlytics_sample_ef)) {
  message("\n测试3：使用模拟数据测试Efficiency Factor")
  print("EF模拟数据预览：")
  print(head(athlytics_sample_ef))

  message("绘制EF图表...")
  tryCatch({
    p <- plot_ef(ef_df = athlytics_sample_ef)
    print(p)  # 显示图表
    message("EF图表绘制成功")
  }, error = function(e) {
    message("EF图表绘制失败：", e$message)
  })
}

# 测试4：使用模拟数据测试PBs计算和绘图
if (exists("athlytics_sample_pbs") && !is.null(athlytics_sample_pbs)) {
  message("\n测试4：使用模拟数据测试PBs")
  print("PBs模拟数据预览：")
  print(head(athlytics_sample_pbs))

  message("绘制PBs图表...")
  tryCatch({
    p <- plot_pbs(pb_df = athlytics_sample_pbs)
    print(p)  # 显示图表
    message("PBs图表绘制成功")
  }, error = function(e) {
    message("PBs图表绘制失败：", e$message)
  })
}

# 测试5：使用模拟数据测试Decoupling计算和绘图
if (exists("athlytics_sample_decoupling") && !is.null(athlytics_sample_decoupling)) {
  message("\n测试5：使用模拟数据测试Decoupling")
  print("Decoupling模拟数据预览：")
  print(head(athlytics_sample_decoupling))

  message("绘制Decoupling图表...")
  tryCatch({
    p <- plot_decoupling(decoupling_df = athlytics_sample_decoupling)
    print(p)  # 显示图表
    message("Decoupling图表绘制成功")
  }, error = function(e) {
    message("Decoupling图表绘制失败：", e$message)
  })
}
remove.packages("Athlytics")
message("\n--- 模拟数据测试完成 ---")
devtools::load_all(".")
devtools::document()
devtools::ins Beautiful.tall()
pkgdown::build_site()
devtools::install(upgrade = 'never', quiet = FALSE)
devtools::install()
devtools::check(cran = TRUE)
devtools::document()
devtools::build()
devtools::check_built(path = "C:/Users/Ang/Documents/GitHub/Athlytics_0.1.1.tar.gz", cran = TRUE)
devtools::load_all()
devtools::run_examples()
tempdir()
