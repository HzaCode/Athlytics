# 创建包含多个运动员的示例数据
library(Athlytics)
library(dplyr)
library(tibble)

set.seed(123)  # 确保可重复性

# ===================================
# 1. ACWR 示例数据 - 添加 3 个运动员
# ===================================
cat("生成 ACWR 多运动员示例数据...\n")

# 加载原始数据
data("athlytics_sample_acwr", package = "Athlytics")
original_acwr <- athlytics_sample_acwr

# 创建 3 个运动员的数据
athlete_ids <- c("Athlete_A", "Athlete_B", "Athlete_C")
acwr_list <- list()

for (i in seq_along(athlete_ids)) {
  athlete_data <- original_acwr %>%
    mutate(
      # 为不同运动员添加一些变化
      atl = atl * runif(1, 0.8, 1.2),
      ctl = ctl * runif(1, 0.8, 1.2),
      acwr = atl / ctl,
      acwr_smooth = acwr_smooth * runif(1, 0.85, 1.15),
      athlete_id = athlete_ids[i]
    ) %>%
    select(athlete_id, everything())  # 把 athlete_id 放在第一列
  
  acwr_list[[i]] <- athlete_data
}

athlytics_sample_acwr <- bind_rows(acwr_list)
cat("  - 运动员数量:", length(unique(athlytics_sample_acwr$athlete_id)), "\n")
cat("  - 总行数:", nrow(athlytics_sample_acwr), "\n")

# ===================================
# 2. EF 示例数据 - 添加 3 个运动员
# ===================================
cat("\n生成 EF 多运动员示例数据...\n")

# 加载原始数据
data("athlytics_sample_ef", package = "Athlytics")
original_ef <- athlytics_sample_ef

ef_list <- list()

for (i in seq_along(athlete_ids)) {
  athlete_data <- original_ef %>%
    mutate(
      # 为不同运动员添加一些变化
      ef_value = ef_value * runif(1, 0.85, 1.15),
      athlete_id = athlete_ids[i]
    ) %>%
    select(athlete_id, everything())
  
  ef_list[[i]] <- athlete_data
}

athlytics_sample_ef <- bind_rows(ef_list)
cat("  - 运动员数量:", length(unique(athlytics_sample_ef$athlete_id)), "\n")
cat("  - 总行数:", nrow(athlytics_sample_ef), "\n")

# ===================================
# 3. Decoupling 示例数据 - 保持单运动员
# ===================================
cat("\n保留 Decoupling 单运动员示例数据...\n")
# Decoupling 通常是单个活动的分析，保持单运动员数据更合适
data("athlytics_sample_decoupling", package = "Athlytics")
# 不修改 decoupling 数据

cat("  - 总行数:", nrow(athlytics_sample_decoupling), "\n")

# ===================================
# 4. 保存数据到 data 目录
# ===================================
cat("\n保存示例数据到 data/ 目录...\n")

save(athlytics_sample_acwr, file = "data/athlytics_sample_acwr.rda", compress = "xz")
cat("  ✓ 已保存: data/athlytics_sample_acwr.rda\n")

save(athlytics_sample_ef, file = "data/athlytics_sample_ef.rda", compress = "xz")
cat("  ✓ 已保存: data/athlytics_sample_ef.rda\n")

save(athlytics_sample_decoupling, file = "data/athlytics_sample_decoupling.rda", compress = "xz")
cat("  ✓ 已保存: data/athlytics_sample_decoupling.rda\n")

# ===================================
# 5. 验证保存的数据
# ===================================
cat("\n验证保存的数据...\n")

# 清空环境
rm(list = ls())

# 重新加载
load("data/athlytics_sample_acwr.rda")
cat("ACWR: ", nrow(athlytics_sample_acwr), "行, ", 
    length(unique(athlytics_sample_acwr$athlete_id)), "个运动员\n")

load("data/athlytics_sample_ef.rda")
cat("EF: ", nrow(athlytics_sample_ef), "行, ", 
    length(unique(athlytics_sample_ef$athlete_id)), "个运动员\n")

load("data/athlytics_sample_decoupling.rda")
cat("Decoupling: ", nrow(athlytics_sample_decoupling), "行\n")

cat("\n✅ 所有示例数据已成功更新！\n")


