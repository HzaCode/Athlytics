# 重新生成图表 - 使用示例数据
# 这个脚本用示例数据重新生成所有在文档中使用的图表

library(Athlytics)
library(ggplot2)
library(dplyr)

# 创建输出目录（如果不存在）
output_dir <- "man/figures"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

message("===== 开始生成图表 =====\n")

# =====================================
# 1. ACWR 图表
# =====================================
message("正在生成 ACWR 图表...")

# 加载 ACWR 示例数据
data("athlytics_sample_acwr", package = "Athlytics")

# 检查数据
message("  - 加载的数据行数: ", nrow(athlytics_sample_acwr))
if ("athlete_id" %in% colnames(athlytics_sample_acwr)) {
  message("  - 包含 athlete_id, 运动员数量: ", length(unique(athlytics_sample_acwr$athlete_id)))
}

# 生成图表 - 直接传递 ACWR 数据框作为第一个参数
p_acwr <- plot_acwr(
  data = athlytics_sample_acwr,
  group_var = if("athlete_id" %in% colnames(athlytics_sample_acwr)) "athlete_id" else NULL,
  highlight_zones = TRUE
)

# 保存图表
ggsave(
  filename = file.path(output_dir, "01b_acwr_multi_group.png"),
  plot = p_acwr,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
message("  ✓ ACWR 图表已保存到: man/figures/01b_acwr_multi_group.png\n")

# =====================================
# 2. EF (效率因子) 图表
# =====================================
message("正在生成 EF 图表...")

# 加载 EF 示例数据
data("athlytics_sample_ef", package = "Athlytics")

# 检查数据
message("  - 加载的数据行数: ", nrow(athlytics_sample_ef))
if ("athlete_id" %in% colnames(athlytics_sample_ef)) {
  message("  - 包含 athlete_id, 运动员数量: ", length(unique(athlytics_sample_ef$athlete_id)))
}

# 生成图表 - 直接传递 EF 数据框作为第一个参数
p_ef <- plot_ef(
  data = athlytics_sample_ef,
  group_var = if("athlete_id" %in% colnames(athlytics_sample_ef)) "athlete_id" else NULL,
  add_trend_line = TRUE
)

# 保存图表
ggsave(
  filename = file.path(output_dir, "02b_ef_multi_group.png"),
  plot = p_ef,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
message("  ✓ EF 图表已保存到: man/figures/02b_ef_multi_group.png\n")

# =====================================
# 3. Decoupling (脱耦) 图表
# =====================================
message("正在生成 Decoupling 图表...")

# 加载 Decoupling 示例数据
data("athlytics_sample_decoupling", package = "Athlytics")

# 检查数据
message("  - 加载的数据行数: ", nrow(athlytics_sample_decoupling))

# 生成图表 - 使用 decoupling_df 参数传递已计算好的数据
p_decoupling <- plot_decoupling(
  decoupling_df = athlytics_sample_decoupling,
  add_trend_line = TRUE
)

# 保存图表
ggsave(
  filename = file.path(output_dir, "05b_decoupling_multi_group.png"),
  plot = p_decoupling,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
message("  ✓ Decoupling 图表已保存到: man/figures/05b_decoupling_multi_group.png\n")

# =====================================
# 完成
# =====================================
message("===== 所有图表生成完成! =====")
message("\n生成的图表:")
message("  1. man/figures/01b_acwr_multi_group.png")
message("  2. man/figures/02b_ef_multi_group.png")
message("  3. man/figures/05b_decoupling_multi_group.png")

