library(tidyverse)
library(zoo)

ndvi_df_raw <- read_csv("E:/WUR_Intern/RewildingProject_RawData/NDVI/monthly_data_combined.csv")

clean_ndvi_data <- function(df) {
  
  # 步骤1：识别潜在的异常值
  # 我们对每个'ClassName'进行独立的时间序列分析
  ndvi_cleaned <- df %>%
    # 从 Year 和 Month 列创建标准的日期列
    mutate(date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    # 按类别分组，并按日期排序
    group_by(ClassName) %>%
    arrange(date) %>%
    mutate(
      # 计算5个时间点的移动平均和移动标准差
      # 'align = "center"' 使窗口居中，更适合异常值检测
      rolling_mean = zoo::rollmean(IndexValue, k = 5, fill = NA, align = "center"),
      rolling_sd = zoo::rollapply(IndexValue, width = 5, FUN = sd, fill = NA, align = "center"),
      
      # 条件1：统计异常值（偏离移动平均超过3个标准差）
      deviation = abs(IndexValue - rolling_mean),
      is_outlier = deviation > 3 * rolling_sd & !is.na(rolling_sd),
      
      # 条件2：物理范围异常值（IndexValue值超出常规范围）
      # -0.2 是一个相对宽松的下限，考虑到裸土等情况
      is_invalid = IndexValue < -0.2 | IndexValue > 1.0,
      
      # 条件3：V型骤降（可能是云/雪/阴影污染的典型特征）
      # 计算当前值与前一个值的差异
      ndvi_diff = IndexValue - lag(IndexValue),
      # 判断是否为V型：急剧下降后紧接着急剧上升
      sudden_drop = ndvi_diff < -0.3 & lead(ndvi_diff) > 0.3,
      
      # 综合标记：任何一个条件满足，即被视为有问题的数据点
      is_problematic = is_outlier | is_invalid | sudden_drop
    )
  
  # 步骤2：生成并打印数据质量问题汇总报告
  problem_summary <- ndvi_cleaned %>%
    filter(is_problematic) %>%
    group_by(ClassName) %>%
    summarise(
      n_outliers = sum(is_outlier, na.rm = TRUE),
      n_invalid = sum(is_invalid, na.rm = TRUE),
      n_sudden_drops = sum(sudden_drop, na.rm = TRUE),
      total_problems = n(),
      # 列出所有有问题数据点的日期
      problem_dates = list(date[is_problematic])
    )
  
  cat("--- (函数内)数据质量问题汇总 ---\n")
  if (nrow(problem_summary) > 0) {
    print(as.data.frame(problem_summary))
  } else {
    cat("未发现任何异常值。\n")
  }
  cat("----------------------------------\n\n")
  
  # 步骤3：处理异常值
  ndvi_processed <- ndvi_cleaned %>%
    mutate(
      # 将有问题的数据点设置为NA (Not Available)
      NDVI_clean = if_else(is_problematic, NA_real_, IndexValue)
    ) %>%
    # 再次按类别分组，以进行独立的插值处理
    group_by(ClassName) %>%
    # 使用线性插值填充NA值
    # 'maxgap = 2' 限制了连续插值的最大数量，避免在数据缺失过多的地方进行不合理的插值
    mutate(
      NDVI_interpolated = zoo::na.approx(NDVI_clean, maxgap = 2, na.rm = FALSE)
    )
  
  # 返回一个包含清理后数据和汇总表的列表
  return(list(cleaned_data = ndvi_processed, summary_table = problem_summary))
}

# 4. 可视化检查函数
visualize_cleaning <- function(df, class_to_check) {
  
  # 筛选出要检查的特定类别的数据
  plot_data <- df %>%
    filter(ClassName == class_to_check) %>%
    select(date, NDVI_original = IndexValue, NDVI_interpolated, is_problematic)
  
  # 检查是否有数据用于绘图
  if(nrow(plot_data) == 0) {
    warning(paste("找不到类别 '", class_to_check, "' 的数据。", sep=""))
    return(invisible(NULL))
  }
  
  p <- ggplot(plot_data, aes(x = date)) +
    # 原始数据点和线
    geom_line(aes(y = NDVI_original, color = "Original"), alpha = 0.6) +
    geom_point(aes(y = NDVI_original, color = "Original"), alpha = 0.6) +
    
    # 清理后的数据线
    geom_line(aes(y = NDVI_interpolated, color = "Cleaned"), size = 1) +
    
    # 标记出被识别为异常值的点
    geom_point(data = filter(plot_data, is_problematic), 
               aes(y = NDVI_original, shape = "Outlier"), color = "red", size = 3, stroke = 1.5) +
    
    # 美化图表
    scale_color_manual(name = "Data", values = c("Original" = "grey50", "Cleaned" = "steelblue")) +
    scale_shape_manual(name = "", values = c("Outlier" = 4)) +
    labs(
      title = paste("NDVI 数据清理效果:", class_to_check),
      subtitle = "对比原始数据、异常点和清理/插值后的数据",
      x = "Date",
      y = "NDVI (IndexValue)"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
  
  print(p)
}


# --- 执行流程 ---

# 1. 运行清理函数，并获取结果列表
cleaning_results <- clean_ndvi_data(ndvi_df_raw)

# 从列表中提取清理后的数据框和汇总表
ndvi_df_cleaned <- cleaning_results$cleaned_data
outlier_summary_table <- cleaning_results$summary_table

# 2. 打印异常值汇总表
cat("--- 最终的异常值汇总表 ---\n")
print(outlier_summary_table)
cat("--------------------------\n\n")

# 3. 可视化检查特定类别的清理效果
# 你可以更改 "Early Rewilded Natural Grassland" 为任何你想检查的 'ClassName'
# 从 outlier_summary_table 中选择一个有问题的类别进行检查会很有用
cat("正在生成 'Early Rewilded Natural Grassland' 类的可视化图表...\n")
visualize_cleaning(ndvi_df_cleaned, "Early Rewilded Forest")

# 示例：检查另一个类别
# cat("正在生成 'Stable Agriculture Grassland' 类的可视化图表...\n")
# visualize_cleaning(ndvi_df_cleaned, "Stable Agriculture Grassland")



final_cleaned_table <- ndvi_df_cleaned %>%
  # 如果有插值（NDVI_interpolated 不是 NA），则使用插值；否则，保留原始值。
  # 这可以确保即使有无法插值的点，数据也不会丢失。
  mutate(IndexValue = coalesce(NDVI_interpolated, IndexValue)) %>%
  # 选择与原始文件相同的列，并按原顺序排列
  select(ClassName, AnalysisType, IndexType, Year, Month, IndexValue, CategoryGroup)

write_csv(final_cleaned_table, "E:/WUR_Intern/RewildingProject_RawData/NDVI/monthly_data_combined_cleaned.csv")

