library(tidyverse)


# --- 步骤 2: 定义文件路径 ---
# 解释: 请将下面的路径修改为您之前生成的两个CSV文件的实际路径。

# 包含12个地类像素级数据的文件路径
# 这是由 "pixel_extraction_workflow_r_cn" 脚本生成的
class_data_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/pixel_level_ndvi_timeseries.csv"

# 包含4个站点像素级数据的文件路径
# 这是由 "site_pixel_extraction_workflow_r_cn" 脚本生成的
site_data_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/pixel_level_ndvi_timeseries_poly.csv"

# 您希望保存最终合并结果的输出路径
output_combined_csv_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/combined_pixel_level_data.csv"
output_cleaned_csv_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/cleaned_combined_pixel_level_data.csv"

# --- 步骤 3: 读取并准备两个数据集 ---
cat("--- 正在读取并准备数据 ---\n")

# 读取地类数据
tryCatch({
  class_df <- read_csv(class_data_path)
}, error = function(e) {
  stop("无法加载地类数据文件，请检查路径是否正确: ", class_data_path)
})

# 读取站点数据
tryCatch({
  site_df <- read_csv(site_data_path)
}, error = function(e) {
  stop("无法加载站点数据文件，请检查路径是否正确: ", site_data_path)
})

# 准备地类数据
# 1. 添加一个AnalysisType列，用于标识数据来源
# 2. 将ClassID列重命名为一个统一的名称，例如 UnitName
class_df_prepared <- class_df %>%
  mutate(AnalysisType = "Regional Class") %>%
  rename(UnitName = ClassID)

# 准备站点数据
# 1. 添加一个AnalysisType列
# 2. 将SiteName列重命名为与地类数据统一的名称 UnitName
site_df_prepared <- site_df %>%
  mutate(AnalysisType = "Specific Site") %>%
  rename(UnitName = SiteName)


# --- 步骤 4: 合并两个数据集 ---
cat("--- 正在合并两个数据集 ---\n")

# 解释: 使用dplyr包的bind_rows()函数将两个数据框上下堆叠在一起。
# 这个函数很智能，会自动根据列名进行匹配。
combined_df <- bind_rows(class_df_prepared, site_df_prepared)


# --- 步骤 5: 检查并保存最终结果 ---
cat("--- 正在将合并后的结果保存到CSV文件 ---\n")

# 检查合并后的数据框维度
cat(paste("合并后的数据框包含", nrow(combined_df), "行和", ncol(combined_df), "列。\n"))

# 保存到CSV文件
write_csv(combined_df, output_combined_csv_path)

cat(paste("\n处理完成！\n最终的合并数据已保存到:", output_combined_csv_path, "\n"))

# 打印最终数据的前几行和后几行以供预览，检查合并是否成功
cat("\n--- 合并数据预览 (前5行) ---\n")
print(head(combined_df, 5))

cat("\n--- 合并数据预览 (后5行) ---\n")
print(tail(combined_df, 5))

# --- 步骤 5: 定义异常值检测与插值函数 ---
# 解释: 我们将异常值处理逻辑封装成一个函数，以便后续高效调用。
# 这个函数会接收一个数据框（代表一个像素的时间序列），并返回一个清理过的NDVI向量。
clean_pixel_timeseries <- function(df) {
  
  # 确保数据按时间排序
  df <- df %>% arrange(Year, Month)
  
  # 计算移动平均和标准差
  df <- df %>%
    mutate(
      rolling_mean = zoo::rollmean(NDVI, k = 5, fill = NA, align = "center"),
      rolling_sd = zoo::rollapply(NDVI, width = 5, FUN = sd, fill = NA, align = "center")
    )
  
  # 识别异常值
  df <- df %>%
    mutate(
      is_outlier = abs(NDVI - rolling_mean) > 3 * rolling_sd & !is.na(rolling_sd),
      is_invalid = NDVI < -0.2 | NDVI > 1.0,
      ndvi_diff = NDVI - lag(NDVI),
      sudden_drop = ndvi_diff < -0.3 & lead(ndvi_diff) > 0.3,
      is_problematic = is_outlier | is_invalid | sudden_drop
    )
  
  # 插值
  df <- df %>%
    mutate(
      NDVI_clean = if_else(is_problematic, NA_real_, NDVI),
      NDVI_interpolated = zoo::na.approx(NDVI_clean, maxgap = 2, na.rm = FALSE)
    )
  
  # 返回最终清理过的NDVI值，如果插值失败则保留原始值
  final_ndvi <- coalesce(df$NDVI_interpolated, df$NDVI)
  
  return(final_ndvi)
}


# ==================== 新增步骤 ====================
# --- 步骤 6: 对每个像素进行异常值处理 ---
cat("--- 正在对每个像素的时间序列进行异常值检测与插值 ---\n")
cat("--- 这个过程可能会非常耗时，请耐心等待... ---\n")

# 解释:
# 1. 我们按 Pixel_ID 和 UnitName 进行分组。
# 2. 对于每个分组（即每个像素的时间序列），我们调用之前定义的 clean_pixel_timeseries 函数。
# 3. 将返回的干净的NDVI值，作为一个新列 `NDVI_cleaned` 添加到数据框中。
# 4. nest() 和 unnest() 是处理这种分组应用函数的标准高效方法。
cleaned_df <- combined_df %>%
  group_by(Pixel_ID, UnitName, AnalysisType) %>%
  # nest() 会将每个像素的所有时间序列数据（Year, Month, NDVI等）“折叠”起来
  nest() %>%
  # 我们对这个折叠起来的data列（它是一个列表，每个元素都是一个数据框）
  # 应用我们的清理函数
  mutate(
    NDVI_cleaned = map(data, clean_pixel_timeseries)
  ) %>%
  # unnest() 会将数据重新“展开”回长格式
  unnest(cols = c(data, NDVI_cleaned)) %>%
  # ungroup() 是一个好习惯，表示我们完成了分组操作
  ungroup()

# --- 步骤 7: 准备并保存最终结果 ---
cat("--- 正在准备并保存最终的干净数据 ---\n")

# 1. 用清理过的值替换原始的NDVI列
# 2. 选择最终需要的列
final_cleaned_df <- cleaned_df %>%
  mutate(NDVI = NDVI_cleaned) %>%
  select(Pixel_ID, x, y, UnitName, AnalysisType, Year, Month, NDVI)

# 检查最终数据框的维度
cat(paste("清理后的数据框包含", nrow(final_cleaned_df), "行和", ncol(final_cleaned_df), "列。\n"))

# 保存到CSV文件
write_csv(final_cleaned_df, output_cleaned_csv_path)

cat(paste("\n处理完成！\n最终的干净数据已保存到:", output_cleaned_csv_path, "\n"))

# 打印最终数据的前几行以供预览
print(head(final_cleaned_df))
























