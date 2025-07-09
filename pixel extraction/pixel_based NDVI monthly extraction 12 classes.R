library(terra)
library(tidyverse)

# Load the NDVI data and classification map
ndvi_folder_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/monthly/NDVI/"
classification_raster_path <- "E:/WUR_Intern/RewildingProject_RawData/Reclassification_map_12classes/Final12classes.tif"

output_csv_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/pixel_level_ndvi_timeseries.csv"

# --- Load raster data---
tryCatch({
  class_rast <- rast(classification_raster_path)
}, error = function(e) {
  stop("无法加载分类栅格，请检查路径是否正确: ", classification_raster_path)
})

ndvi_files <- list.files(path = ndvi_folder_path, 
                         pattern = "\\.tif$", 
                         full.names = TRUE)

if (length(ndvi_files) == 0) {
  stop("在指定的文件夹中没有找到任何.tif文件: ", ndvi_folder_path)
}  

# Spat Raster
ndvi_stack <- rast(ndvi_files)

# --- aggregate NDVI and classification map ---
if (!compareGeom(class_rast, ndvi_stack, stopOnError = FALSE)) {
  cat("警告: 分类栅格与NDVI栅格的空间几何信息不完全匹配。\n")
  cat("正在尝试将分类栅格重采样至与NDVI栅格对齐...\n")
  # 将分类图重采样，使其与NDVI堆栈的网格完全对齐
  # method="near" 用于分类数据，确保类别值不变
  class_rast <- resample(class_rast, ndvi_stack, method = "near")
}

ndvi_masked <- mask(ndvi_stack, class_rast)

class_valid_mask <- !is.na(class_rast)
# 2. 计算NDVI数据中，在任何一个时间点有有效值的区域
#    app(x, 'any') 会检查每个像素的时间序列，只要有一个不是NA，就返回TRUE(1)
ndvi_any_valid_mask <- app(ndvi_masked, 'any', na.rm = TRUE)
# 3. 将两个有效区域相乘。只有在两个图层中都为1的像素，结果才为1
overlap_check <- class_valid_mask * ndvi_any_valid_mask
# 4. 计算重叠区域的总像素数
overlap_pixels <- global(overlap_check, "sum", na.rm = TRUE)$sum

if (is.na(overlap_pixels) || overlap_pixels == 0) {
  stop("关键错误: 分类图的有效区域与NDVI数据的有效区域没有任何空间重叠。\n",
       "这意味着在您划定的所有地块内，没有任何一个像素在任何一个时间点上有有效的NDVI值。\n",
       "请在GIS软件中仔细检查两个数据集的坐标系、空间范围以及由云/水体等造成的NA值掩膜。")
}
cat(paste("验证成功: 发现", overlap_pixels, "个像素在分类区和NDVI数据区存在重叠。\n"))



combined_stack <- c(class_rast, ndvi_masked)

names(combined_stack)[1] <- "ClassID"

class_frequencies <- freq(class_rast)
print(class_frequencies)
plot(class_rast)






# --- extract all the pixels to dataframe ---
# 定义分块数量。如果您的内存仍然不足，可以尝试增加这个数字（例如，50或100）。
# --- 步骤 5: 分块处理与提取 (更稳健的版本) ---
cat("--- 数据量过大，启动分块处理模式 ---\n")

# 定义分块数量。如果您的内存仍然不足，可以尝试增加这个数字（例如，50或100）。
n_chunks <- 20 
rows <- 1:nrow(combined_stack)
# 将所有行号分割成n_chunks个组
row_chunks <- split(rows, cut(rows, n_chunks))

# 创建一个列表来存储每个块的处理结果
results_list <- list()

# 循环处理每个数据块
for (i in 1:length(row_chunks)) {
  
  chunk <- row_chunks[[i]]
  cat(paste("--- 正在处理块", i, "of", n_chunks, "(行:", min(chunk), "至", max(chunk), ") ---\n"))
  
  # 步骤 5.1: 提取当前数据块
  block_rast <- combined_stack[chunk, , drop = FALSE]
  
  # 步骤 5.2: 分开提取分类信息和NDVI信息
  # 解释: 这是解决问题的关键。我们不再对整个大堆栈使用as.data.frame，
  # 而是分开处理，以提高稳健性。
  class_block <- block_rast[[1]] # 提取第一个图层 (ClassID)
  ndvi_block <- block_rast[[-1]] # 提取除第一个图层外的所有NDVI图层
  
  # 将分类信息转换为数据框
  class_df <- as.data.frame(class_block, xy = TRUE, na.rm = FALSE)
  
  # 将NDVI信息转换为数据框
  ndvi_df <- as.data.frame(ndvi_block, na.rm = FALSE)
  
  # 合并两个数据框
  block_df_wide <- bind_cols(class_df, ndvi_df) %>%
    # 移除那些ClassID为NA的行，或者所有NDVI值都为NA的行
    filter(!is.na(ClassID) & rowSums(is.na(select(., -x, -y, -ClassID))) < (nlyr(combined_stack) - 1))
  
  # 如果这个块处理后为空，则跳到下一个块
  if(nrow(block_df_wide) == 0) {
    cat("  -> 诊断信息: 这个数据块不包含任何有效的重叠像素，将跳过。\n")
    next 
  }
  
  # 步骤 5.3: 将小块数据重塑为长格式
  block_df_long <- block_df_wide %>%
    pivot_longer(
      cols = -c("x", "y", "ClassID"),
      names_to = "Time_Raw",
      values_to = "NDVI"
    )
  
  # 步骤 5.4: 将处理好的小块结果添加到列表中
  results_list[[length(results_list) + 1]] <- block_df_long
}
# ==================== 分块处理结束 ====================


# --- 步骤 6: 合并所有块的结果 ---
cat("--- 正在合并所有处理好的数据块 ---\n")
# 使用bind_rows()高效地将列表中的所有数据框合并成一个大的数据框
merged_df_with_na <- bind_rows(results_list)


# ==================== 新增步骤 ====================
# --- 步骤 6.2: 诊断NA值的分布情况 ---
cat("\n--- 正在诊断数据中的NA值分布 ---\n")

total_observations <- nrow(merged_df_with_na)
na_count <- sum(is.na(merged_df_with_na$NDVI))
na_percentage <- (na_count / total_observations) * 100

cat(paste("总观测数 (包括NA):", total_observations, "\n"))
cat(paste("NA值总数:", na_count, "\n"))
cat(paste("NA值占比:", round(na_percentage, 2), "%\n\n"))

# --- 步骤 6.5: 过滤掉不需要的类别和NA值 ---
cat("--- 正在进行最终数据清理 ---\n")

final_df <- merged_df_with_na %>%
  # 1. 过滤掉不需要的类别
  filter(!ClassID %in% c("Water", "Built-up")) %>%
  # 2. 移除NA值
  filter(!is.na(NDVI)) %>%
  # 3. 清理并添加时间信息
  mutate(
    Year = as.integer(str_extract(Time_Raw, "(?<=_)\\d{4}(?=_)")),
    Month = as.integer(str_extract(Time_Raw, "(?<=_\\d{4}_)\\d{1,2}"))
  ) %>%
  unite("Pixel_ID", c("x", "y"), sep = "_", remove = FALSE) %>%
  select(Pixel_ID, x, y, ClassID, Year, Month, NDVI)

# --- write the final df ---
write_csv(final_df, output_csv_path)
cat(paste("\n处理完成！\n最终的数据已保存到:", output_csv_path, "\n"))
cat(paste("总共处理了", nrow(final_df), "行数据。\n"))




