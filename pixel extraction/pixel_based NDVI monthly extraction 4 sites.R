library(terra)
library(tidyverse)
library(sf)


# --- 步骤 2: 定义文件路径和参数 ---
# 解释: 请将下面的路径和参数修改为您自己电脑上的实际情况。

# 包含您所有月度NDVI影像的文件夹路径
ndvi_folder_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/monthly/NDVI/"

# 您的多边形文件的完整文件路径 (例如 .shp, .gpkg, 等)
sites_polygon_path <- "E:/WUR_Intern/RewildingProject_RawData/4sites_polygon/4sites_poly.shp"

# 在您的多边形文件的属性表中，用于区分不同站点的列的名称
# 例如，如果您的属性表有一列叫 "Site_Name"，其值为 "OW", "BB"等，就填入 "Site_Name"
site_name_column <- "Site_ID" 

# 您希望保存最终结果的输出路径
output_csv_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/pixel_level_ndvi_timeseries_poly.csv"


# --- 步骤 3: 加载栅格和矢量数据 ---
cat("--- 正在加载栅格和矢量数据 ---\n")

# 加载多边形文件
# 使用sf包的st_read()函数
tryCatch({
  sites_sf <- st_read(sites_polygon_path)
}, error = function(e) {
  stop("无法加载多边形文件，请检查路径是否正确: ", sites_polygon_path)
})
# 将sf对象转换为terra的SpatVector对象，以便与terra包无缝协作
sites_vect <- vect(sites_sf)

# 加载NDVI时间序列 (与之前脚本相同)
ndvi_files <- list.files(path = ndvi_folder_path, pattern = "\\.tif$", full.names = TRUE)
if (length(ndvi_files) == 0) {
  stop("在指定的文件夹中没有找到任何.tif文件: ", ndvi_folder_path)
}
ndvi_stack <- rast(ndvi_files)


# --- 步骤 4: 检查并统一坐标参考系统 (CRS) ---
cat("--- 正在检查坐标参考系统 (CRS) ---\n")
# 解释: 这是矢量与栅格数据叠加分析前至关重要的一步。
if (crs(sites_vect) != crs(ndvi_stack)) {
  cat("警告: 多边形与NDVI栅格的坐标系不匹配。\n")
  cat("正在尝试将多边形的坐标系转换为与NDVI栅格一致...\n")
  # project()函数用于坐标系变换
  sites_vect <- project(sites_vect, crs(ndvi_stack))
}


# --- 步骤 5: 循环提取每个站点的数据 ---
# --- 步骤 5: 循环提取每个站点的数据 (更稳健的版本) ---
cat("--- 开始循环提取每个站点的数据 ---\n")

# 创建一个列表来存储每个站点的处理结果
results_list <- list()
site_names <- sites_vect[[site_name_column]]

for (site in site_names) {
  
  cat(paste("--- 正在处理站点:", site, "---\n"))
  
  # 步骤 5.1: 筛选出当前循环的站点多边形
  current_site_poly <- sites_vect[sites_vect[[site_name_column]] == site, ]
  
  # 步骤 5.2: 使用多边形掩膜和裁切NDVI堆栈
  cat("  -> 正在掩膜和裁切NDVI数据...\n")
  ndvi_masked <- mask(crop(ndvi_stack, current_site_poly), current_site_poly)
  
  # 步骤 5.3: 验证该站点内的数据重叠情况
  any_valid_mask <- app(ndvi_masked, 'any', na.rm = TRUE)
  valid_pixel_count <- global(any_valid_mask, "sum", na.rm = TRUE)$sum
  
  if (is.na(valid_pixel_count) || valid_pixel_count == 0) {
    cat(paste("  -> 警告: 站点", site, "区域内没有找到任何有效的NDVI像素，将跳过。\n"))
    next # 跳到下一个循环
  }
  cat(paste("  -> 验证成功: 在站点", site, "内发现", valid_pixel_count, "个有效像素。\n"))
  
  # 步骤 5.4: 提取像素值到数据框 (稳健方法)
  # 解释: 我们分开提取坐标和像元值，然后合并，以避免as.data.frame的潜在问题。
  cat("  -> 正在提取像素值...\n")
  
  # 提取坐标
  coords_df <- as.data.frame(ndvi_masked, xy = TRUE, na.rm = FALSE)[, c("x", "y")]
  # 提取像元值
  values_df <- as.data.frame(ndvi_masked, na.rm = FALSE)
  
  # 合并并过滤掉完全是NA的行
  site_df_wide <- bind_cols(coords_df, values_df) %>%
    filter(rowSums(is.na(select(., -x, -y))) < nlyr(ndvi_masked))
  
  if(nrow(site_df_wide) == 0) {
    cat(paste("  -> 警告(步骤5.4): 提取和合并后行为0，站点", site, "被跳过。\n"))
    next
  }
  
  # 步骤 5.5: 为数据框添加站点名称标识
  site_df_wide$SiteName <- site
  
  # 步骤 5.6: 将处理好的数据框添加到列表中
  results_list[[length(results_list) + 1]] <- site_df_wide
}
# --- 循环处理结束 ---


# --- 步骤 6: 合并、重塑和清理数据 ---
cat("--- 正在合并所有站点的数据并进行清理 ---\n")

if (length(results_list) == 0) {
  stop("处理完成，但没有从任何站点提取到有效数据。请检查您的多边形是否与NDVI数据有重叠。")
}

# 合并所有站点的数据
combined_wide_df <- bind_rows(results_list)

# 将数据从“宽”格式重塑为“长”格式
long_df <- combined_wide_df %>%
  pivot_longer(
    cols = -c("x", "y", "SiteName"),
    names_to = "Time_Raw",
    values_to = "NDVI"
  )

# 清理并添加最终需要的列
final_df <- long_df %>%
  mutate(
    Year = as.integer(str_extract(Time_Raw, "(?<=_)\\d{4}(?=_)")),
    Month = as.integer(str_extract(Time_Raw, "(?<=_\\d{4}_)\\d{1,2}"))
  ) %>%
  unite("Pixel_ID", c("x", "y"), sep = "_", remove = FALSE) %>%
  select(Pixel_ID, x, y, SiteName, Year, Month, NDVI) %>%
  filter(!is.na(NDVI))


# --- 步骤 7: 保存最终结果 ---
cat("--- 正在将结果保存到CSV文件 ---\n")

write_csv(final_df, output_csv_path)

cat(paste("\n处理完成！\n最终的数据已保存到:", output_csv_path, "\n"))
cat(paste("总共处理了", nrow(final_df), "行数据。\n"))

# 打印最终数据的前几行以供预览
print(head(final_df))

