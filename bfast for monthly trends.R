library(tidyverse)
library(bfast)

path_4sites_monthly_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_4Sites_Monthly.csv"
sites_monthly_data_raw <- read_csv(path_4sites_monthly_data)

sites_monthly_data <- sites_monthly_data_raw %>%
  rename(
    SiteName = Site_ID,
    IndexValue = MEAN
  ) %>%
  separate(SourceFile, into = c("IndexType", "Year", "Month"), sep = "_", remove = FALSE) %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.numeric(Month)
  )

print(head(sites_monthly_data))

# 1. Data modelling for BFAST ----
bfast_results_list <- list()
sites_to_analyze <- c("MW", "BB", "OW", "EW")
indices_to_analyze <- c("NDVI")

for (site in sites_to_analyze) {
  for (index in indices_to_analyze) {
    
    current_data <- sites_monthly_data %>%
      filter(SiteName == site, IndexType == index) %>%
      arrange(Year, Month)
    
    if (nrow(current_data) < 24) { 
      print(paste("数据不足，跳过", site, "-", index))
      next 
    }
    
    full_time_grid <- tibble(
      Year = rep(1993:2024, each = 12),
      Month = rep(1:12, times = 32)
    )
    
    ts_data_full <- full_time_grid %>%
      left_join(current_data, by = c("Year", "Month"))
    
    current_ts <- ts(ts_data_full$IndexValue, start = c(1993, 1), frequency = 12)
    
    bfast_result <- tryCatch({
      bfast(current_ts, h = 0.15, season = "harmonic")
    }, error = function(e) {
      print(paste("BFAST在处理", site, "-", index, "时出错:", e$message))
      return(NULL) 
    })
    
    result_name <- paste(site, index, sep = "_")
    bfast_results_list[[result_name]] <- bfast_result
  }
}

print("--- All bfast analysis finished！---")


successful_models <- names(bfast_results_list)[!sapply(bfast_results_list, is.null)]

if (length(successful_models) > 0) {
  print(paste("成功生成了以下BFAST模型:", paste(successful_models, collapse = ", ")))
  
  # 循环绘制所有成功生成的结果图
  for (model_name in successful_models) {
    plot(bfast_results_list[[model_name]], main = paste("BFAST Decomposition for", model_name))
  }
} else {
  print("未成功生成任何BFAST模型，无法绘制图表。请检查您的月度数据完整性。")
}

# 2. BFAST outcome ----
print(bfast_results_list$BB_NDVI)
plot(bfast_results_list$BB_NDVI, main = "BFAST Decomposition for Site: BB (NDVI)")

print(bfast_results_list$MW_NDVI)
plot(bfast_results_list$MW_NDVI, main = "BFAST Decomposition for Site: MW (NDVI)")

print(bfast_results_list$OW_NDVI)
plot(bfast_results_list$OW_NDVI, main = "BFAST Decomposition for Site: OW (NDVI)")

print(bfast_results_list$EW_NDVI)
plot(bfast_results_list$EW_NDVI, main = "BFAST Decomposition for Site: EW (NDVI)")



