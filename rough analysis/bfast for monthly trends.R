library(tidyverse) 
library(bfast)     
library(ggplot2)  

# PART A: 4个再野化站点的分析 (Analysis for 4 Rewilding Sites) ----
# A1. Read Data ----
path_4sites_annual_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_4Sites_Annual.csv"
path_4sites_monthly_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_4Sites_Monthly.csv"

sites_annual_data_raw <- read_csv(path_4sites_annual_data)
sites_monthly_data_raw <- read_csv(path_4sites_monthly_data)

# A2. Pre-process Data ----
sites_annual_data <- sites_annual_data_raw %>%
  rename(
    UnitName = Site_ID, 
    IndexValue = MEAN
  ) %>%
  separate(SourceFile, into = c("IndexType", "Year"), sep = "_", remove = FALSE) %>%
  mutate(Year = as.numeric(Year))

sites_monthly_data <- sites_monthly_data_raw %>%
  rename(
    UnitName = Site_ID, 
    IndexValue = MEAN
  ) %>%
  separate(SourceFile, into = c("IndexType", "Year", "Month"), sep = "_", remove = FALSE) %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.numeric(Month)
  )


# A3. Analysis Loop ----
sites_to_analyze <- c("MW", "BB", "OW", "EW")
indices_to_analyze <- c("NDVI")

all_models_sites <- list()
cv_comparison_sites <- data.frame()

for (site in sites_to_analyze) {
  for (index in indices_to_analyze) {
    
    print(paste("\n=== 分析站点:", site, "- 指数:", index, "==="))
    
    # --- 年度数据分析 ---
    annual_data_subset <- sites_annual_data %>%
      filter(UnitName == site, IndexType == index) %>%
      arrange(Year)
    
    # 年度线性回归
    lm_annual <- lm(IndexValue ~ Year, data = annual_data_subset)
    cv_lm_annual <- (sd(residuals(lm_annual)) / mean(annual_data_subset$IndexValue, na.rm = TRUE)) * 100
    all_models_sites[[paste(site, "LM_Annual", sep="_")]] <- lm_annual
    
    # 年度BFAST
    ts_annual <- ts(annual_data_subset$IndexValue, start = min(annual_data_subset$Year), frequency = 1)
    bfast_annual <- tryCatch({
      bfast(ts_annual, h = 0.25, season = "none", max.iter = 5)
    }, error = function(e) NULL)
    
    if (!is.null(bfast_annual)) {
      all_models_sites[[paste(site, "BFAST_Annual", sep="_")]] <- bfast_annual
    }
    
    # --- 月度数据分析 ---
    monthly_data_subset <- sites_monthly_data %>%
      filter(UnitName == site, IndexType == index) %>%
      arrange(Year, Month) %>%
      mutate(
        Time = (Year - min(Year)) * 12 + Month,
        MonthFactor = as.factor(Month)
      )
    
    # 月度线性回归 (含季节项)
    lm_monthly <- lm(IndexValue ~ Time + MonthFactor, data = monthly_data_subset)
    cv_lm_monthly <- (sd(residuals(lm_monthly)) / mean(monthly_data_subset$IndexValue, na.rm = TRUE)) * 100
    all_models_sites[[paste(site, "LM_Monthly", sep="_")]] <- lm_monthly
    
    # 月度BFAST
    full_time_grid <- tibble(Year = rep(1993:2024, each = 12), Month = rep(1:12, times = 32))
    ts_data_full <- full_time_grid %>% left_join(monthly_data_subset, by = c("Year", "Month"))
    ts_monthly <- ts(ts_data_full$IndexValue, start = c(1993, 1), frequency = 12)
    
    bfast_monthly <- tryCatch({
      bfast(ts_monthly, h = 0.15, season = "harmonic", max.iter = 5)
    }, error = function(e) NULL)
    
    if (!is.null(bfast_monthly)) {
      all_models_sites[[paste(site, "BFAST_Monthly", sep="_")]] <- bfast_monthly
      cv_bfast_monthly_raw <- sd(ts_monthly, na.rm=TRUE) / mean(ts_monthly, na.rm=TRUE) * 100
    } else {
      cv_bfast_monthly_raw <- NA
    }
    
    # --- 汇总结果 ---
    cv_comparison_sites <- rbind(cv_comparison_sites, data.frame(
      Unit = site,
      Index = index,
      CV_LM_Annual = round(cv_lm_annual, 2),
      CV_LM_Monthly = round(cv_lm_monthly, 2),
      CV_BFAST_Monthly_Raw = round(cv_bfast_monthly_raw, 2)
    ))
  }
}

print("\n=== 4个站点CV比较汇总表 ===")
print(cv_comparison_sites)


# PART B: 12个土地利用变化类别的分析 (Analysis for 12 LULC Classes) ----

# B1. 读取数据 (Read Data) ----
path_12classes_annual_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_14Class_Annual.csv"
path_12classes_monthly_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_14Class_Monthly.csv"

if (file.exists(path_12classes_annual_data) && file.exists(path_12classes_monthly_data)) {
  
  classes_annual_data_raw <- read_csv(path_12classes_annual_data)
  classes_monthly_data_raw <- read_csv(path_12classes_monthly_data)
  
  # B2. 数据预处理 (Pre-process Data) ----
  # 使用您提供的逻辑进行预处理
  
  # 首先创建一个可复用的处理函数
  process_class_data <- function(df) {
    df %>%
      # 重命名列以匹配您的CSV（Value和MEAN）
      rename(IndexValue = MEAN) %>%
      # 使用case_when创建描述性名称
      mutate(
        ClassName = case_when(
          Value == 1  ~ "EarlyRW-Forest",
          Value == 2  ~ "EarlyRW-Grass",
          Value == 3  ~ "EarlyRW-Wetland",
          Value == 4  ~ "EarlyRW-Unstable",
          Value == 11 ~ "LateRW-Forest",
          Value == 12 ~ "LateRW-Grass",
          Value == 13 ~ "LateRW-Wetland",
          Value == 31 ~ "Stable-Forest",
          Value == 32 ~ "Stable-NatGrass",
          Value == 33 ~ "Stable-Wetland",
          Value == 41 ~ "Stable-AgriGrass",
          Value == 42 ~ "Stable-AgriCrops",
          Value == 20 ~ "Water",
          Value == 21 ~ "Built-up",
          TRUE        ~ "Unknown"
        )
      ) %>%
      # 根据您的要求，去除"Water"和"Built-up"类别，不参与分析
      filter(!ClassName %in% c("Water", "Built-up", "Unknown"))
  }
  
  # 处理年度数据
  classes_annual_data <- process_class_data(classes_annual_data_raw) %>%
    separate(SourceFile, into = c("IndexType", "Year"), sep = "_", remove = FALSE) %>%
    mutate(Year = as.numeric(Year))
  
  # 处理月度数据
  classes_monthly_data <- process_class_data(classes_monthly_data_raw) %>%
    separate(SourceFile, into = c("IndexType", "Year", "Month"), sep = "_", remove = FALSE) %>%
    mutate(
      Year = as.numeric(Year),
      Month = as.numeric(Month)
    )
  
  print("12个类别的数据读取与处理完成。")
  
  # B3. 分析循环 (Analysis Loop) ----
  # 直接从处理好的数据中获取要分析的类别列表
  classes_to_analyze <- unique(classes_annual_data$ClassName)
  
  all_models_classes <- list()
  cv_comparison_classes <- data.frame()
  
  for (class_name in classes_to_analyze) {
    for (index in indices_to_analyze) {
      
      print(paste("\n=== 分析类别:", class_name, "- 指数:", index, "==="))
      
      # --- 年度数据分析 ---
      annual_data_subset <- classes_annual_data %>%
        filter(ClassName == class_name, IndexType == index) %>%
        arrange(Year)
      
      # 年度线性回归
      lm_annual <- lm(IndexValue ~ Year, data = annual_data_subset)
      cv_lm_annual <- (sd(residuals(lm_annual)) / mean(annual_data_subset$IndexValue, na.rm = TRUE)) * 100
      all_models_classes[[paste(class_name, "LM_Annual", sep="_")]] <- lm_annual
      
      # 年度BFAST
      ts_annual <- ts(annual_data_subset$IndexValue, start = min(annual_data_subset$Year), frequency = 1)
      bfast_annual <- tryCatch({
        bfast(ts_annual, h = 0.25, season = "none", max.iter = 5)
      }, error = function(e) NULL)
      
      if (!is.null(bfast_annual)) {
        all_models_classes[[paste(class_name, "BFAST_Annual", sep="_")]] <- bfast_annual
      }
      
      # --- 月度数据分析 ---
      monthly_data_subset <- classes_monthly_data %>%
        filter(ClassName == class_name, IndexType == index) %>%
        arrange(Year, Month) %>%
        mutate(
          Time = (Year - min(Year)) * 12 + Month,
          MonthFactor = as.factor(Month)
        )
      
      # 月度线性回归 (含季节项)
      lm_monthly <- lm(IndexValue ~ Time + MonthFactor, data = monthly_data_subset)
      cv_lm_monthly <- (sd(residuals(lm_monthly)) / mean(monthly_data_subset$IndexValue, na.rm = TRUE)) * 100
      all_models_classes[[paste(class_name, "LM_Monthly", sep="_")]] <- lm_monthly
      
      # 月度BFAST
      full_time_grid <- tibble(Year = rep(1993:2024, each = 12), Month = rep(1:12, times = 32))
      ts_data_full <- full_time_grid %>% left_join(monthly_data_subset, by = c("Year", "Month"))
      ts_monthly <- ts(ts_data_full$IndexValue, start = c(1993, 1), frequency = 12)
      
      bfast_monthly <- tryCatch({
        bfast(ts_monthly, h = 0.15, season = "harmonic", max.iter = 5)
      }, error = function(e) NULL)
      
      if (!is.null(bfast_monthly)) {
        all_models_classes[[paste(class_name, "BFAST_Monthly", sep="_")]] <- bfast_monthly
        cv_bfast_monthly_raw <- sd(ts_monthly, na.rm=TRUE) / mean(ts_monthly, na.rm=TRUE) * 100
      } else {
        cv_bfast_monthly_raw <- NA
      }
      
      # --- 汇总结果 ---
      cv_comparison_classes <- rbind(cv_comparison_classes, data.frame(
        Unit = class_name,
        Index = index,
        CV_LM_Annual = round(cv_lm_annual, 2),
        CV_LM_Monthly = round(cv_lm_monthly, 2),
        CV_BFAST_Monthly_Raw = round(cv_bfast_monthly_raw, 2)
      ))
    }
  }
}

# ===================================================================
# PART C: 可视化比较 (Comparative Visualization)
# ===================================================================

# C1. 可视化4个站点的CV比较 ----
cv_long_sites <- cv_comparison_sites %>%
  select(Unit,
         `Linear (Annual)` = CV_LM_Annual,
         `Linear (Monthly)` = CV_LM_Monthly,
         `BFAST (Monthly, Raw Data)` = CV_BFAST_Monthly_Raw) %>%
  pivot_longer(cols = -Unit, names_to = "Method", values_to = "CV") %>%
  filter(!is.na(CV))

plot_sites_cv <- ggplot(cv_long_sites, aes(x = Unit, y = CV, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f", CV)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(title = "4个站点的模型变异系数(CV)比较",
       subtitle = "CV越低，模型对数据的解释度越高",
       x = "站点 (Site)",
       y = "变异系数 CV (%)",
       fill = "方法 (Method)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_sites_cv)


# C2. 可视化12个类别的CV比较 ----
if (exists("cv_comparison_classes")) {
  cv_long_classes <- cv_comparison_classes %>%
    select(Unit,
           `Linear (Annual)` = CV_LM_Annual,
           `Linear (Monthly)` = CV_LM_Monthly,
           `BFAST (Monthly, Raw Data)` = CV_BFAST_Monthly_Raw) %>%
    pivot_longer(cols = -Unit, names_to = "Method", values_to = "CV") %>%
    filter(!is.na(CV))
  
  # 确保类别的顺序是固定的，而不是按字母顺序
  cv_long_classes$Unit <- factor(cv_long_classes$Unit, levels = unique(cv_comparison_classes$Unit))
  
  plot_classes_cv <- ggplot(cv_long_classes, aes(x = Unit, y = CV, fill = Method)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = sprintf("%.1f", CV)),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3) +
    labs(title = "12个土地利用变化类别的模型变异系数(CV)比较",
         subtitle = "CV越低，模型对数据的解释度越高",
         x = "类别 (Class)",
         y = "变异系数 CV (%)",
         fill = "方法 (Method)") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) # 旋转X轴标签以防重叠
  
  print(plot_classes_cv)
}


# ===================================================================
# PART D: 绘制BFAST分解图 (Plot BFAST Decompositions)
# ===================================================================
# 备注：此函数将绘制所有检测到断点的BFAST模型图。
# Note: This function will plot all BFAST models where breakpoints were detected.

plot_bfast_results <- function(model_list) {
  print("\n--- 绘制BFAST分解图 ---")
  for (model_name in names(model_list)) {
    if (grepl("BFAST", model_name)) {
      model <- model_list[[model_name]]
      if (!is.null(model) && "bfast" %in% class(model)) {
        # 确定是否有断点
        if (grepl("Monthly", model_name)) {
          has_bp <- !model$nobp$Vt || !model$nobp$Wt
        } else { # Annual
          has_bp <- length(model$output) > 1 && !is.na(model$output[[2]]$Vt.bp)
        }
        
        if (has_bp) {
          print(paste("检测到断点，正在绘制:", model_name))
          plot(model, main = paste("BFAST分解 -", model_name))
        }
      }
    }
  }
}

# 为站点和类别分别调用绘图函数
plot_bfast_results(all_models_sites)
if (exists("all_models_classes")) {
  plot_bfast_results(all_models_classes)
}

print("\n分析全部完成！")


# ===================================================================
# 如何从BFAST模型中提取精确的断点时间
# How to Extract Precise Breakpoint Timing from a BFAST Model
# ===================================================================

# 假设您的主分析脚本已经运行完毕，
# 所有的模型都已经存储在 all_models_sites 和 all_models_classes 两个列表中。
# We assume your main analysis script has been run, and all models are stored.

# --- 1. 定义一个用于转换时间的辅助函数 (无变化) ----
decimal_year_to_date <- function(dec_year) {
  if (length(dec_year) == 0 || is.null(dec_year) || is.na(dec_year)) {
    return("无断点 (No Breakpoint)")
  }
  year <- floor(dec_year)
  day_of_year <- ceiling((dec_year - year) * 365.25)
  dt <- as.Date(paste(year, 1, 1, sep = "-")) + (day_of_year - 1)
  return(format(dt, "%Y-%m"))
}


# --- 2. 检查一个具体的月度BFAST模型 (已修正) ----
# 让我们以 "EW_BFAST_Monthly" 为例
model_to_inspect <- all_models_sites[["EW_BFAST_Monthly"]]

# 检查模型是否存在
if (!is.null(model_to_inspect)) {
  
  # === 核心修正：直接从模型主干提取所有断点 ===
  # The model$Vt.bp 和 model$St.bp 分别包含所有趋势和季节性断点的索引
  trend_bp_indices <- model_to_inspect$Vt.bp 
  season_bp_indices <- model_to_inspect$St.bp
  
  # 从模型的时间向量中，通过索引找到实际的小数年份
  time_vector <- time(model_to_inspect$Yt)
  
  # 处理可能存在的多个断点
  trend_decimal_years <- if (any(!is.na(trend_bp_indices))) time_vector[trend_bp_indices] else NA
  season_decimal_years <- if (any(!is.na(season_bp_indices))) time_vector[season_bp_indices] else NA
  
  # 打印正确的小数年份
  print(paste("EW站点的趋势断点 (小数年份):", paste(round(trend_decimal_years, 2), collapse=", ")))
  print(paste("EW站点的季节性断点 (小数年份):", paste(round(season_decimal_years, 2), collapse=", ")))
  
  # 使用我们的辅助函数进行转换
  trend_bp_dates <- sapply(trend_decimal_years, decimal_year_to_date)
  season_bp_dates <- sapply(season_decimal_years, decimal_year_to_date)
  
  print(paste("===> EW站点的趋势断点精确时间:", paste(trend_bp_dates, collapse=", ")))
  print(paste("===> EW站点的季节性断点精确时间:", paste(season_bp_dates, collapse=", ")))
  
}


# --- 3. 循环所有模型，创建一个断点汇总表 (已修正) ----
breakpoint_summary <- data.frame(
  ModelName = character(),
  TrendBreakpoint = character(),
  SeasonBreakpoint = character()
)

# 首先合并两个模型列表
all_models_combined <- c(all_models_sites, all_models_classes)

for (model_name in names(all_models_combined)) {
  # 只处理月度BFAST模型
  if (grepl("BFAST_Monthly", model_name)) {
    model <- all_models_combined[[model_name]]
    
    # 检查模型是否有效
    if (!is.null(model) && "bfast" %in% class(model)) {
      
      # === 核心修正：使用正确的逻辑提取所有断点时间 ===
      time_vector <- time(model$Yt)
      
      # 提取趋势断点
      trend_bp_indices <- model$Vt.bp
      trend_decimal_years <- if (any(!is.na(trend_bp_indices))) time_vector[trend_bp_indices] else NA
      trend_dates_str <- paste(sapply(trend_decimal_years, decimal_year_to_date), collapse=", ")
      
      # 提取季节性断点
      season_bp_indices <- model$St.bp
      season_decimal_years <- if (any(!is.na(season_bp_indices))) time_vector[season_bp_indices] else NA
      season_dates_str <- paste(sapply(season_decimal_years, decimal_year_to_date), collapse=", ")
      
      # 只有当至少一个断点被找到时才添加到汇总表
      if (trend_dates_str != "无断点 (No Breakpoint)" || season_dates_str != "无断点 (No Breakpoint)") {
        breakpoint_summary <- rbind(breakpoint_summary, data.frame(
          ModelName = model_name,
          TrendBreakpoint = trend_dates_str,
          SeasonBreakpoint = season_dates_str
        ))
      }
    }
  }
}

# 打印最终的断点汇总表
print("\n--- 所有检测到的断点精确时间汇总 ---")
print(breakpoint_summary)
