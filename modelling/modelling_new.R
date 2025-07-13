# --- 步骤 1: 加载库和数据 ---
# 解释: 加载所有必需的库。
library(tidyverse)
library(lubridate)
library(nlme)
library(dplyr)

# 定义文件路径 (请修改为您的实际路径)
pixel_data_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/cleaned_combined_pixel_level_data.csv"
events_path <- "E:/WUR_Intern/RewildingProject_RawData/ExtremeEvents_selected/FinalExtremeEvents.csv"

# 读取数据
cat("--- 正在加载像素级数据和事件数据 ---\n")
pixel_df <- read_csv(pixel_data_path)
events_df <- read_csv(events_path)


# --- 步骤 2: 计算NDVI异常值 (Z-score) ---
# 解释: 此步骤与之前相同。
cat("--- 正在计算每个像素的NDVI Z-score ---\n")
ndvi_data_final <- pixel_df %>%
  mutate(month_num = month(ymd(paste(Year, Month, 1)))) %>%
  group_by(UnitName, month_num) %>%
  mutate(
    NDVI_mean_hist = mean(NDVI, na.rm = TRUE),
    NDVI_sd_hist = sd(NDVI, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    NDVI_zscore = (NDVI - NDVI_mean_hist) / NDVI_sd_hist,
    NDVI_zscore = if_else(is.finite(NDVI_zscore), NDVI_zscore, 0),
    NDVI_anomaly = NDVI - NDVI_mean_hist
  ) %>%
  mutate(date = ymd(paste(Year, Month, 1)))


# ==================== 核心修改部分 ====================
# --- 步骤 3: 为每个像素计算多指标、多窗口的恢复力 ---
# 解释: 这是最核心也是最耗时的一步。我们将为每个像素在每次事件中的表现，
# 在多个对称的时间窗口下，计算三个恢复力指标。
cat("--- 正在为每个像素计算多指标恢复力 (此步骤非常耗时) ---\n")

# 准备事件数据
events_prepared <- events_df %>%
  mutate(
    EventID = row_number(),
    StartDate = ymd(StartDate),
    EndDate = ymd(EndDate)
    )

# 直接对所有像素数据进行处理
resilience_metrics_long <- ndvi_with_zscore %>%
  # 按像素ID分组
  group_by(Pixel_ID, UnitName, AnalysisType) %>%
  # nest() 会将每个像素的所有时间序列数据“折叠”进一个名为 'data' 的列
  nest() %>%
  # 现在我们对 'data' 列进行操作。'data' 是一个列表，每个元素都是一个像素的数据框
  mutate(metrics = map(data, function(pixel_data) {
    # 这里的 pixel_data 就是一个独立的、干净的数据框，不会有歧义

    # 遍历所有事件
    map_dfr(1:nrow(events_prepared), function(i) {
      event <- events_prepared[i, ]

      # 定义要计算的时间窗口
      time_windows <- c(3, 6, 9, 12, 15, 18)

      # 遍历所有时间窗口
      map_dfr(time_windows, function(w) {

        # 定义对称的事前和事后窗口
        pre_start <- event$StartDate %m-% months(w)
        pre_end <- event$StartDate %m-% days(1)
        post_start <- event$EndDate %m+% days(1)
        post_end <- event$EndDate %m+% months(w)

        # 使用基础R的子集提取语法
        before_data <- pixel_data[pixel_data$date >= pre_start & pixel_data$date <= pre_end, ]
        during_data <- pixel_data[pixel_data$date >= floor_date(event$StartDate, "month") & pixel_data$date <= floor_date(event$EndDate, "month"), ]
        after_data <- pixel_data[pixel_data$date >= post_start & pixel_data$date <= post_end, ]

        # 数据质量控制: 确保每个窗口都有足够的数据
        if(nrow(before_data) < w/2 || nrow(during_data) == 0 || nrow(after_data) < w/2) {
          return(tibble(EventID = event$EventID, TimeWindow = w, Resistance = NA_real_, Resilience = NA_real_, Recovery = NA_real_))
        }

        # 计算指标
        ndvi_before_mean <- mean(before_data$NDVI_zscore, na.rm = TRUE)
        ndvi_during_min <- min(during_data$NDVI_zscore, na.rm = TRUE)
        ndvi_after_mean <- mean(after_data$NDVI_zscore, na.rm = TRUE)

        resistance <- ndvi_during_min / ndvi_before_mean
        resilience <- ndvi_after_mean / ndvi_before_mean

        if(abs(ndvi_before_mean - ndvi_during_min) > 0.01) {
          recovery <- (ndvi_after_mean - ndvi_during_min) / (ndvi_before_mean - ndvi_during_min)
        } else {
          recovery <- NA_real_
        }

        tibble(EventID = event$EventID, TimeWindow = w, Resistance = resistance, Resilience = resilience, Recovery = recovery)
      })
    })
  })) %>%
  # 移除原始的嵌套数据列 'data'
  select(-data) %>%
  # unnest() 会将我们新计算的 'metrics' 列（它也是一个列表）展开
  unnest(cols = c(metrics)) %>%
  ungroup() %>%
  # 过滤掉计算失败的行
  filter(!is.na(Resilience) & !is.na(Resistance) & !is.na(Recovery))

write_csv(resilience_metrics_long, "E:/WUR_Intern/RewildingProject_RawData/Resilience_metrics/resilience_metrics.csv")

# 这是提取数据的new logic
resilience_metrics_long_new <- ndvi_data_final %>%
  group_by(Pixel_ID, UnitName, AnalysisType) %>%
  nest() %>%
  mutate(metrics = map(data, function(pixel_data) {
    map_dfr(1:nrow(events_prepared), function(i) {
      event <- events_prepared[i, ]
      time_windows <- c(3, 6, 9, 12, 15, 18)
      map_dfr(time_windows, function(w) {

        pre_start <- event$StartDate %m-% months(w)
        pre_end <- event$StartDate %m-% days(1)
        post_start <- event$EndDate %m+% days(1)
        post_end <- event$EndDate %m+% months(w)

        before_data <- pixel_data[pixel_data$date >= pre_start & pixel_data$date <= pre_end, ]
        during_data <- pixel_data[pixel_data$date >= floor_date(event$StartDate, "month") & 
                                    pixel_data$date <= floor_date(event$EndDate, "month"), ]
        after_data <- pixel_data[pixel_data$date >= post_start & pixel_data$date <= post_end, ]

        if(nrow(before_data) < w/2 || nrow(during_data) == 0 || nrow(after_data) < w/2) {
          return(tibble(
            EventID = event$EventID, 
            TimeWindow = w,
            Resistance = NA_real_,
            Resilience = NA_real_,
            Recovery = NA_real_,
            Resistance_new = NA_real_,
            Recovery_new = NA_real_,
            Resilience_new = NA_real_
          ))
        }

        # =========================================================================
        # 第一部分: 计算您原有的三个指标 (基于Z-score) - 保持不变
        ndvi_before_mean <- mean(before_data$NDVI_zscore, na.rm = TRUE)
        ndvi_during_min  <- min(during_data$NDVI_zscore, na.rm = TRUE)
        ndvi_after_mean  <- mean(after_data$NDVI_zscore, na.rm = TRUE)

        resistance <- ndvi_during_min / ndvi_before_mean
        resilience <- ndvi_after_mean / ndvi_before_mean
        recovery <- NA_real_
        if(abs(ndvi_before_mean - ndvi_during_min) > 0.01) {
          recovery <- (ndvi_after_mean - ndvi_during_min) / (ndvi_before_mean - ndvi_during_min)
        }

        # =========================================================================
        # 第二部分: 计算新的指标 (基于anomaly)
        # 1. 定义基准：正常状态 = anomaly为0
        baseline_normal <- 0

        # 2. 计算事件期间的影响（考虑事件类型）
        if(event$EventType == "Drought") {
          # 干旱：取最小值（最负的anomaly）
          anomaly_impact <- min(during_data$NDVI_anomaly, na.rm = TRUE)
        } else {
          # 洪水：可能导致NDVI上升或下降，取绝对偏离最大的值
          anomaly_max <- max(during_data$NDVI_anomaly, na.rm = TRUE)
          anomaly_min <- min(during_data$NDVI_anomaly, na.rm = TRUE)
          anomaly_impact <- ifelse(abs(anomaly_max) > abs(anomaly_min), anomaly_max, anomaly_min)
        }

        # 3. 计算恢复期的平均状态
        anomaly_recovered <- mean(after_data$NDVI_anomaly, na.rm = TRUE)

        # 4. 计算Resistance_new（类似Isbell的思想）
        # Resistance = 正常状态维持能力 / 扰动强度
        resistance_new <- anomaly_impact - baseline_normal

        # 5. 计算Recovery_new（已经恢复的比例）
        recovery_new <- NA_real_
        if(abs(anomaly_impact - baseline_normal) > 1e-6) {
            recovery_new <- (anomaly_recovered - anomaly_impact) / (baseline_normal - anomaly_impact)
          }

        # 6. 计算Resilience_new（可选，基于Isbell的定义）
        # Resilience = 初始偏离 / 剩余偏离
        resilience_new <- NA_real_
        if(abs(anomaly_recovered - baseline_normal) > 1e-6) {
          resilience_new <- abs(anomaly_impact - baseline_normal) / abs(anomaly_recovered - baseline_normal)
          # 值>1表示恢复中，值<1表示进一步偏离
        }

        # =========================================================================
        # 返回所有指标
        tibble(
          EventID = event$EventID,
          TimeWindow = w,
          # 原有指标
          Resistance = resistance,
          Resilience = resilience,
          Recovery = recovery,
          # 新增指标
          Resistance_new = resistance_new,
          Recovery_new = recovery_new,
          Resilience_new = resilience_new
        )
      })
    })
  })) %>%
  select(-data) %>%
  unnest(cols = c(metrics)) %>%
  ungroup() %>%
  filter(!is.na(Resilience) & !is.na(Resistance) & !is.na(Recovery) & !is.na(Resilience_new) & !is.na(Resistance_new) & !is.na(Recovery_new))

write_csv(resilience_metrics_long_new, "E:/WUR_Intern/RewildingProject_RawData/Resilience_metrics/resilience_metrics_new_1.csv")










# --- 步骤 4: 分离数据并准备建模 ---
cat("--- 正在分离数据用于两个独立的模型 ---\n")

# 合并事件信息
full_modeling_df <- resilience_metrics_long %>%
  left_join(select(events_prepared, EventID, EventType, PeakSeverity_SPEI12, SRI_3), by = "EventID")

# ==================== 新增: 按事件类型拆分数据 ====================
drought_data <- full_modeling_df %>% filter(EventType == "Drought")
flood_data <- full_modeling_df %>% filter(EventType == "Flood")

# --- 准备模型一的数据 (地块类型比较) ---
class_drought_df <- drought_data %>%
  filter(AnalysisType == "Regional Class") %>%
  mutate(UnitName = factor(UnitName), EventID = factor(EventID), Pixel_ID = factor(Pixel_ID))

class_flood_df <- flood_data %>%
  filter(AnalysisType == "Regional Class") %>%
  mutate(UnitName = factor(UnitName), EventID = factor(EventID), Pixel_ID = factor(Pixel_ID))

# --- 准备模型二的数据 (再野化年限分析) ---
site_info <- tibble(
  UnitName = c("MW", "BB", "OW", "EW"),
  rewilding_start_year = c(1993, 2002, 2006, 2008)
)

# 为干旱数据添加再野化年限
site_drought_df <- drought_data %>%
  filter(AnalysisType == "Specific Site") %>%
  left_join(site_info, by = "UnitName") %>%
  left_join(select(events_prepared, EventID, StartDate), by = "EventID") %>%
  mutate(EventYear = year(StartDate), RewildingAge = EventYear - rewilding_start_year) %>%
  filter(RewildingAge >= 0) %>%
  mutate(RewildingAge_scaled = scale(RewildingAge)[,1], UnitName = factor(UnitName), EventID = factor(EventID), Pixel_ID = factor(Pixel_ID))

# 为洪水数据添加再野化年限
site_flood_df <- flood_data %>%
  filter(AnalysisType == "Specific Site") %>%
  left_join(site_info, by = "UnitName") %>%
  left_join(select(events_prepared, EventID, StartDate), by = "EventID") %>%
  mutate(EventYear = year(StartDate), RewildingAge = EventYear - rewilding_start_year) %>%
  filter(RewildingAge >= 0) %>%
  mutate(RewildingAge_scaled = scale(RewildingAge)[,1], UnitName = factor(UnitName), EventID = factor(EventID), Pixel_ID = factor(Pixel_ID))


# --- 步骤 5: 运行并解读模型 (以Resilience_12m为例) ---

# ==================== Model 1: 地块类型比较 ====================
cat("\n\n--- 正在运行模型一: 地块类型对12个月恢复力的影响 ---\n")

# --- 针对干旱事件 ---
cat("\n--- 分析干旱事件 ---\n")
class_drought_12m <- class_drought_df %>% filter(TimeWindow == 12)
cat(paste("干旱模型: 将使用全部", nrow(class_drought_12m), "行数据进行建模...\n"))
# 解释: 模型公式直接使用原始的SPEI值，并使用全部数据
lme_drought_class <- lme(Resilience ~ UnitName + PeakSeverity_SPEI12, random = ~ 1 | EventID / Pixel_ID, data = class_drought_12m, na.action = na.exclude)
print(summary(lme_drought_class))
print(anova(lme_drought_class, type = "marginal"))


# 干旱交互效应模型
lme_drought_class_interaction <- lme(Resilience ~ UnitName * PeakSeverity_SPEI12,
                                         random = ~ 1 | EventID / Pixel_ID,
                                         data = class_drought_12m,
                                         na.action = na.exclude)
  
# 比较两个模型
cat("\n--- 模型比较（是否需要交互项）---\n")
print(anova(lme_drought_class, lme_drought_class_interaction))
print(summary(final_drought_class_model))
print(anova(final_drought_class_model, type = "marginal"))




# --- 针对洪水事件 ---
cat("\n--- 分析洪水事件 ---\n")
class_flood_12m <- class_flood_df %>% filter(TimeWindow == 12)
cat(paste("洪水模型: 将使用全部", nrow(class_flood_12m), "行数据进行建模...\n"))
# 解释: 模型公式直接使用原始的SRI值，并使用全部数据
lme_flood_class <- lme(Resilience ~ UnitName + SRI_3, random = ~ 1 | EventID / Pixel_ID, data = class_flood_12m, na.action = na.exclude)
print(summary(lme_flood_class))
print(anova(lme_flood_class, type = "marginal"))

# 洪水交互效应模型
lme_flood_class_interaction <- lme(Resilience ~ UnitName * SRI_3,
                                       random = ~ 1 | EventID / Pixel_ID,
                                       data = class_flood_12m,
                                       na.action = na.exclude)
  
if(anova(lme_flood_class, lme_flood_class_interaction)$`p-value`[2] < 0.05) {
    final_flood_class_model <- lme_flood_class_interaction
      } else {
          final_flood_class_model <- lme_flood_class
            }
  
print(summary(final_flood_class_model))
print(anova(final_flood_class_model, type = "marginal"))

if(anova(final_flood_class_model, type = "marginal")["UnitName", "p-value"] < 0.05) {
    emm_flood <- emmeans(final_flood_class_model, ~ UnitName)
      print(pairs(emm_flood, adjust = "tukey"))
      }



# ==================== Model 2: 再野化年限分析 ====================
cat("\n\n--- 正在运行模型二: 再野化年限对12个月恢复力的影响 ---\n")

# --- 针对干旱事件 ---
cat("\n--- 分析干旱事件 ---\n")
site_drought_12m <- site_drought_df %>% filter(TimeWindow == 12)
cat(paste("干旱模型: 将使用全部", nrow(site_drought_12m), "行数据进行建模...\n"))
# 解释: 模型公式直接使用原始的SPEI值，并使用全部数据
lme_drought_site <- lme(Resilience ~ RewildingAge_scaled + PeakSeverity_SPEI12, random = ~ 1 | EventID / Pixel_ID, data = site_drought_12m, na.action = na.exclude)
print(summary(lme_drought_site))
print(anova(lme_drought_site, type = "marginal"))

# --- 针对洪水事件 ---
cat("\n--- 分析洪水事件 ---\n")
site_flood_12m <- site_flood_df %>% filter(TimeWindow == 12)
cat(paste("洪水模型: 将使用全部", nrow(site_flood_12m), "行数据进行建模...\n"))
# 解释: 模型公式直接使用原始的SRI值，并使用全部数据
lme_flood_site <- lme(Resilience ~ RewildingAge_scaled + SRI_3, random = ~ 1 | EventID / Pixel_ID, data = site_flood_12m, na.action = na.exclude)
print(summary(lme_flood_site))
print(anova(lme_flood_site, type = "marginal"))


# # ==================== 步骤 6: 自动化建模与显著性筛选 ====================
# 
# # 步骤 5.1: 定义一个用于自动化建模和检验的函数
# run_lme_and_report <- function(data, response_var, predictor_var, severity_var, model_description) {
#   
#   # 检查数据是否为空
#   if(nrow(data) < 100) { # 如果数据太少，直接跳过
#     return(NULL)
#   }
#   
#   # 构建模型公式
#   # 我们需要将字符串变量转换为公式
#   formula_str <- paste(response_var, "~", predictor_var, "+", severity_var)
#   model_formula <- as.formula(formula_str)
#   
#   # 运行LMM模型
#   # 使用tryCatch来捕获可能发生的错误（例如模型不收敛）
#   lme_model <- tryCatch({
#     lme(fixed = model_formula,
#         random = ~ 1 | EventID / Pixel_ID,
#         data = data,
#         na.action = na.exclude)
#   }, error = function(e) {
#     cat(paste("\n--- 警告:", model_description, "模型运行失败 ---\n"))
#     cat(paste("错误信息:", e$message, "\n"))
#     return(NULL)
#   })
#   
#   # 如果模型运行失败，则退出函数
#   if (is.null(lme_model)) return(NULL)
#   
#   # 运行ANOVA检验
#   anova_results <- anova(lme_model, type = "marginal")
#   
#   # 提取核心预测变量的p值
#   # 我们需要从anova表的行名中找到我们的预测变量
#   p_value <- anova_results[predictor_var, "p-value"]
#   
#   # 检查p值是否显著
#   if (p_value < 0.05) {
#     cat("\n=====================================================================")
#     cat(paste("\n### 发现显著结果:", model_description, "###"))
#     cat("\n=====================================================================\n")
#     
#     cat("\n--- ANOVA 检验结果 (p-value < 0.05) ---\n")
#     print(anova_results)
#     
#     cat("\n--- 模型详细摘要 (Summary) ---\n")
#     print(summary(lme_model))
#     
#     cat("\n\n")
#   }
# }
# 
# # 步骤 5.2: 定义要循环的参数
# response_vars <- c("Resistance", "Resilience", "Recovery")
# time_windows <- c(3, 6, 9, 12, 15, 18)
# 
# # 步骤 5.3: 开始循环，自动运行所有模型
# cat("\n\n--- 开始自动化建模，将只报告显著结果 (p < 0.05) ---\n")
# 
# for (resp_var in response_vars) {
#   for (w in time_windows) {
#     
#     # --- Model 1: 地块类型比较 ---
#     
#     # 针对干旱事件
#     model_data_drought_class <- class_drought_df %>% filter(TimeWindow == w)
#     run_lme_and_report(
#       data = model_data_drought_class,
#       response_var = resp_var,
#       predictor_var = "UnitName",
#       severity_var = "PeakSeverity_SPEI12",
#       model_description = paste("地块类型 vs 干旱", resp_var, "@", w, "个月")
#     )
#     
#     # 针对洪水事件
#     model_data_flood_class <- class_flood_df %>% filter(TimeWindow == w)
#     run_lme_and_report(
#       data = model_data_flood_class,
#       response_var = resp_var,
#       predictor_var = "UnitName",
#       severity_var = "SRI_3",
#       model_description = paste("地块类型 vs 洪水", resp_var, "@", w, "个月")
#     )
#     
#     # --- Model 2: 再野化年限分析 ---
#     
#     # 针对干旱事件
#     model_data_drought_site <- site_drought_df %>% filter(TimeWindow == w)
#     run_lme_and_report(
#       data = model_data_drought_site,
#       response_var = resp_var,
#       predictor_var = "RewildingAge_scaled",
#       severity_var = "PeakSeverity_SPEI12",
#       model_description = paste("再野化年限 vs 干旱", resp_var, "@", w, "个月")
#     )
#     
#     # 针对洪水事件
#     model_data_flood_site <- site_flood_df %>% filter(TimeWindow == w)
#     run_lme_and_report(
#       data = model_data_flood_site,
#       response_var = resp_var,
#       predictor_var = "RewildingAge_scaled",
#       severity_var = "SRI_3",
#       model_description = paste("再野化年限 vs 洪水", resp_var, "@", w, "个月")
#     )
#   }
# }
# 
# cat("\n--- 自动化建模分析完成 ---\n")


# ==================== 步骤 6: 自动化建模与显著性筛选（方案3版本）====================

# ==================== 步骤 6: 自动化建模与显著性筛选（完整版本）====================

# 创建输出文件夹和文件名
output_dir <- "E:/WUR_Intern/RewildingProject_RawData/Model_Outputs/"
dir.create(output_dir, showWarnings = FALSE)
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path(output_dir, paste0("Model_Results_", timestamp, ".txt"))
summary_file <- file.path(output_dir, paste0("Model_Summary_", timestamp, ".csv"))

# 开始记录输出
sink(output_file, split = TRUE)  # split=TRUE 同时输出到控制台和文件
cat("=====================================\n")
cat("自动化建模分析结果\n")
cat(paste("分析时间:", Sys.time(), "\n"))
cat(paste("数据行数:", nrow(full_modeling_df), "\n"))
cat("=====================================\n\n")

# 创建诊断汇总表（全局变量）
diagnostic_summary <- tibble()

# 步骤 6.1: 定义增强版的自动化建模和检验函数
run_lme_and_report <- function(data, response_var, predictor_var, severity_var, model_description, time_window) {
  
  # 检查数据是否为空
  if(nrow(data) < 100) { 
    return(NULL)
  }
  
  # 构建模型公式
  formula_str <- paste(response_var, "~", predictor_var, "+", severity_var)
  model_formula <- as.formula(formula_str)
  
  # 添加交互效应公式
  formula_interaction_str <- paste(response_var, "~", predictor_var, "*", severity_var)
  model_formula_interaction <- as.formula(formula_interaction_str)
  
  # 运行基础LMM模型
  lme_model <- tryCatch({
    lme(fixed = model_formula,
        random = ~ 1 | EventID / Pixel_ID,
        data = data,
        na.action = na.exclude)
  }, error = function(e) {
    cat(paste("\n--- 警告:", model_description, "基础模型运行失败 ---\n"))
    cat(paste("错误信息:", e$message, "\n"))
    return(NULL)
  })
  
  if (is.null(lme_model)) return(NULL)
  
  # 尝试运行交互模型
  lme_model_interaction <- tryCatch({
    lme(fixed = model_formula_interaction,
        random = ~ 1 | EventID / Pixel_ID,
        data = data,
        na.action = na.exclude)
  }, error = function(e) {
    return(NULL)  # 如果交互模型失败，使用基础模型
  })
  
  # 选择最终模型
  model_type <- ""
  if (!is.null(lme_model_interaction)) {
    anova_comparison <- tryCatch({
      anova(lme_model, lme_model_interaction)
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(anova_comparison) && anova_comparison$`p-value`[2] < 0.05) {
      final_model <- lme_model_interaction
      model_type <- "交互效应"
    } else {
      final_model <- lme_model
      model_type <- "主效应"
    }
  } else {
    final_model <- lme_model
    model_type <- "主效应"
  }
  
  # 运行ANOVA检验
  anova_results <- anova(final_model, type = "marginal")
  
  # 提取核心预测变量的p值
  p_value <- anova_results[predictor_var, "p-value"]
  
  # 收集诊断信息（无论是否显著）
  residuals_norm <- residuals(final_model, type = "normalized")
  sw_test <- shapiro.test(sample(residuals_norm, min(5000, length(residuals_norm))))
  
  # 计算R²（边际）
  var_fixed <- var(as.numeric(fitted(final_model)))
  var_resid <- sigma(final_model)^2
  marginal_r2 <- var_fixed / (var_fixed + var_resid)
  
  # 添加到诊断汇总表
  diagnostic_summary <<- bind_rows(
    diagnostic_summary,
    tibble(
      Model = model_description,
      TimeWindow = time_window,
      ModelType = model_type,
      P_value = round(p_value, 4),
      Significant = ifelse(p_value < 0.05, "是", "否"),
      Normality_p = round(sw_test$p.value, 4),
      Residual_SD = round(sd(residuals_norm), 4),
      Marginal_R2 = round(marginal_r2, 3),
      AIC = round(AIC(final_model), 1),
      N_obs = nrow(data),
      Warning = case_when(
        sw_test$p.value < 0.01 ~ "残差非正态***",
        sw_test$p.value < 0.05 ~ "残差非正态*",
        TRUE ~ "正常"
      )
    )
  )
  
  # 只显示显著结果的详细信息
  if (p_value < 0.05) {
    cat("\n=====================================================================")
    cat(paste("\n### 发现显著结果:", model_description, "(", model_type, ") ###"))
    cat("\n=====================================================================\n")
    
    cat("\n--- ANOVA 检验结果 (p-value < 0.05) ---\n")
    print(anova_results)
    
    cat("\n--- 模型详细摘要 (Summary) ---\n")
    print(summary(final_model))
    
    # 添加简要诊断信息
    cat("\n--- 快速诊断 ---\n")
    cat(paste("残差正态性 (Shapiro-Wilk p):", round(sw_test$p.value, 4),
              ifelse(sw_test$p.value < 0.05, "⚠️", "✓"), "\n"))
    cat(paste("边际R²:", round(marginal_r2, 3), "\n"))
    cat(paste("AIC:", round(AIC(final_model), 1), "\n"))
    
    # 多重比较（如果是土地类型）
    if (predictor_var == "UnitName") {
      cat("\n--- 多重比较 (Tukey HSD) ---\n")
      library(emmeans)
      emm <- emmeans(final_model, as.formula(paste("~", predictor_var)))
      print(pairs(emm, adjust = "tukey"))
    }
    
    cat("\n\n")
  }
}

# 步骤 6.2: 定义要循环的参数
response_vars <- c("Resistance", "Resilience", "Recovery")
time_windows <- c(3, 6, 9, 12, 15, 18)

# 步骤 6.3: 开始循环，自动运行所有模型
cat("\n\n--- 开始自动化建模，将只报告显著结果 (p < 0.05) ---\n")

# 创建进度追踪
total_models <- length(response_vars) * length(time_windows) * 4  # 4个模型类型
current_model <- 0

for (resp_var in response_vars) {
  for (w in time_windows) {
    
    # 显示进度
    current_model <- current_model + 4
    cat(paste("\n进度:", current_model, "/", total_models, "模型\n"))
    
    # --- Model 1: 地块类型比较 ---
    
    # 针对干旱事件
    model_data_drought_class <- class_drought_df %>% filter(TimeWindow == w)
    run_lme_and_report(
      data = model_data_drought_class,
      response_var = resp_var,
      predictor_var = "UnitName",
      severity_var = "PeakSeverity_SPEI12",
      model_description = paste("地块类型 vs 干旱", resp_var, "@", w, "个月"),
      time_window = w
    )
    
    # 针对洪水事件
    model_data_flood_class <- class_flood_df %>% filter(TimeWindow == w)
    run_lme_and_report(
      data = model_data_flood_class,
      response_var = resp_var,
      predictor_var = "UnitName",
      severity_var = "SRI_3",
      model_description = paste("地块类型 vs 洪水", resp_var, "@", w, "个月"),
      time_window = w
    )
    
    # --- Model 2: 再野化年限分析 ---
    
    # 针对干旱事件
    model_data_drought_site <- site_drought_df %>% filter(TimeWindow == w)
    run_lme_and_report(
      data = model_data_drought_site,
      response_var = resp_var,
      predictor_var = "RewildingAge_scaled",
      severity_var = "PeakSeverity_SPEI12",
      model_description = paste("再野化年限 vs 干旱", resp_var, "@", w, "个月"),
      time_window = w
    )
    
    # 针对洪水事件
    model_data_flood_site <- site_flood_df %>% filter(TimeWindow == w)
    run_lme_and_report(
      data = model_data_flood_site,
      response_var = resp_var,
      predictor_var = "RewildingAge_scaled",
      severity_var = "SRI_3",
      model_description = paste("再野化年限 vs 洪水", resp_var, "@", w, "个月"),
      time_window = w
    )
  }
}

# 步骤 6.4: 显示诊断汇总表
cat("\n\n=====================================================================")
cat("\n### 所有模型诊断汇总表 ###")
cat("\n=====================================================================\n")

# 显示有问题的模型
problem_models <- diagnostic_summary %>%
  filter(Warning != "正常" | Significant == "是") %>%
  arrange(desc(Significant), Warning, TimeWindow)

if(nrow(problem_models) > 0) {
  cat("\n--- 需要注意的模型 ---\n")
  print(problem_models, n = Inf)
}

# 显示所有显著模型的汇总
cat("\n--- 所有显著结果汇总 ---\n")
significant_models <- diagnostic_summary %>%
  filter(Significant == "是") %>%
  arrange(TimeWindow, Model) %>%
  select(Model, TimeWindow, ModelType, P_value, Marginal_R2, Warning)

print(significant_models, n = Inf)

# 按时间窗口统计显著结果数量
cat("\n--- 按时间窗口统计 ---\n")
window_summary <- diagnostic_summary %>%
  group_by(TimeWindow) %>%
  summarise(
    总模型数 = n(),
    显著模型数 = sum(Significant == "是"),
    显著比例 = round(mean(Significant == "是"), 2),
    平均R2 = round(mean(Marginal_R2[Significant == "是"], na.rm = TRUE), 3)
  )

print(window_summary)

# 按响应变量统计
cat("\n--- 按响应变量统计 ---\n")
response_summary <- diagnostic_summary %>%
  mutate(ResponseVar = str_extract(Model, "Resistance|Resilience|Recovery")) %>%
  group_by(ResponseVar) %>%
  summarise(
    总模型数 = n(),
    显著模型数 = sum(Significant == "是"),
    显著比例 = round(mean(Significant == "是"), 2),
    最高R2 = round(max(Marginal_R2[Significant == "是"], na.rm = TRUE), 3)
  )

print(response_summary)

cat("\n--- 自动化建模分析完成 ---\n")

# 保存诊断汇总表到CSV
write_csv(diagnostic_summary, summary_file)
cat(paste("\n诊断汇总表已保存到:", summary_file, "\n"))

# 关闭输出记录
cat(paste("\n\n分析结果已保存到:", output_file, "\n"))
sink()  # 停止记录

# 额外：保存显著结果的详细信息
if(nrow(significant_models) > 0) {
  significant_file <- file.path(output_dir, paste0("Significant_Results_", timestamp, ".csv"))
  write_csv(significant_models, significant_file)
  cat(paste("显著结果已单独保存到:", significant_file, "\n"))
}

# 创建一个简单的报告摘要
report_summary <- file.path(output_dir, paste0("Report_Summary_", timestamp, ".txt"))
writeLines(c(
  "=== 建模分析快速摘要 ===",
  paste("分析完成时间:", Sys.time()),
  paste("总共运行模型数:", nrow(diagnostic_summary)),
  paste("显著结果数:", sum(diagnostic_summary$Significant == "是")),
  "",
  "主要发现：",
  paste("- 最显著的效应出现在", 
        significant_models$Model[which.min(significant_models$P_value)],
        "(p =", min(significant_models$P_value), ")"),
  paste("- 最高的解释力(R²):", max(significant_models$Marginal_R2, na.rm = TRUE)),
  paste("- 最佳时间窗口可能是:", 
        window_summary$TimeWindow[which.max(window_summary$显著比例)], "个月"),
  "",
  "需要注意的模型数:", sum(diagnostic_summary$Warning != "正常"),
  "详细结果请查看主输出文件。"
), report_summary)

cat(paste("\n快速摘要已保存到:", report_summary, "\n"))

# 创建一个函数来提取特定模型的详细结果
extract_model_details <- function(model_name_pattern, output_file) {
  lines <- readLines(output_file)
  start_indices <- grep(model_name_pattern, lines)
  
  if(length(start_indices) == 0) {
    cat("未找到匹配的模型\n")
    return(NULL)
  }
  
  # 找到下一个"===="分隔符
  separator_indices <- grep("=====", lines)
  
  for(start in start_indices) {
    end <- min(separator_indices[separator_indices > start], length(lines))
    cat(paste(lines[start:end], collapse = "\n"))
    cat("\n\n")
  }
}

# 使用示例：
extract_model_details("地块类型 vs 干旱 Recovery @ 3 个月", output_file)



