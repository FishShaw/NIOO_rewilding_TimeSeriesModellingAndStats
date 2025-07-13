library(tidyverse)
library(lubridate)
library(nlme)
# library(emmeans) 
# library(multcomp)
# library(multcompView)
# library(ggpubr)
library(ggplot2)


metrics_data_path <- "E:/WUR_Intern/RewildingProject_RawData/Resilience_metrics/resilience_metrics_new_1.csv"
events_path <- "E:/WUR_Intern/RewildingProject_RawData/ExtremeEvents_selected/FinalExtremeEvents.csv"

resilience_metrics_long <- read_csv(metrics_data_path)
events_df <- read_csv(events_path)

# --- 步骤 2: 创建生态大类分组 ---
events_prepared <- events_df %>%
  mutate(
    EventID = row_number(),
    StartDate = ymd(StartDate),
    EndDate = ymd(EndDate)
  )

full_modeling_df <- resilience_metrics_long %>%
  left_join(dplyr::select(events_prepared, EventID, EventType, PeakSeverity_SPEI12, SRI_3), by = "EventID") %>%
mutate(
  EcologicalGroup = case_when(
    AnalysisType == "Specific Site" ~ "Specific Site",
    str_detect(UnitName, "Early Rewilded") ~ "Early Started Rewilded",
    str_detect(UnitName, "Late Rewilded") ~ "Late Started Rewilded",
    str_detect(UnitName, "Stable Natural") ~ "Stable Natural",
    str_detect(UnitName, "Stable Agriculture") ~ "Stable Agriculture",
    str_detect(UnitName, "Stable Original Forest") ~ "Stable Natural", 
    TRUE ~ "Other"
  )
) %>%
  filter(EcologicalGroup != "Other") %>%
  mutate(
    EventType = factor(EventType),
    EventID = factor(EventID),
    Pixel_ID = factor(Pixel_ID)
  )

# --- 准备模型一的数据 (生态大类比较) ---
class_modelling_base_df <- full_modeling_df %>%
  filter(AnalysisType == "Regional Class") %>%
  mutate(
    EcologicalGroup = factor(EcologicalGroup, 
                             levels = c("Stable Natural", "Stable Agriculture", "Early Started Rewilded", "Late Started Rewilded"),
                             ordered = FALSE)
  )

# --- 准备模型二的数据 (再野化年限分析) ---
site_modeling_base_df <- full_modeling_df %>%
  filter(AnalysisType == "Specific Site") %>%
  mutate(
    RewildingRank = case_when(
      UnitName == "MW" ~ "1_Earliest Started", # 1993
      UnitName == "BB" ~ "2_Mid-Early Started", # 2002
      UnitName == "OW" ~ "3_Mid-Late Started",  # 2006
      UnitName == "EW" ~ "4_Latest Started"    # 2008
    ),
    RewildingRank = factor(RewildingRank, 
                           levels = c("1_Earliest Started", "2_Mid-Early Started", "3_Mid-Late Started", "4_Latest Started"),
                           ordered = FALSE)
  )


# --- 步骤 3: 自动化建模与显著性筛选 ---
# 解释: 我们将针对所有指标和时间窗口，自动化地运行模型、比较模型、检验显著性并报告结果。

# 创建输出文件夹和文件名
output_dir <- "E:/WUR_Intern/RewildingProject_RawData/Model_Outputs/" 
dir.create(output_dir, showWarnings = FALSE)
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path(output_dir, paste0("Model_Results_", timestamp, ".txt"))
summary_file <- file.path(output_dir, paste0("Model_Summary_", timestamp, ".csv"))

# 开始记录输出
sink(output_file, split = TRUE)
cat("=====================================\n")
cat("自动化建模分析结果\n")
cat(paste("分析时间:", Sys.time(), "\n"))
cat(paste("数据行数:", nrow(full_modeling_df), "\n"))
cat("=====================================\n\n")

# 创建一个空的诊断汇总表
diagnostic_summary <- tibble()

# 定义增强版的自动化建模和检验函数
run_lme_and_report <- function(data, response_var, predictor_var, severity_var, model_description, time_window) {
  
  if(nrow(data) < 100) return(NULL)
  
  # 构建主效应和交互效应的模型公式
  formula_main <- as.formula(paste(response_var, "~", predictor_var, "+", severity_var))
  formula_interaction <- as.formula(paste(response_var, "~", predictor_var, "*", severity_var))
  
  # 运行基础模型
  lme_main <- tryCatch({ lme(fixed = formula_main, random = ~ 1 | EventID / Pixel_ID, data = data, na.action = na.exclude) }, error = function(e) NULL)
  if (is.null(lme_main)) {
    cat(paste("\n--- 警告:", model_description, "主效应模型运行失败 ---\n"))
    return(NULL)
  }
  
  # 尝试运行交互模型
  lme_interaction <- tryCatch({ lme(fixed = formula_interaction, random = ~ 1 | EventID / Pixel_ID, data = data, na.action = na.exclude) }, error = function(e) NULL)
  
  # 通过ANOVA比较，选择最优模型
  model_type <- "主效应"
  final_model <- lme_main
  if (!is.null(lme_interaction)) {
    anova_comp <- anova(lme_main, lme_interaction)
    if (anova_comp$`p-value`[2] < 0.05) {
      final_model <- lme_interaction
      model_type <- "交互效应"
    }
  }
  
  # 对最终模型进行ANOVA检验
  anova_results <- anova(final_model, type = "marginal")
  p_value <- anova_results[predictor_var, "p-value"]
  
  # 收集诊断信息
  residuals_norm <- residuals(final_model, type = "normalized")
  sw_test <- shapiro.test(sample(residuals_norm, min(5000, length(residuals_norm))))
  var_fixed <- var(as.numeric(fitted(final_model)))
  var_resid <- sigma(final_model)^2
  marginal_r2 <- var_fixed / (var_fixed + var_resid)
  
  diagnostic_summary <<- bind_rows(diagnostic_summary, tibble(Model = model_description, TimeWindow = time_window, ModelType = model_type, P_value = p_value, Significant = ifelse(p_value < 0.05, "是", "否"), Normality_p = sw_test$p.value, Marginal_R2 = marginal_r2, AIC = AIC(final_model), N_obs = nrow(data)))
  
  # 只报告显著结果
  if (p_value < 0.05) {
    cat("\n=====================================================================")
    cat(paste("\n### 发现显著结果:", model_description, "(", model_type, ") ###"))
    cat("\n=====================================================================\n")
    cat("\n--- ANOVA 检验结果 (p-value < 0.05) ---\n"); print(anova_results)
    cat("\n--- 模型详细摘要 (Summary) ---\n"); print(summary(final_model))
    cat("\n--- 快速诊断 ---\n"); cat(paste("残差正态性 (Shapiro-Wilk p):", round(sw_test$p.value, 4), ifelse(sw_test$p.value < 0.05, "⚠️", "✓"), "\n")); cat(paste("边际R²:", round(marginal_r2, 3), "\n")); cat(paste("AIC:", round(AIC(final_model), 1), "\n"))
    # 更新：现在对EcologicalGroup和RewildingRank都进行多重比较
    if (predictor_var %in% c("EcologicalGroup", "RewildingRank")) {
      cat("\n--- 多重比较 (Tukey HSD) ---\n")
      emm <- emmeans(final_model, as.formula(paste("~", predictor_var)))
      print(summary(pairs(emm, adjust = "tukey")))
    }
    cat("\n\n")
  }
}

# 定义要循环的参数
response_vars <- c("Resistance", "Resilience", "Recovery","Resistance_new", "Resilience_new", "Recovery_new")
time_windows <- c(3, 6, 9, 12, 15, 18)

# 开始循环
cat("\n\n--- 开始自动化建模，将只报告显著结果 (p < 0.05) ---\n")
total_models <- length(response_vars) * length(time_windows) * 4
current_model <- 0

for (resp_var in response_vars) {
  for (w in time_windows) {
    current_model <- current_model + 4
    cat(paste("\n进度:", current_model, "/", total_models, "模型\n"))
    
    # Model 1: 生态大类比较
    run_lme_and_report(data = class_modelling_base_df %>% filter(EventType == "Drought", TimeWindow == w), response_var = resp_var, predictor_var = "EcologicalGroup", severity_var = "PeakSeverity_SPEI12", model_description = paste("生态大类 vs 干旱", resp_var, "@", w, "个月"), time_window = w)
    run_lme_and_report(data = class_modelling_base_df %>% filter(EventType == "Flood", TimeWindow == w), response_var = resp_var, predictor_var = "EcologicalGroup", severity_var = "SRI_3", model_description = paste("生态大类 vs 洪水", resp_var, "@", w, "个月"), time_window = w)
    
    # Model 2: 再野化站点排序分析
    run_lme_and_report(data = site_modeling_base_df %>% filter(EventType == "Drought", TimeWindow == w), response_var = resp_var, predictor_var = "RewildingRank", severity_var = "PeakSeverity_SPEI12", model_description = paste("再野化站点排序 vs 干旱", resp_var, "@", w, "个月"), time_window = w)
    run_lme_and_report(data = site_modeling_base_df %>% filter(EventType == "Flood", TimeWindow == w), response_var = resp_var, predictor_var = "RewildingRank", severity_var = "SRI_3", model_description = paste("再野化站点排序 vs 洪水", resp_var, "@", w, "个月"), time_window = w)
  }
}

# 步骤 4: 显示和保存最终报告
cat("\n\n=====================================================================")
cat("\n### 所有模型诊断汇总表 ###")
cat("\n=====================================================================\n")

# 显示所有显著模型的汇总
significant_models <- diagnostic_summary %>% filter(Significant == "是") %>% arrange(TimeWindow, Model) %>% dplyr::select(Model, TimeWindow, ModelType, P_value, Marginal_R2)
cat("\n--- 所有显著结果汇总 ---\n"); print(significant_models, n = Inf)

# 按时间窗口统计
window_summary <- diagnostic_summary %>% group_by(TimeWindow) %>% summarise(总模型数 = n(), 显著模型数 = sum(Significant == "是"), 显著比例 = round(mean(Significant == "是"), 2))
cat("\n--- 按时间窗口统计 ---\n"); print(window_summary)

# 按响应变量统计
response_summary <- diagnostic_summary %>% mutate(ResponseVar = str_extract(Model, "Resistance|Resilience|Recovery")) %>% group_by(ResponseVar) %>% summarise(总模型数 = n(), 显著模型数 = sum(Significant == "是"), 显著比例 = round(mean(Significant == "是"), 2))
cat("\n--- 按响应变量统计 ---\n"); print(response_summary)

cat("\n--- 自动化建模分析完成 ---\n")
write_csv(diagnostic_summary, summary_file)
cat(paste("\n诊断汇总表已保存到:", summary_file, "\n"))
sink()
cat(paste("\n\n分析结果已保存到:", output_file, "\n"))























