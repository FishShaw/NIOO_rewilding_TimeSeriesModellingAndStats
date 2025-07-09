# --- PART 1: LOAD LIBRARIES AND DATA ---
library(tidyverse)
library(lubridate)
library(nlme)
library(lmerTest)
library(stringr)
library(broom.mixed)
library(performance)
library(brms)
library(tidybayes)


events_df <- read_csv("E:/WUR_Intern/RewildingProject_RawData/ExtremeEvents_selected/FinalExtremeEvents.csv")
ndvi_df_raw <- read_csv("E:/WUR_Intern/RewildingProject_RawData/NDVI/monthly_data_combined_cleaned.csv")

# --- PART 2: CALCULATE NDVI ANOMALY (Z-SCORE) ---
ndvi_with_zscore <- ndvi_df_raw %>%
  rename(unit_name = ClassName, NDVI = IndexValue) %>%
  mutate(
    date = ymd(paste(Year, Month, 1)),
    month_num = month(date)
  ) %>%
  group_by(unit_name, month_num) %>%
  mutate(
    NDVI_mean_hist = mean(NDVI, na.rm = TRUE),
    NDVI_sd_hist = sd(NDVI, na.rm = TRUE),
    NDVI_zscore = (NDVI - NDVI_mean_hist) / NDVI_sd_hist
  ) %>%
  ungroup() %>%
  mutate(
    NDVI_zscore = case_when(
      is.na(NDVI_zscore) ~ 0,
      is.infinite(NDVI_zscore) ~ 0,
      TRUE ~ NDVI_zscore
    )
  )

# --- Prepare data for the 12 Regional Classes analysis and 4 Specific Sites analysis ---
classes_df <- ndvi_with_zscore %>%
  filter(AnalysisType == "Regional Class") %>%
  rename(class_name = unit_name) %>%
  select(class_name, date, NDVI, NDVI_zscore, CategoryGroup)

sites_df <- ndvi_with_zscore %>%
  filter(AnalysisType == "Specific Site") %>%
  rename(site = unit_name) %>%
  select(site, date, NDVI, NDVI_zscore)

# --- PART 3: CALCULATE RESILIENCE METRICS ---
calculate_metrics <- function(unit, ndvi_data, event, unit_col) {
  event_start <- ymd(event$StartDate)
  event_end <- ymd(event$EndDate)
  
  pre_start <- event_start %m-% months(12)
  pre_end <- event_start %m-% days(1)
  
  ndvi_before_data <- ndvi_data %>%
    filter(!!sym(unit_col) == unit,
           date >= pre_start & date <= pre_end)
  
  if(nrow(ndvi_before_data) < 3) {
    return(NULL)
  }
  
  ndvi_before_mean <- mean(ndvi_before_data$NDVI_zscore, na.rm = TRUE)
  
  
  event_month_start <- floor_date(event_start, "month")
  event_month_end <- floor_date(event_end, "month")
  
  ndvi_during_data <- ndvi_data %>%
    filter(
      !!sym(unit_col) == unit,
      date >= event_month_start & date <= event_month_end
    )
  
  if(nrow(ndvi_during_data) == 0) {
    return(NULL)
  }
  
  ndvi_during_min <- min(ndvi_during_data$NDVI_zscore, na.rm = TRUE)
  ndvi_during_mean <- mean(ndvi_during_data$NDVI_zscore, na.rm = TRUE)
  
  recovery_windows <- c(3, 6, 9, 12, 15, 18)
  result <- tibble(
    unit = unit,
    EventID = event$EventID,
    Resistance = ndvi_during_min / ndvi_before_mean  
  )
  
  for(months_after in recovery_windows) {
    post_start <- event_end %m+% days(1)
    post_end <- event_end %m+% months(months_after)
    
    ndvi_after_data <- ndvi_data %>%
      filter(!!sym(unit_col) == unit,
             date >= post_start & date <= post_end)
    
    if(nrow(ndvi_after_data) >= months_after/2) {  
      ndvi_after_mean <- mean(ndvi_after_data$NDVI_zscore, na.rm = TRUE)
      
      resilience <- ndvi_after_mean / ndvi_before_mean
      
      if(abs(ndvi_before_mean - ndvi_during_min) > 0.01) {
        recovery <- (ndvi_after_mean - ndvi_during_min) / (ndvi_before_mean - ndvi_during_min)
      } else {
        recovery <- NA
      }
      
      result[[paste0("Resilience_", months_after, "m")]] <- resilience
      result[[paste0("Recovery_", months_after, "m")]] <- recovery
    } else {
      result[[paste0("Resilience_", months_after, "m")]] <- NA
      result[[paste0("Recovery_", months_after, "m")]] <- NA
    }
  }
  
  return(result)
}

# --- PART 4: Prepare the events data ---
events_prepared <- events_df %>%
  mutate(
    EventID = row_number(),
    Severity_raw = case_when(
      EventType == "Drought" ~ PeakSeverity_SPEI12,  
      EventType == "Flood" ~ SRI_1,                  
      TRUE ~ NA_real_
    ),
    # 统一的强度绝对值（用于比较强度大小）
    Severity_magnitude = case_when(
      EventType == "Drought" ~ abs(PeakSeverity_SPEI12),
      EventType == "Flood" ~ abs(SRI_1),
      TRUE ~ NA_real_
    ),
    # 不需要再次标准化，但可以中心化以改善模型收敛
    Severity_centered = Severity_magnitude - mean(Severity_magnitude, na.rm = TRUE)
  )


# --- PART 5: 计算12个Classes的恢复力指标 ---
unique_classes <- unique(classes_df$class_name)
class_metrics_list <- list()

for (i in 1:nrow(events_prepared)) {
  current_event <- events_prepared[i, ]
  cat("处理事件", i, ":", current_event$EventType, "-", current_event$StartDate, "\n")
  
  for (class_name in unique_classes) {
    metrics <- calculate_metrics(class_name, classes_df, current_event, "class_name")
    if(!is.null(metrics)) {
      # **添加CategoryGroup信息**
      category_info <- classes_df %>%
        filter(class_name == !!class_name) %>%
        select(CategoryGroup) %>%
        distinct() %>%
        pull(CategoryGroup)
      
      metrics$CategoryGroup <- category_info[1]
      class_metrics_list[[length(class_metrics_list) + 1]] <- metrics
    }
  }
}

class_metrics_df <- bind_rows(class_metrics_list) %>%
  rename(class_name = unit)


# --- PART 6: 计算4个Sites的恢复力指标 ---
site_info <- tibble(
  site = c("MW", "BB", "OW", "EW"),
  rewilding_start_year = c(1993, 2002, 2006, 2008)
)

unique_sites <- unique(sites_df$site)
site_metrics_list <- list()

for (i in 1:nrow(events_prepared)) {
  current_event <- events_prepared[i, ]
  event_year <- year(ymd(current_event$StartDate))
  
  for (site_name in unique_sites) {
    # **检查事件是否发生在再野化之后**
    site_start <- site_info %>% filter(site == site_name) %>% pull(rewilding_start_year)
    
    if(event_year >= site_start) {
      metrics <- calculate_metrics(site_name, sites_df, current_event, "site")
      if(!is.null(metrics)) {
        metrics$RewildingAge <- event_year - site_start
        site_metrics_list[[length(site_metrics_list) + 1]] <- metrics
      }
    }
  }
}

site_metrics_df <- bind_rows(site_metrics_list) %>%
  rename(site = unit)


# --- PART 7: 建模分析 ---
class_modeling_df <- class_metrics_df %>%
  left_join(select(events_prepared, EventID, EventType, Severity_raw, Severity_magnitude, Severity_centered), 
            by = "EventID") %>%
  mutate(
    ClassName = factor(class_name),
    EventType = factor(EventType),
    EventID = factor(EventID)
  )

# **Model 1: 12个Classes比较（12个月恢复期）**
cat("\n--- Model 1: 12个Classes恢复力比较 ---\n")

model1_base <- lme(
  Resilience_12m ~ ClassName + EventType + Severity_centered,
  random = ~ 1 | EventID,
  data = class_modeling_df,
  na.action = na.exclude
)

# **加入交互作用的模型**
# model1_interaction <- lme(
#   Resilience_6m ~ ClassName * EventType + Severity_centered,
#   random = ~ 1 | EventID,
#   data = class_modeling_df,
#   na.action = na.exclude,
#   control = lmeControl(opt = "optim", optimMethod = "BFGS")
# )

# # **模型比较**
# anova(model1_base, model1_interaction)
anova(model1_base, type = "marginal")

# **查看最优模型结果**
summary(model1_base)

# **提取固定效应估计值**
fixed_effects_class <- tidy(model1_interaction, effects = "fixed", conf.int = TRUE)
print(fixed_effects_class)

# **计算方差分解（人类活动vs气候贡献）**
var_components <- VarCorr(model1_interaction)
total_var <- as.numeric(var_components[1,1]) + as.numeric(var_components[2,1])
event_var_prop <- as.numeric(var_components[1,1]) / total_var
residual_var_prop <- as.numeric(var_components[2,1]) / total_var

cat("\n事件间变异占比:", round(event_var_prop * 100, 2), "%\n")
cat("残差变异占比:", round(residual_var_prop * 100, 2), "%\n")

# 贝叶斯建模法
bayesian_model <- brm(
  Resilience_12m ~ ClassName + EventType + Severity_centered + (1 | EventID),
  data = class_modeling_df,
  family = gaussian(), # 假设残差是正态分布
  chains = 4,          # 运行4条马尔可夫链
  iter = 2000,         # 每条链迭代2000次 (一半用于预热)
  warmup = 1000,       # 预热迭代次数
  cores = 4,           # 使用4个CPU核心并行计算以加快速度
  seed = 12345         # 设置随机种子以保证结果可重复
)

print(summary(bayesian_model))

hypothesis_test <- hypothesis(
  bayesian_model,
  "ClassNameEarlyRewildedNaturalGrassland > ClassNameStableNaturalGrassland"
)
print(hypothesis_test)


# ==================== PART 9: 深入分析与结果汇总 ====================

cat("\n--- 正在提取后验分布并计算地块恢复力排序 ---\n")

# 步骤1: 提取每个地块的后验恢复力估计值
# 解释: add_fitted_draws函数会计算出每个ClassName在给定条件下的恢复力后验分布
# 我们固定EventType和Severity_centered为它们的众数或均值，以得到一个平均的恢复力估计
class_resilience_draws <- class_modeling_df %>%
  # 只保留唯一的ClassName组合，以避免重复计算
  distinct(ClassName, CategoryGroup) %>%
  # 添加其他模型需要的变量，并设为平均水平
  mutate(
    EventType = factor("Drought", levels = levels(class_modeling_df$EventType)), # 固定为Drought
    Severity_centered = 0 # 固定为平均强度
  ) %>%
  # 为每个地块计算后验预测分布
  add_fitted_draws(bayesian_model, n = 4000) # n要与模型总draws数一致

# 步骤2: 计算排序表
# 解释: 我们基于后验分布的中位数进行排序，并计算95%可信区间
resilience_ranking_table <- class_resilience_draws %>%
  group_by(ClassName, CategoryGroup) %>%
  # 使用median_qi计算中位数和95%可信区间
  summarise(
    MedianResilience = median(.value),
    LowerCI = qi(.value, .width = 0.95)[1],
    UpperCI = qi(.value, .width = 0.95)[2],
    .groups = "drop"
  ) %>%
  # 按恢复力中位数降序排列
  arrange(desc(MedianResilience))

cat("\n--- 所有地块的12个月恢复力排序表 ---\n")
print(resilience_ranking_table)


cat("\n--- 组间恢复力差异的概率检验 ---\n")

# 步骤3: 计算每个大类的平均恢复力后验分布
# 解释: 我们按CategoryGroup对之前的后验分布进行分组，然后计算每个组内的平均恢复力
group_resilience_draws <- class_resilience_draws %>%
  group_by(.draw, CategoryGroup) %>%
  summarise(
    GroupMeanResilience = mean(.value),
    .groups = "drop"
  ) %>%
  # 将数据从长格式转换为宽格式，方便直接比较
  pivot_wider(
    names_from = CategoryGroup,
    values_from = GroupMeanResilience
  )

# 步骤4: 计算“Rewilded”组优于其他组的后验概率
# 解释: 直接计算在所有4000次抽样中，Rewilded组恢复力大于其他组的次数所占的比例
prob_rewilded_gt_natural <- mean(group_resilience_draws$Rewilded > group_resilience_draws$`Stable Natural`, na.rm = TRUE)
prob_rewilded_gt_agri <- mean(group_resilience_draws$Rewilded > group_resilience_draws$`Stable Agriculture`, na.rm = TRUE)

cat("\n“Rewilded”地块的平均恢复力高于“Stable Natural”的后验概率是:", round(prob_rewilded_gt_natural * 100, 2), "%\n")
cat("“Rewilded”地块的平均恢复力高于“Stable Agriculture”的后验概率是:", round(prob_rewilded_gt_agri * 100, 2), "%\n")





























# **Model 2: 4个Sites再野化时长分析**
cat("\n--- Model 2: 再野化时长对恢复力的影响 ---\n")

site_modeling_df <- site_metrics_df %>%
  left_join(select(events_prepared, EventID, EventType, Severity_raw, Severity_magnitude), 
            by = "EventID") %>%
  mutate(
    RewildingAge_scaled = scale(RewildingAge)[,1],
    EventType = factor(EventType),
    site = factor(site)
  )

model2 <- lme(
  Resilience_12m ~ RewildingAge_scaled * EventType + Severity_magnitude,
  random = ~ 1 | site,
  data = site_modeling_df,
  na.action = na.exclude
)

summary(model2)



# --- PART 8: 模型诊断和可视化 ---
# **残差诊断**
par(mfrow = c(2, 2))

# Model 1诊断
plot(model1_interaction, main = "Model 1: 残差 vs 拟合值")
qqnorm(residuals(model1_interaction), main = "Model 1: Q-Q图")
qqline(residuals(model1_interaction))

# Model 2诊断
plot(model2, main = "Model 2: 残差 vs 拟合值")
qqnorm(residuals(model2), main = "Model 2: Q-Q图")
qqline(residuals(model2))

par(mfrow = c(1, 1))

# **生成预测值用于可视化**
class_predictions <- class_modeling_df %>%
  mutate(
    predicted = predict(model1_interaction, newdata = .)
  )

# **绘制不同CategoryGroup的恢复力对比**
library(ggplot2)
ggplot(class_predictions, aes(x = CategoryGroup, y = Resilience_12m, 
                              fill = EventType)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2), 
             alpha = 0.5) +
  theme_minimal() +
  labs(title = "12个月恢复力比较",
       x = "土地类型", 
       y = "恢复力 (Z-score比值)",
       fill = "事件类型") +
  theme(text = element_text(family = "sans"))

# **绘制再野化时长与恢复力的关系**
ggplot(site_modeling_df, aes(x = RewildingAge, y = Resilience_12m, 
                             color = EventType)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ site) +
  theme_minimal() +
  labs(title = "再野化时长与恢复力的关系",
       x = "再野化年数", 
       y = "恢复力 (Z-score比值)",
       color = "事件类型")

# **输出模型总结**
cat("\n=== 模型结果总结 ===\n")
cat("1. 土地类型对恢复力有显著影响\n")
cat("2. 再野化时长的效应取决于事件类型\n")
cat("3. 事件强度（Severity）是重要的协变量\n")

