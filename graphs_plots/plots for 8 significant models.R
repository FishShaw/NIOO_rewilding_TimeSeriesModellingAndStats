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


# 诊断干旱事件的唯一值
spei_variability_check <- class_modelling_base_df %>%
  filter(EventType == "Drought") %>%
  group_by(TimeWindow) %>%
  summarise(
    # 计算每个时间窗口内，非NA的SPEI值的数量
    N_Observations = n(),
    # 计算每个时间窗口内，唯一的SPEI值的数量
    Unique_SPEI_Values = n_distinct(PeakSeverity_SPEI12, na.rm = TRUE)
  ) %>%
  ungroup()

# 打印结果
print(spei_variability_check)

cat("\n\n--- 6个月窗口内，SPEI值的具体分布 ---\n")
spei_distribution_6m <- class_modelling_base_df %>%
  filter(EventType == "Drought", TimeWindow == 6) %>%
  group_by(PeakSeverity_SPEI12) %>%
  summarise(
    Count = n()
  ) %>%
  arrange(desc(Count))

print(spei_distribution_6m)

# --- 检查3: 查看9个月窗口内，具体的SPEI值及其分布 ---
cat("\n\n--- 9个月窗口内，SPEI值的具体分布 ---\n")
spei_distribution_9m <- class_modelling_base_df %>%
  filter(EventType == "Drought", TimeWindow == 9) %>%
  group_by(PeakSeverity_SPEI12) %>%
  summarise(
    Count = n()
  ) %>%
  arrange(desc(Count))

print(spei_distribution_9m)


















# plots 部分
# 准备数据 - 干旱Recovery @ 6个月
drought_6m_data <- class_modelling_base_df %>%
  filter(EventType == "Drought", TimeWindow == 6)

# 运行主效应模型
lme_model_1 <- lme(Recovery ~ EcologicalGroup + PeakSeverity_SPEI12,
                   random = ~ 1 | EventID / Pixel_ID,
                   data = drought_6m_data,
                   na.action = na.exclude)


emm_groups <- emmeans(lme_model_1, ~ EcologicalGroup)
pairwise_comp <- pairs(emm_groups, adjust = "tukey")
cld_result <- cld(emm_groups, Letters = letters, alpha = 0.05)

plot_data <- as.data.frame(cld_result) %>%
  mutate(
    UnitName = factor(EcologicalGroup, 
                      levels = c("Stable Natural", "Stable Agriculture", "Early Started Rewilded", "Late Started Rewilded"),
                      ordered = FALSE),
    # 为不同组设置颜色
    GroupColor = case_when(
      EcologicalGroup == "Stable Natural" ~ "#2E8B57",      # 深绿色
      EcologicalGroup == "Stable Agriculture" ~ "#DAA520",   # 金黄色  
      EcologicalGroup == "Early Started Rewilded" ~ "#4682B4", # 钢蓝色
      EcologicalGroup == "Late Started Rewilded" ~ "#B22222"   # 砖红色
    )
  ) %>%
  arrange(EcologicalGroup)


# 获取显著差异的配对用于连线
sig_pairs <- as.data.frame(summary(pairwise_comp)) %>%
  filter(p.value < 0.05) %>%
  mutate(
    group1 = str_extract(contrast, "^[^-]+") %>% str_trim(),
    group2 = str_extract(contrast, "(?<= - ).+$") %>% str_trim()
  ) %>%
  dplyr::select(group1, group2, p.value)

# === 第六步：创建科研级别的图表 ===
# Step 6: Create scientific-quality plot

# 设置显著性标记函数
get_significance_stars <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else return("")
}

# 创建基础图形
p_base <- ggplot(plot_data, aes(x = EcologicalGroup, y = emmean, fill = EcologicalGroup)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.2, size = 0.8, color = "black") +
  scale_fill_manual(values = plot_data$GroupColor) +
  
  # 添加紧凑字母显示
  geom_text(aes(label = .group, y = upper.CL + 0.05), 
            size = 5, fontface = "bold") +
  
  # 主题设置
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey90", size = 0.5),
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8)
  ) +
  
  labs(
    title = "Vegetation Recovery Response to Drought Events",
    subtitle = "6-Month Recovery Period by Ecological Groups",
    x = "Ecological Group",
    y = "Recovery Index (Estimated Marginal Mean ± 95% CI)",
    caption = "Letters indicate significant differences (Tukey HSD, α = 0.05)\n*** p < 0.001, ** p < 0.01, * p < 0.05"
  )

# 如果有显著的配对，添加连线
if (nrow(sig_pairs) > 0) {
  # 为每个显著配对添加连线和星号
  max_y <- max(plot_data$upper.CL)
  line_height_increment <- (max_y - min(plot_data$lower.CL)) * 0.1
  
  for (i in 1:nrow(sig_pairs)) {
    pair <- sig_pairs[i, ]
    
    # 获取组的位置
    pos1 <- which(levels(plot_data$EcologicalGroup) == pair$group1)
    pos2 <- which(levels(plot_data$EcologicalGroup) == pair$group2)
    
    # 计算连线高度
    line_y <- max_y + 0.1 + (i-1) * line_height_increment
    
    # 添加连线
    p_base <- p_base +
      annotate("segment", 
               x = pos1, xend = pos2, 
               y = line_y, yend = line_y,
               color = "black", size = 0.8) +
      annotate("segment", 
               x = pos1, xend = pos1, 
               y = line_y - line_height_increment/4, yend = line_y,
               color = "black", size = 0.8) +
      annotate("segment", 
               x = pos2, xend = pos2, 
               y = line_y - line_height_increment/4, yend = line_y,
               color = "black", size = 0.8) +
      annotate("text", 
               x = (pos1 + pos2)/2, y = line_y + line_height_increment/4,
               label = get_significance_stars(pair$p.value),
               size = 5, fontface = "bold")
  }
  
  # 调整y轴范围以容纳连线
  y_expand <- max_y + 0.1 + nrow(sig_pairs) * line_height_increment + 0.1
  p_base <- p_base + ylim(min(plot_data$lower.CL) * 0.9, y_expand)
}

print(p_base)










# ===================================================================
# 主效应模型作图函数 (已更新，增加CLD字母和显著性连线)
# ===================================================================
generate_main_effect_plot <- function(base_df, 
                                      predictor_var, # 新增: 动态预测变量
                                      metric_name, 
                                      event_type, 
                                      time_window) {
  
  # --- 新增: 确保 multcomp 包已安装并加载 ---
  # 如果未安装，请运行: install.packages("multcomp")
  if (!requireNamespace("multcomp", quietly = TRUE)) {
    stop("请先安装 'multcomp' 包: install.packages('multcomp')")
  }
  
  if (event_type == "Drought") {
    covariate_variable <- "PeakSeverity_SPEI12"
  } else if (event_type == "Flood") {
    covariate_variable <- "SRI_3"
  } else {
    stop(sprintf("错误: 未知的事件类型 '%s'.", event_type))
  }
  
  cat(sprintf("--- 主效应分析: 指标[%s], 事件[%s], 时间窗口[%d个月], 预测变量[%s] ---\n", 
              metric_name, event_type, time_window, predictor_var))
  
  model_data <- base_df %>%
    filter(EventType == event_type, TimeWindow == time_window) %>%
    filter(!is.na(.data[[covariate_variable]]))
  
  if (nrow(model_data) < 20 || n_distinct(model_data[[predictor_var]]) < 2) {
    cat("警告: 数据不足或预测变量级别少于2，已跳过。\n"); return(NULL)
  }
  
  model_formula <- as.formula(sprintf("%s ~ %s + %s", metric_name, predictor_var, covariate_variable))
  
  lme_model <- tryCatch({
    lme(model_formula, random = ~ 1 | EventID / Pixel_ID, data = model_data, na.action = na.exclude)
  }, error = function(e) {
    cat(sprintf("错误: 模型拟合失败: %s\n", e$message)); return(NULL)
  })
  
  if (is.null(lme_model)) return(NULL)
  
  emm_groups <- emmeans(lme_model, as.formula(paste("~", predictor_var)))
  pairwise_comp <- pairs(emm_groups, adjust = "tukey")
  
  # --- 更新: 计算CLD结果并准备绘图数据 ---
  cld_results <- multcomp::cld(emm_groups, Letters = letters, alpha = 0.05)
  
  plot_data <- as.data.frame(cld_results) %>%
    mutate(
      !!predictor_var := factor(.data[[predictor_var]], levels = unique(base_df[[predictor_var]])),
      # 清理CLD字母列中的空格
      .group = trimws(.group)
    ) %>%
    arrange(match(.data[[predictor_var]], levels(.data[[predictor_var]])))
  
  sig_pairs <- as.data.frame(summary(pairwise_comp)) %>%
    filter(p.value < 0.05) %>%
    mutate(
      group1 = str_trim(str_extract(contrast, "^[^-]+")),
      group2 = str_trim(str_extract(contrast, "(?<= - ).+$"))
    ) %>%
    dplyr::select(group1, group2, p.value)
  
  get_significance_stars <- function(p_value) {
    if (p_value < 0.001) return("***")
    else if (p_value < 0.01) return("**")
    else if (p_value < 0.05) return("*")
    else return("ns")
  }
  
  # --- 更新: ggplot调用，同时添加CLD字母 ---
  letter_offset <- (max(plot_data$upper.CL) - min(plot_data$lower.CL)) * 0.05
  
  p_base <- ggplot(plot_data, aes(x = .data[[predictor_var]], y = emmean, fill = .data[[predictor_var]])) +
    geom_bar(stat = "identity", alpha = 0.8, color = "black", width = 0.7) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, size = 0.8, color = "black") +
    # 新增: 在误差线上方添加CLD字母
    geom_text(aes(label = .group, y = upper.CL + letter_offset), 
              size = 5, vjust = 0, fontface = "bold", color = "black") +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12),
      axis.text.y = element_text(color = "black", size = 12),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5, size = 10),
      legend.position = "none"
    ) +
    labs(
      title = sprintf("Main Effect on %s for %s Events", 
                      stringr::str_to_title(metric_name), 
                      event_type),
      subtitle = sprintf("%d-Month Window (Adjusted for %s)", 
                         time_window, 
                         covariate_variable),
      x = predictor_var,
      y = sprintf("Adjusted %s (EMMean ± 95%% CI)", stringr::str_to_title(metric_name)),
      caption = "Letters indicate significant differences (Tukey HSD, p < 0.05)\n*** p < 0.001, ** p < 0.01, * p < 0.05"
    )
  
  all_custom_colors <- c(
    "Stable Natural" = "#2E8B57", "Stable Agriculture" = "#DAA520",
    "Early Started Rewilded" = "#4682B4", "Late Started Rewilded" = "#B22222",
    "1_Earliest Started" = "#4682B4", "2_Mid-Early Started" = "#73a2c9",
    "3_Mid-Late Started" = "#d17c7c", "4_Latest Started" = "#B22222"
  )
  p_base <- p_base + scale_fill_manual(values = all_custom_colors)
  
  # --- 更新: 调整显著性连线的起始高度 ---
  if (nrow(sig_pairs) > 0) {
    sig_pairs$dist <- abs(match(sig_pairs$group1, levels(plot_data[[predictor_var]])) - match(sig_pairs$group2, levels(plot_data[[predictor_var]])))
    sig_pairs <- sig_pairs %>% arrange(dist)
    
    # 调整起始高度，确保连线在字母上方
    max_y <- max(plot_data$upper.CL) + letter_offset * 2 
    y_range <- diff(range(c(plot_data$lower.CL, plot_data$upper.CL), na.rm = TRUE))
    line_increment <- y_range * 0.15
    y_positions <- c() 
    
    for (i in 1:nrow(sig_pairs)) {
      pair <- sig_pairs[i, ]
      pos1 <- which(levels(plot_data[[predictor_var]]) == pair$group1)
      pos2 <- which(levels(plot_data[[predictor_var]]) == pair$group2)
      
      line_y <- max_y + line_increment * 1.2
      while(any(abs(line_y - y_positions) < line_increment)) {
        line_y <- line_y + line_increment
      }
      y_positions <- c(y_positions, line_y)
      
      p_base <- p_base +
        annotate("segment", x = pos1, xend = pos2, y = line_y, yend = line_y, color = "black", size = 0.7) +
        annotate("segment", x = pos1, xend = pos1, y = line_y - line_increment * 0.1, yend = line_y, color = "black", size = 0.7) +
        annotate("segment", x = pos2, xend = pos2, y = line_y - line_increment * 0.1, yend = line_y, color = "black", size = 0.7) +
        annotate("text", x = (pos1 + pos2) / 2, y = line_y + line_increment * 0.1,
                 label = get_significance_stars(pair$p.value), size = 6, fontface = "bold")
    }
    
    p_base <- p_base + coord_cartesian(ylim = c(min(plot_data$lower.CL, na.rm = TRUE) * 0.9, max(y_positions) * 1.1), clip = "off")
  }
  
  cat(sprintf("--- 主效应分析在 %d 个月处理完成 ---\n\n", time_window))
  return(p_base)
}



run_full_main_effect_analysis <- function(event_type, 
                                          metric_name, 
                                          base_df,
                                          predictor_var, # 新增: 动态预测变量
                                          time_windows = c(3, 6, 9, 12, 15, 18)) {
  
  cat(sprintf("====== 开始对 [%s] 事件的 [%s] 指标进行完整主效应分析 ======\n", event_type, metric_name))
  
  all_plots <- lapply(time_windows, function(tw) {
    generate_main_effect_plot(
      base_df = base_df,
      predictor_var = predictor_var, # 传递新参数
      metric_name = metric_name,
      event_type = event_type,
      time_window = tw
    )
  })
  
  all_plots <- all_plots[!sapply(all_plots, is.null)]
  
  if (length(all_plots) > 0) {
    cat("\n--- 正在R绘图窗口中显示所有结果图... ---\n")
    for (p in all_plots) {
      print(p)
    }
  } else {
    cat("--- 未生成任何有效图形。请检查数据。 ---\n")
  }
  
  cat(sprintf("====== [%s] 事件的 [%s] 指标分析完成 ======\n\n", event_type, metric_name))
  
  invisible(all_plots)
}

run_full_main_effect_analysis(
  event_type = "Drought",
  metric_name = "Recovery",
  base_df = class_modelling_base_df,
  predictor_var = "EcologicalGroup"
)

# 示例2: 分析 RewildingRank (主效应)
run_full_main_effect_analysis(
 event_type = "Flood",
 metric_name = "Recovery_new",
 base_df = site_modeling_base_df,
  predictor_var = "RewildingRank")



















# 交互模型专用 ----
generate_interaction_analysis <- function(base_df, 
                                          predictor_var, # 新增参数: 指定预测变量
                                          metric_name, 
                                          event_type, 
                                          time_window) {
  
  if (event_type == "Drought") {
    severity_variable <- "PeakSeverity_SPEI12"
    plot_range <- seq(-2.5, -1.5, by = 0.1) 
  } else if (event_type == "Flood") {
    severity_variable <- "SRI_3"
    plot_range <- seq(1, 3, by = 0.2) 
  } else {
    stop(sprintf("错误: 未知的事件类型 '%s'.", event_type))
  }
  
  cat(sprintf("--- 交互分析: 指标[%s], 事件[%s], 时间窗口[%d个月], 预测变量[%s] ---\n", 
              metric_name, event_type, time_window, predictor_var))
  
  # 1. 准备数据
  model_data <- base_df %>%
    filter(EventType == event_type, TimeWindow == time_window) %>%
    filter(!is.na(.data[[severity_variable]])) 
  
  # 新增检查: 确保在当前数据子集中，预测变量至少有两个级别
  if (n_distinct(model_data[[predictor_var]], na.rm = TRUE) < 2) {
    cat(sprintf("警告: 预测变量 '%s' 在此数据子集中只有一个或更少的级别，已跳过。\n", predictor_var))
    return(NULL)
  }
  
  if (nrow(model_data) < 20) {
    cat("警告: 数据不足，已跳过。\n"); return(NULL)
  }
  
  # 2. 动态创建模型公式 (使用 predictor_var)
  model_formula <- as.formula(
    sprintf("%s ~ %s * %s", metric_name, predictor_var, severity_variable)
  )
  
  lme_model <- tryCatch({
    lme(model_formula,
        random = ~ 1 | EventID / Pixel_ID,
        data = model_data,
        na.action = na.exclude)
  }, error = function(e) {
    cat(sprintf("错误: 模型拟合失败: %s\n", e$message)); return(NULL)
  })
  
  if (is.null(lme_model)) return(NULL)
  
  # 3. 核心绘图逻辑
  at_list <- list()
  at_list[[severity_variable]] <- plot_range
  
  emm_results <- tryCatch({
    # 使用 predictor_var
    emmeans(lme_model, as.formula(paste("~", predictor_var, "|", severity_variable)), at = at_list)
  }, error = function(e) {
    cat(sprintf("错误: emmeans 计算失败: %s\n", e$message)); return(NULL)
  })
  
  if(is.null(emm_results)) return(NULL)
  
  plot_data <- as.data.frame(emm_results)
  
  # 使用 predictor_var 进行 ggplot 绘图
  interaction_plot <- ggplot(plot_data, aes(x = .data[[severity_variable]], y = emmean, color = .data[[predictor_var]])) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = .data[[predictor_var]], group = .data[[predictor_var]]), alpha = 0.15, linetype = 0) +
    geom_line(aes(group = .data[[predictor_var]]), size = 1.2) +
    labs(
      title = sprintf("Interaction: %s vs. %s", stringr::str_to_title(metric_name), predictor_var),
      subtitle = sprintf("%d-Month Window for %s Events", time_window, event_type),
      x = severity_variable, 
      y = sprintf("Predicted %s (EMMean ± 95%% CI)", stringr::str_to_title(metric_name)),
      color = predictor_var, # 动态图例标题
      fill = predictor_var  # 动态图例标题
    ) +
    theme_minimal(base_size = 14) + 
    theme(legend.position = "bottom")
  
  # --- 应用统一的自定义颜色方案 ---
  # Define a comprehensive color palette for all models
  all_custom_colors <- c(
    # Colors for EcologicalGroup
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520",
    "Early Started Rewilded" = "#4682B4",
    "Late Started Rewilded" = "#B22222",
    # Colors for RewildingRank
    "1_Earliest Started"   = "#4682B4", # Steel Blue
    "2_Mid-Early Started"  = "#73a2c9", # Lighter Steel Blue
    "3_Mid-Late Started"   = "#d17c7c", # Lighter Firebrick
    "4_Latest Started"     = "#B22222"  # Firebrick
  )
  
  # Apply the custom color scales
  interaction_plot <- interaction_plot +
    scale_color_manual(values = all_custom_colors) +
    scale_fill_manual(values = all_custom_colors)
  
  # 4. emtrends 分析 (使用 predictor_var)
  trends <- emtrends(lme_model, as.formula(paste("~", predictor_var)), var = severity_variable)
  trends_pairs <- pairs(trends)
  
  cat("\n--- 各分组的斜率 (Trend) 分析 ---\n")
  print(summary(trends))
  cat("\n--- 斜率的成对比较 ---\n")
  print(summary(trends_pairs))
  cat(sprintf("--- 交互效应分析在 %d 个月处理完成 ---\n\n", time_window))
  
  return(list(
    plot = interaction_plot,
    trends_summary = summary(trends),
    trends_comparison = summary(trends_pairs)
  ))
}

# 运行函数
run_full_interaction_analysis <- function(event_type, 
                                          metric_name, 
                                          base_df,
                                          predictor_var, # 新增参数
                                          time_windows = c(3, 6, 9, 12, 15, 18)) {
  
  cat(sprintf("====== 开始对 [%s] 事件的 [%s] 指标进行完整交互效应分析 ======\n", event_type, metric_name))
  
  all_results <- lapply(time_windows, function(tw) {
    generate_interaction_analysis(
      base_df = base_df,
      predictor_var = predictor_var, # 传递新参数
      metric_name = metric_name,
      event_type = event_type,
      time_window = tw
    )
  })
  
  all_results <- all_results[!sapply(all_results, is.null)]
  
  if (length(all_results) > 0) {
    cat("\n--- 正在R绘图窗口中显示所有结果图... ---\n")
    for (res in all_results) {
      print(res$plot)
    }
  } else {
    cat("--- 未生成任何有效图形。请检查数据。 ---\n")
  }
  
  cat(sprintf("====== [%s] 事件的 [%s] 指标分析完成 ======\n\n", event_type, metric_name))
  
  invisible(all_results)
}

# For flood
run_full_interaction_analysis(
  event_type = "Flood",
  metric_name = "Recovery_new",
  base_df = site_modeling_base_df,
  predictor_var = "RewildingRank" #or EcologicalGroup
)

# For drought
run_full_interaction_analysis(
  event_type = "Drought",
  metric_name = "Resistance_new",
  base_df = class_modelling_base_df,
  predictor_var = "EcologicalGroup")























# plots 探索
significant_data <- class_modelling_base_df %>%
  filter(EventType == "Drought", TimeWindow == 6)

# 绘制箱线图
ggplot(significant_data, aes(x = UnitName, y = Recovery, fill = EcologicalGroup)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c(
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520", 
    "Early Started Rewilded" = "#4682B4",
    "Late Started Rewilded" = "#B22222"
  )) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top"
  ) +
  labs(
    title = "Recovery Response to Drought (6 months) - Detailed View",
    subtitle = "Showing all 12 land cover classes grouped by ecological category",
    x = "Land Cover Class",
    y = "Recovery Index",
    fill = "Ecological Group"
  ) 

# 绘制小提琴图+散点
ggplot(significant_data, aes(x = EcologicalGroup, y = Recovery, color = UnitName)) +
  geom_violin(aes(fill = EcologicalGroup), alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~ EventType)

# 计算每个子类别的均值和标准差
sub_stats <- significant_data %>%
  group_by(EcologicalGroup, UnitName) %>%
  summarise(
    mean_recovery = mean(Recovery, na.rm = TRUE),
    sd_recovery = sd(Recovery, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  
  ggplot(sub_stats, aes(x = reorder(UnitName, mean_recovery), 
                        y = mean_recovery, 
                        color = EcologicalGroup)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_recovery - se_recovery, 
                    ymax = mean_recovery + se_recovery), 
                width = 0.2) +
  scale_color_manual(values = c(
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520", 
    "Early Started Rewilded" = "#4682B4",
    "Late Started Rewilded" = "#B22222"
  )) +
  theme_classic() +
  coord_flip() +
  labs(
    title = "Mean Recovery by Land Cover Class",
    subtitle = "Error bars show ±1 SE",
    x = "Land Cover Class",
    y = "Mean Recovery Index"
  )

# 查看某个大类内部的变异
sub_stats %>%
  filter(EcologicalGroup == "Early Started Rewilded")


significant_summary <- tibble(
  响应变量 = c("Recovery", "Recovery", "Recovery", "Recovery", "Recovery", 
           "Recovery", "Recovery", "Resistance", "Resilience"),
  事件类型 = c("干旱", "洪水", "干旱", "干旱", "干旱", "干旱", "干旱", 
           "洪水", "洪水"),
  时间窗口 = c(3, 3, 6, 9, 12, 15, 18, 18, 15),
  分析类型 = c("生态大类", "生态大类", "生态大类", "生态大类", "生态大类", 
           "生态大类", "生态大类", "生态大类", "站点排序"),
  模型类型 = c("交互效应", "交互效应", "主效应", "主效应", "主效应", 
           "主效应", "主效应", "交互效应", "主效应"),
  P值 = c(0.0095, 0.0001, 0.0012, 0.0002, 0.0001, 0.0001, 0.0005, 
         0.0484, 0.0186),
  主要发现 = c(
    "短期恢复受事件严重程度调节",
    "短期恢复受事件严重程度调节", 
    "早期再野化>稳定农业(p<0.001)",
    "早期再野化>稳定农业(p<0.001)",
    "早期再野化>稳定农业(p<0.01)",
    "早期再野化>稳定农业(p<0.001)",
    "早期再野化>稳定农业(p<0.001)",
    "组间差异受事件严重程度调节",
    "中早期站点>最晚期站点(p<0.05)"
  )
)

print(significant_summary)









# boxplot单独，无显著性连线
drought_6m_data <- class_modelling_base_df %>%
  filter(EventType == "Drought", TimeWindow == 6)


# 运行模型获取估计边际均值
lme_model <- lme(Recovery ~ EcologicalGroup + PeakSeverity_SPEI12,
                 random = ~ 1 | EventID / Pixel_ID,
                 data = drought_6m_data,
                 na.action = na.exclude)

# 获取估计边际均值和字母显示
emm_groups <- emmeans(lme_model, ~ EcologicalGroup)
cld_result <- cld(emm_groups, Letters = letters, alpha = 0.05)

# 准备绘图数据
plot_data <- as.data.frame(cld_result) %>%
  mutate(
    EcologicalGroup = factor(EcologicalGroup, 
                             levels = c("Stable Natural", "Stable Agriculture", 
                                        "Early Started Rewilded", "Late Started Rewilded")),
    GroupColor = case_when(
      EcologicalGroup == "Stable Natural" ~ "#2E8B57",
      EcologicalGroup == "Stable Agriculture" ~ "#DAA520",
      EcologicalGroup == "Early Started Rewilded" ~ "#4682B4",
      EcologicalGroup == "Late Started Rewilded" ~ "#B22222"
    )
  )

# 创建论文级别图表
p_publication <- ggplot(plot_data, aes(x = EcologicalGroup, y = emmean)) +
  # 条形图
  geom_bar(stat = "identity", aes(fill = EcologicalGroup), 
           alpha = 0.8, color = "black", width = 0.7) +
  # 误差线
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.2, size = 0.8, color = "black") +
  # 显著性字母
  geom_text(aes(label = .group, y = upper.CL + 0.05), 
            size = 5, fontface = "bold") +
  # 颜色设置
  scale_fill_manual(values = c(
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520",
    "Early Started Rewilded" = "#4682B4",
    "Late Started Rewilded" = "#B22222"
  )) +
  # 主题设置
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey90", size = 0.3),
    axis.line = element_line(color = "black", size = 0.8)
  ) +
  labs(
    title = "Vegetation Recovery Response to Drought Events",
    subtitle = "6-Month Recovery Period",
    x = "Ecological Group",
    y = "Recovery Index (EMM ± 95% CI)"
  ) +
  # 添加样本量信息
  annotate("text", x = 4.5, y = min(plot_data$lower.CL) - 0.15, 
           label = paste("N =", nrow(drought_6m_data), "pixels from 3 drought events"),
           size = 3, hjust = 1, fontface = "italic")

print(p_publication)









# 3R metrics 历史趋势图
# 1.单独画图，对我来说最重要的只有recovery drought 3，6，9，12，15，18
# 准备所有时间窗口的数据
all_timewindows_data <- class_modelling_base_df %>%
  filter(EventType == "Drought", TimeWindow %in% c(3, 6, 9, 12, 15, 18)) %>%
  group_by(EcologicalGroup, TimeWindow) %>%
  summarise(
    mean_recovery = mean(Recovery, na.rm = TRUE),
    se_recovery = sd(Recovery, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

p_timeline <- ggplot(all_timewindows_data, 
                     aes(x = TimeWindow, y = mean_recovery, 
                         color = EcologicalGroup, group = EcologicalGroup)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = mean_recovery - se_recovery, 
                  ymax = mean_recovery + se_recovery,
                  fill = EcologicalGroup), 
              alpha = 0.2, linetype = 0) +
  scale_color_manual(values = c(
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520",
    "Early Started Rewilded" = "#4682B4", 
    "Late Started Rewilded" = "#B22222"
  )) +
  scale_fill_manual(values = c(
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520",
    "Early Started Rewilded" = "#4682B4",
    "Late Started Rewilded" = "#B22222"
  )) +
  theme_classic() +
  labs(
    title = "Recovery Trajectories Across Time Windows",
    subtitle = "Response to drought events",
    x = "Time Window (months)",
    y = "Mean Recovery Index (± SE)",
    color = "Ecological Group",
    fill = "Ecological Group"
  )

print(p_timeline)




# 简化版本 - 创建一个大型面板图
create_panel_plot <- function(data) {
  
  # 准备数据
  summary_data <- data %>%
    filter(TimeWindow %in% c(3, 6, 9, 12, 15, 18)) %>%
    mutate(
      GroupVar = ifelse(AnalysisType == "Regional Class", 
                        EcologicalGroup, 
                        UnitName),
      PlotGroup = paste(EventType, "-", AnalysisType)
    ) %>%
    group_by(GroupVar, TimeWindow, EventType, AnalysisType, PlotGroup) %>%
    summarise(
      across(c(Resistance, Resilience, Recovery,Resilience_new, Resistance_new, Recovery_new), 
             list(mean = ~mean(.x, na.rm = TRUE),
                  se = ~sd(.x, na.rm = TRUE)/sqrt(n())),
             .names = "{.col}_{.fn}"),
      .groups = 'drop'
    )
  
  # 转换为长格式
  summary_long <- summary_data %>%
    pivot_longer(
      cols = matches("mean|se"),
      names_to = c("Metric", ".value"),
      names_pattern = "(.*)_(mean|se)"
    )
  
  # 创建面板图
  p <- ggplot(summary_long, 
              aes(x = TimeWindow, y = mean, 
                  color = GroupVar, group = GroupVar)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se, 
                    fill = GroupVar), 
                alpha = 0.2, linetype = 0) +
    facet_grid(Metric ~ PlotGroup, scales = "free_y") +
    scale_x_continuous(breaks = c(3, 6, 9, 12, 15, 18)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = "All Resilience Metrics Across Time Windows",
      x = "Time Window (months)",
      y = "Mean Value ± SE",
      color = "Group",
      fill = "Group"
    )
  
  return(p)
}

# 使用
panel_plot <- create_panel_plot(full_modeling_df)
print(panel_plot)

# # 保存大图
# ggsave("All_resilience_metrics_panel.png", 
#        panel_plot, 
#        width = 16, height = 12, dpi = 300)



# 创建综合绘图函数
plot_all_resilience_timeseries <- function(data, save_plots = TRUE, output_dir = NULL) {
  
  # 加载必要的库
  library(tidyverse)
  library(patchwork)
  
  # 定义配色方案
  ecological_colors <- c(
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520",
    "Early Started Rewilded" = "#4682B4",
    "Late Started Rewilded" = "#B22222"
  )
  
  site_colors <- c(
    "MW" = "#d95f02",  # 1993
    "BB" = "#7570b3",  # 2002
    "OW" = "#1b9e77",  # 2006
    "EW" = "#e7298a"   # 2008
  )
  
  # 定义参数组合
  response_vars <- c("Resistance", "Resilience", "Recovery")
  event_types <- c("Drought", "Flood")
  analysis_types <- c("Regional Class", "Specific Site")
  
  # 存储所有图表
  all_plots <- list()
  plot_counter <- 1
  
  # 创建绘图函数
  create_timeseries_plot <- function(plot_data, response_var, event_type, 
                                     analysis_type, group_var, colors) {
    
    # 计算汇总统计
    summary_data <- plot_data %>%
      filter(TimeWindow %in% c(3, 6, 9, 12, 15, 18)) %>%
      group_by(!!sym(group_var), TimeWindow) %>%
      summarise(
        mean_value = mean(!!sym(response_var), na.rm = TRUE),
        se_value = sd(!!sym(response_var), na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = 'drop'
      )
    
    # 创建图表
    p <- ggplot(summary_data, 
                aes(x = TimeWindow, y = mean_value, 
                    color = !!sym(group_var), 
                    group = !!sym(group_var))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_ribbon(aes(ymin = mean_value - se_value, 
                      ymax = mean_value + se_value,
                      fill = !!sym(group_var)), 
                  alpha = 0.2, linetype = 0) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      scale_x_continuous(breaks = c(3, 6, 9, 12, 15, 18)) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10)
      ) +
      labs(
        title = paste(response_var, "Response to", event_type, "Events"),
        subtitle = paste("Analysis:", analysis_type),
        x = "Time Window (months)",
        y = paste("Mean", response_var, "± SE"),
        color = ifelse(analysis_type == "Regional Class", "Ecological Group", "Site"),
        fill = ifelse(analysis_type == "Regional Class", "Ecological Group", "Site")
      )
    
    # 添加样本量信息
    p <- p + 
      geom_text(data = summary_data %>% filter(TimeWindow == 18),
                aes(label = paste("n=", n)),
                hjust = -0.1, size = 3, show.legend = FALSE)
    
    return(p)
  }
  
  # 循环生成所有图表
  for (response in response_vars) {
    for (event in event_types) {
      for (analysis in analysis_types) {
        
        # 筛选数据
        if (analysis == "Regional Class") {
          plot_data <- data %>%
            filter(EventType == event, 
                   AnalysisType == analysis)
          group_var <- "EcologicalGroup"
          colors <- ecological_colors
        } else {
          plot_data <- data %>%
            filter(EventType == event, 
                   AnalysisType == analysis)
          group_var <- "UnitName"
          colors <- site_colors
        }
        
        # 检查数据是否存在
        if (nrow(plot_data) > 0) {
          # 创建图表
          p <- create_timeseries_plot(plot_data, response, event, 
                                      analysis, group_var, colors)
          
          # 存储图表
          plot_name <- paste(response, event, analysis, sep = "_")
          all_plots[[plot_name]] <- p
          
          # 如果需要保存单独的图表
          if (save_plots && !is.null(output_dir)) {
            filename <- file.path(output_dir, paste0(plot_name, ".png"))
            ggsave(filename, p, width = 8, height = 6, dpi = 300)
          }
          
          plot_counter <- plot_counter + 1
        }
      }
    }
  }
  
  cat(paste("共生成", length(all_plots), "张图表\n"))
  
  return(all_plots)
}

# 创建组合图表展示函数
create_combined_displays <- function(all_plots) {
  
  # 按响应变量组合（每个响应变量一页）
  combined_by_response <- list()
  
  for (response in c("Resistance", "Resilience", "Recovery")) {
    # 筛选该响应变量的所有图
    response_plots <- all_plots[grep(response, names(all_plots))]
    
    if (length(response_plots) == 4) {
      # 2x2 布局
      combined <- (response_plots[[1]] | response_plots[[2]]) / 
        (response_plots[[3]] | response_plots[[4]]) +
        plot_annotation(
          title = paste(response, "Across All Conditions"),
          theme = theme(plot.title = element_text(size = 16, face = "bold"))
        )
      
      combined_by_response[[response]] <- combined
    }
  }
  
  # 按事件类型组合
  combined_by_event <- list()
  
  for (event in c("Drought", "Flood")) {
    event_plots <- all_plots[grep(event, names(all_plots))]
    
    if (length(event_plots) == 6) {
      # 3x2 布局
      combined <- wrap_plots(event_plots, ncol = 2) +
        plot_annotation(
          title = paste("All Resilience Metrics -", event, "Events"),
          theme = theme(plot.title = element_text(size = 16, face = "bold"))
        )
      
      combined_by_event[[event]] <- combined
    }
  }
  
  return(list(
    by_response = combined_by_response,
    by_event = combined_by_event
  ))
}

# 使用函数
# 假设您的数据已经准备好了
all_plots <- plot_all_resilience_timeseries(
  data = full_modeling_df,  # 您的完整数据
  save_plots = TRUE,
  output_dir = "E:/WUR_Intern/RewildingProject_RawData/Timeseries_Plots/"
)

# 创建组合显示
combined_displays <- create_combined_displays(all_plots)

# 显示某个特定的组合图
print(combined_displays$by_response$Recovery)  # 显示所有Recovery的图
print(combined_displays$by_event$Drought)      # 显示所有Drought的图

# 保存组合图
ggsave("Recovery_all_conditions.png", 
       combined_displays$by_response$Recovery, 
       width = 14, height = 10, dpi = 300)





# 交互效应制图
drought_3m_data <- class_modelling_base_df %>%
  filter(EventType == "Drought", TimeWindow == 3)


lme_interaction <- lme(Recovery ~ EcologicalGroup * PeakSeverity_SPEI12,
                       random = ~ 1 | EventID / Pixel_ID,
                       data = drought_3m_data,
                       na.action = na.exclude)

spei_values <- c(-2.5, -1.5, -0.5)  # 严重、中度、轻微干旱

emm_interaction <- emmeans(lme_interaction, 
                           ~ EcologicalGroup | PeakSeverity_SPEI12,
                           at = list(PeakSeverity_SPEI12 = spei_values))

interaction_df <- as.data.frame(emm_interaction)

p_interaction <- ggplot(interaction_df, 
                        aes(x = PeakSeverity_SPEI12, 
                            y = emmean,
                            color = EcologicalGroup,
                            group = EcologicalGroup)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, 
                  fill = EcologicalGroup), 
              alpha = 0.2, linetype = 0) +
  scale_color_manual(values = c(
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520",
    "Early Started Rewilded" = "#4682B4",
    "Late Started Rewilded" = "#B22222"
  )) +
  scale_fill_manual(values = c(
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520",
    "Early Started Rewilded" = "#4682B4",
    "Late Started Rewilded" = "#B22222"
  )) +
  labs(
    title = "Interaction Effect: Recovery Response to Drought Severity",
    subtitle = "3-Month Recovery Period",
    x = "Drought Severity (SPEI-12)",
    y = "Recovery Index",
    color = "Ecological Group",
    fill = "Ecological Group"
  ) +
  scale_x_continuous(breaks = spei_values,
                     labels = c("Severe\n(-2.5)", "Moderate\n(-1.5)", "Mild\n(-0.5)")) +
  theme_classic() +
  theme(legend.position = "right")

print(p_interaction)


slopes <- emtrends(lme_interaction, ~ EcologicalGroup, var = "PeakSeverity_SPEI12")
slopes_df <- as.data.frame(slopes)

p_slopes <- ggplot(slopes_df, aes(x = EcologicalGroup, y = PeakSeverity_SPEI12.trend)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4, aes(color = EcologicalGroup)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, color = EcologicalGroup), 
                width = 0.2, size = 1) +
  scale_color_manual(values = c(
    "Stable Natural" = "#2E8B57",
    "Stable Agriculture" = "#DAA520",
    "Early Started Rewilded" = "#4682B4",
    "Late Started Rewilded" = "#B22222"
  )) +
  labs(
    title = "Differential Response to Drought Severity",
    subtitle = "Slope of Recovery ~ SPEI relationship",
    x = "Ecological Group",
    y = "Slope (Change in Recovery per unit SPEI)",
    caption = "Positive slope = better recovery under less severe drought"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_flip()

print(p_slopes)


pred_grid <- expand.grid(
  EcologicalGroup = levels(drought_3m_data$EcologicalGroup),
  PeakSeverity_SPEI12 = seq(-3, 0, by = 0.1)
)

# 获取预测值
predictions <- predict(lme_interaction, newdata = pred_grid, level = 0)
pred_grid$Recovery_pred <- predictions

# 创建热图
p_heatmap <- ggplot(pred_grid, 
                    aes(x = PeakSeverity_SPEI12, 
                        y = EcologicalGroup, 
                        fill = Recovery_pred)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", name = "Predicted\nRecovery") +
  geom_vline(xintercept = c(-2.5, -1.5, -0.5), 
             linetype = "dashed", color = "white", alpha = 0.5) +
  labs(
    title = "Recovery Response Surface",
    subtitle = "Interaction between ecological group and drought severity",
    x = "Drought Severity (SPEI-12)",
    y = "Ecological Group"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 10)
  )

print(p_heatmap)
