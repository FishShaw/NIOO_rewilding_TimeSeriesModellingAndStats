library(bfast)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)
library(viridis)
library(RColorBrewer)
library(stringr)
library(purrr)

# 1. 数据路径设置 ----

path_4sites_annual_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_4Sites_Annual.csv"
path_4sites_monthly_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_4Sites_Monthly.csv"
path_12classes_annual_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_14Class_Annual.csv"
path_12classes_monthly_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_14Class_Monthly.csv"

output_dir <- "E:/WUR_Intern/RewildingProject_R/BFAST"
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 2. 数据预处理函数 ----
process_site_annual <- function(data) {
  data %>%
    rename(
      UnitName = Site_ID,
      IndexValue = MEAN
    ) %>%
    separate(SourceFile, into = c("IndexType", "Year"), sep = "_", remove = FALSE) %>%
    mutate(
      Year = as.numeric(Year),
      Month = 1,  # 年度数据设为1月
      date = as.Date(paste(Year, Month, "01", sep = "-")),
      IndexValue = ifelse(IndexValue < -1 | IndexValue > 1, NA, IndexValue)
    ) %>%
    arrange(UnitName, date) %>%
    filter(!is.na(Year), !is.na(IndexValue))
}

# 站点月度数据预处理
process_site_monthly <- function(data) {
  data %>%
    rename(
      UnitName = Site_ID,
      IndexValue = MEAN
    ) %>%
    separate(SourceFile, into = c("IndexType", "Year", "Month"), sep = "_", remove = FALSE) %>%
    mutate(
      Year = as.numeric(Year),
      Month = as.numeric(Month),
      date = as.Date(paste(Year, Month, "01", sep = "-")),
      IndexValue = ifelse(IndexValue < -1 | IndexValue > 1, NA, IndexValue)
    ) %>%
    arrange(UnitName, date) %>%
    filter(!is.na(Year), !is.na(Month), !is.na(IndexValue))
}

# 土地类型年度数据预处理
process_class_annual <- function(data) {
  data %>%
    filter(!Value %in% c(20, 21)) %>%  # 排除water和built-up
    rename(
      UnitCode = Value,
      IndexValue = MEAN
    ) %>%
    separate(SourceFile, into = c("IndexType", "Year"), sep = "_", remove = FALSE) %>%
    mutate(
      Year = as.numeric(Year),
      Month = 1,  # 年度数据设为1月
      date = as.Date(paste(Year, Month, "01", sep = "-")),
      IndexValue = ifelse(IndexValue < -1 | IndexValue > 1, NA, IndexValue)
    ) %>%
    arrange(UnitCode, date) %>%
    filter(!is.na(Year), !is.na(IndexValue))
}

# 土地类型月度数据预处理
process_class_monthly <- function(data) {
  data %>%
    filter(!Value %in% c(20, 21)) %>%  # 排除water和built-up
    rename(
      UnitCode = Value,
      IndexValue = MEAN
    ) %>%
    separate(SourceFile, into = c("IndexType", "Year", "Month"), sep = "_", remove = FALSE) %>%
    mutate(
      Year = as.numeric(Year),
      Month = as.numeric(Month),
      date = as.Date(paste(Year, Month, "01", sep = "-")),
      IndexValue = ifelse(IndexValue < -1 | IndexValue > 1, NA, IndexValue)
    ) %>%
    arrange(UnitCode, date) %>%
    filter(!is.na(Year), !is.na(Month), !is.na(IndexValue))
}

# 创建时间序列函数
create_timeseries <- function(data, is_monthly = FALSE) {
  if(nrow(data) < 5) {
    return(NULL)
  }
  
  frequency_val <- if(is_monthly) 12 else 1
  start_year <- min(data$Year, na.rm = TRUE)
  start_period <- if(is_monthly) min(data$Month, na.rm = TRUE) else 1
  
  ts_data <- ts(data$IndexValue, 
                frequency = frequency_val, 
                start = c(start_year, start_period))
  
  return(ts_data)
}


# 3. BFAST分析函数 ----

perform_bfast_analysis <- function(ts_data, analysis_name = "", h = 0.15, is_monthly = FALSE) {
  
  if(is.null(ts_data) || length(ts_data) < 10) {
    return(data.frame(
      analysis_name = analysis_name,
      n_breaks = 0,
      break_dates = "数据不足",
      magnitudes = "无",
      directions = "无",
      model_quality = NA,
      data_completeness = 0,
      error = "时间序列太短",
      stringsAsFactors = FALSE
    ))
  }
  
  tryCatch({
    data_completeness <- sum(!is.na(ts_data)) / length(ts_data)
    season_model <- if(is_monthly) "harmonic" else "none"
    bf_result <- bfast(ts_data, 
                       h = h,
                       season = season_model,
                       max.iter = 3,
                       breaks = 3,)  
    
    if(!is.null(bf_result$output) && length(bf_result$output) > 0) {
      final_output <- bf_result$output[[length(bf_result$output)]]
      
      breakpoints <- NULL
      
      if(!is.null(final_output$bp.Vt)) {
        if(is.list(final_output$bp.Vt) && !is.null(final_output$bp.Vt$breakpoints)) {
          breakpoints <- final_output$bp.Vt$breakpoints
        } else if(is.numeric(final_output$bp.Vt) && length(final_output$bp.Vt) > 0) {
          breakpoints <- final_output$bp.Vt
        }
      }
      
      if(!is.null(breakpoints) && length(breakpoints) > 0 && !all(is.na(breakpoints))) {
        n_breaks <- length(breakpoints)
        
        ts_start <- start(ts_data)
        ts_freq <- frequency(ts_data)
        break_times <- ts_start[1] + (breakpoints - 1) / ts_freq
        
        if(!is.null(final_output$Tt)) {
          trend_component <- final_output$Tt
          magnitudes <- calculate_change_magnitudes(trend_component, breakpoints)
          directions <- ifelse(magnitudes > 0, "增加", "减少")
        } else {
          magnitudes <- rep(NA, n_breaks)
          directions <- rep("未知", n_breaks)
        }
        
        model_rmse <- NA
        if(!is.null(final_output$Nt)) {
          residuals <- final_output$Nt
          model_rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
        }
        
        return(data.frame(
          analysis_name = analysis_name,
          n_breaks = n_breaks,
          break_dates = paste(round(break_times, 2), collapse = "; "),
          magnitudes = paste(round(magnitudes, 4), collapse = "; "),
          directions = paste(directions, collapse = "; "),
          model_quality = if(is.na(model_rmse)) NA else round(model_rmse, 4),
          data_completeness = round(data_completeness, 3),
          error = "无",
          bfast_result = I(list(bf_result)),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # 没有断点
    return(data.frame(
      analysis_name = analysis_name,
      n_breaks = 0,
      break_dates = "无断点",
      magnitudes = "无",
      directions = "无",
      model_quality = NA,
      data_completeness = round(data_completeness, 3),
      error = "无",
      stringsAsFactors = FALSE
    ))
    
  }, error = function(e) {
    return(data.frame(
      analysis_name = analysis_name,
      n_breaks = 0,
      break_dates = "分析失败",
      magnitudes = "无",
      directions = "无",
      model_quality = NA,
      data_completeness = 0,
      error = as.character(e),
      bfast_result = I(list(NULL)),
      stringsAsFactors = FALSE
    ))
  })
}

# 计算变化幅度
calculate_change_magnitudes <- function(trend_component, breakpoints) {
  map_dbl(breakpoints, function(bp_idx) {
    pre_start <- max(1, bp_idx - 10)
    pre_end <- max(1, bp_idx - 1)
    post_start <- min(length(trend_component), bp_idx + 1)
    post_end <- min(length(trend_component), bp_idx + 10)
    
    pre_mean <- mean(trend_component[pre_start:pre_end], na.rm = TRUE)
    post_mean <- mean(trend_component[post_start:post_end], na.rm = TRUE)
    
    return(post_mean - pre_mean)
  })
}

# 4. 主分析流程 ----
# 读取数据
sites_annual_data_raw <- read.csv(path_4sites_annual_data, stringsAsFactors = FALSE)
sites_monthly_data_raw <- read.csv(path_4sites_monthly_data, stringsAsFactors = FALSE)
classes_annual_data_raw <- read.csv(path_12classes_annual_data, stringsAsFactors = FALSE)
classes_monthly_data_raw<- read.csv(path_12classes_monthly_data, stringsAsFactors = FALSE)

# 数据预处理
sites_annual <- process_site_annual(sites_annual_data_raw)
sites_monthly <- process_site_monthly(sites_monthly_data_raw)
classes_annual <- process_class_annual(classes_annual_data_raw)
classes_monthly <- process_class_monthly(classes_monthly_data_raw)

# 土地类型映射（12个类型）
land_class_mapping <- c(
  "1" = "Early Rewilded Forest",
  "2" = "Early Rewilded Natural Grassland", 
  "3" = "Early Rewilded Open/Wetland",
  "4" = "Early Rewilded Unstable",
  "11" = "Late Rewilded Forest",
  "12" = "Late Rewilded Natural Grassland",
  "13" = "Late Rewilded Open/Wetland",
  "31" = "Stable Original Forest",
  "32" = "Stable Natural Grassland",
  "33" = "Stable Natural Open/Wetland",
  "41" = "Stable Agriculture Grassland",
  "42" = "Stable Agriculture Crops"
)

# 再野化站点起始年份
rewilding_start_years <- c("MW" = 1993, "BB" = 2002, "OW" = 2006, "EW" = 2008)

# 5. 站点分析 ----
# 5.1 站点年度分析
site_annual_results <- sites_annual %>%
  group_by(UnitName) %>%
  group_map(~ {
    cat(paste("分析站点(年度):", .y$UnitName, "\n"))
    ts_data <- create_timeseries(.x, is_monthly = FALSE)
    if(!is.null(ts_data)) {
      perform_bfast_analysis(ts_data, paste(.y$UnitName, "年度", sep = "_"), h = 0.25, is_monthly = FALSE)
    } else {
      tibble(analysis_name = paste(.y$UnitName, "年度", sep = "_"), n_breaks = 0, 
             break_dates = "数据不足", error = "无法创建时间序列")
    }
  }) %>%
  bind_rows()

# 5.2 站点月度分析
site_monthly_results <- sites_monthly %>%
  group_by(UnitName) %>%
  group_map(~ {
    cat(paste("分析站点(月度):", .y$UnitName, "\n"))
    if(nrow(.x) > 20) {  # 月度数据需要更多点
      ts_data <- create_timeseries(.x, is_monthly = TRUE)
      if(!is.null(ts_data)) {
        perform_bfast_analysis(ts_data, paste(.y$UnitName, "月度", sep = "_"), h = 0.15, is_monthly = TRUE)
      } else {
        tibble(analysis_name = paste(.y$UnitName, "月度", sep = "_"), n_breaks = 0,
               break_dates = "数据不足", error = "无法创建时间序列")
      }
    } else {
      tibble(analysis_name = paste(.y$UnitName, "月度", sep = "_"), n_breaks = 0,
             break_dates = "数据不足", error = "数据点太少")
    }
  }) %>%
  bind_rows()

# 6. 土地类型分析 ----
# 6.1 土地类型月度分析
class_annual_results <- classes_annual %>%
  group_by(UnitCode) %>%
  group_map(~ {
    class_name <- land_class_mapping[as.character(.y$UnitCode)]
    if(is.na(class_name)) class_name <- paste("类型", .y$UnitCode)
    
    cat(paste("分析土地类型(年度):", class_name, "\n"))
    
    ts_data <- create_timeseries(.x, is_monthly = FALSE)
    if(!is.null(ts_data)) {
      perform_bfast_analysis(ts_data, paste("类型", .y$UnitCode, class_name, "年度", sep = "_"), h = 0.25, is_monthly = FALSE)
    } else {
      tibble(analysis_name = paste("类型", .y$UnitCode, class_name, "年度", sep = "_"), 
             n_breaks = 0, break_dates = "数据不足", error = "无法创建时间序列")
    }
  }) %>%
  bind_rows()

# 6.2 土地类型月度分析
class_monthly_results <- classes_monthly %>%
  group_by(UnitCode) %>%
  group_map(~ {
    class_name <- land_class_mapping[as.character(.y$UnitCode)]
    if(is.na(class_name)) class_name <- paste("类型", .y$UnitCode)
    
    cat(paste("分析土地类型(月度):", class_name, "\n"))
    
    if(nrow(.x) > 30) {  # 月度数据需要更多点
      ts_data <- create_timeseries(.x, is_monthly = TRUE)
      if(!is.null(ts_data)) {
        perform_bfast_analysis(ts_data, paste("类型", .y$UnitCode, class_name, "月度", sep = "_"), h = 0.15, is_monthly = TRUE)
      } else {
        tibble(analysis_name = paste("类型", .y$UnitCode, class_name, "月度", sep = "_"), 
               n_breaks = 0, break_dates = "数据不足", error = "无法创建时间序列")
      }
    } else {
      tibble(analysis_name = paste("类型", .y$UnitCode, class_name, "月度", sep = "_"), 
             n_breaks = 0, break_dates = "数据不足", error = "数据点太少")
    }
  }) %>%
  bind_rows()

# 7. 结果汇总 ----
all_results <- bind_rows(
  site_annual_results,
  site_monthly_results,
  class_annual_results,
  class_monthly_results
) %>%
  mutate(
    数据类型 = case_when(
      str_detect(analysis_name, "MW|BB|OW|EW") & str_detect(analysis_name, "年度") ~ "再野化站点_年度",
      str_detect(analysis_name, "MW|BB|OW|EW") & str_detect(analysis_name, "月度") ~ "再野化站点_月度",
      str_detect(analysis_name, "类型") & str_detect(analysis_name, "年度") ~ "土地类型_年度",
      str_detect(analysis_name, "类型") & str_detect(analysis_name, "月度") ~ "土地类型_月度",
      TRUE ~ "其他"
    ),
    分析单元 = analysis_name
  ) %>%
  select(分析单元, 数据类型, 断点数量 = n_breaks, 断点时间 = break_dates, 
         变化幅度 = magnitudes, 变化方向 = directions, 
         模型质量RMSE = model_quality, 数据完整性 = data_completeness, 错误信息 = error)


# 8. 结果保存和可视化 ----

# 保存汇总表格
# write.csv(summary_table, file.path(output_dir, "BFAST_断点检测汇总表.csv"), 
#           row.names = FALSE, fileEncoding = "UTF-8")

# 创建可视化
cat("正在创建可视化结果...\n")

# 1. 断点数量统计
breaks_summary <- summary_table %>%
  group_by(数据类型) %>%
  summarise(
    总分析数 = n(),
    有断点数 = sum(断点数量 > 0),
    平均断点数 = round(mean(断点数量, na.rm = TRUE), 2),
    .groups = 'drop'
  )

p1 <- ggplot(breaks_summary, aes(x = 数据类型, y = 有断点数, fill = 数据类型)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(有断点数, "/", 总分析数)), vjust = -0.3) +
  theme_minimal() +
  labs(title = "不同数据类型检测到断点的数量",
       x = "数据类型", y = "检测到断点的分析单元数",
       subtitle = "数字表示：有断点数/总分析数") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# 2. 断点时间分布
break_years <- c()
break_sources <- c()

for(i in 1:nrow(summary_table)) {
  if(summary_table$断点时间[i] != "无断点" && 
     summary_table$断点时间[i] != "数据不足" && 
     summary_table$断点时间[i] != "分析失败" &&
     !is.na(summary_table$断点时间[i])) {
    
    dates_str <- unlist(strsplit(summary_table$断点时间[i], "; "))
    years <- as.numeric(dates_str)
    valid_years <- years[!is.na(years) & years >= 1990 & years <= 2025]
    
    if(length(valid_years) > 0) {
      break_years <- c(break_years, valid_years)
      source_type <- ifelse(grepl("站点", summary_table$数据类型[i]), "再野化站点", "土地类型")
      break_sources <- c(break_sources, rep(source_type, length(valid_years)))
    }
  }
}

if(length(break_years) > 0) {
  break_df <- data.frame(year = break_years, source = break_sources)
  
  p2 <- ggplot(break_df, aes(x = year, fill = source)) +
    geom_histogram(binwidth = 1, alpha = 0.7, position = "stack") +
    theme_minimal() +
    labs(title = "断点检测的时间分布",
         x = "年份", y = "断点数量",
         fill = "数据来源") +
    scale_x_continuous(breaks = seq(1990, 2025, 5)) +
    geom_vline(xintercept = c(1993, 2002, 2006, 2008), 
               linetype = "dashed", alpha = 0.5, color = "red") +
    annotate("text", x = c(1993, 2002, 2006, 2008), y = max(table(break_years)), 
             label = c("MW", "BB", "OW", "EW"), angle = 90, vjust = 1.2, color = "red")
} else {
  p2 <- ggplot() + 
    labs(title = "没有检测到有效断点") +
    theme_minimal()
}

# 保存图表
ggsave(file.path(output_dir, "断点数量统计.png"), p1, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "断点时间分布.png"), p2, width = 12, height = 8, dpi = 300)


# =============================================
# 9. 输出结果总结 ----
cat("\n" + paste(rep("=", 60), collapse = "") + "\n")
cat("=== BFAST分析完成 ===\n")
cat(paste(rep("=", 60), collapse = "") + "\n")

cat(paste("总共分析了", length(all_results), "个时间序列\n"))
cat(paste("检测到断点的序列数:", sum(summary_table$断点数量 > 0), "\n"))
cat(paste("总断点数:", sum(summary_table$断点数量), "\n\n"))

# 按类型汇总
cat("=== 分析结果按类型汇总 ===\n")
print(breaks_summary)

cat("\n=== 再野化站点断点分析 ===\n")
site_results <- summary_table[grepl("MW|BB|OW|EW", summary_table$分析单元), ]

for(site in c("MW", "BB", "OW", "EW")) {
  site_data <- site_results[grepl(site, site_results$分析单元), ]
  if(nrow(site_data) > 0) {
    cat(paste("\n站点", site, "(再野化开始:", rewilding_start_years[site], "年):\n"))
    for(j in 1:nrow(site_data)) {
      cat(paste("  ", gsub("_", " ", site_data$数据类型[j]), 
                "- 断点数:", site_data$断点数量[j]))
      if(site_data$断点数量[j] > 0) {
        cat(paste(" | 断点时间:", site_data$断点时间[j]))
      }
      cat("\n")
    }
  }
}

cat("\n=== 土地类型断点统计 ===\n")
class_results <- summary_table[grepl("类型", summary_table$分析单元), ]
class_break_summary <- class_results %>%
  mutate(class_type = gsub(".*_(Early|Late|Stable).*", "\\1", 分析单元)) %>%
  group_by(class_type) %>%
  summarise(
    类型数 = n(),
    有断点数 = sum(断点数量 > 0),
    平均断点数 = round(mean(断点数量), 2),
    .groups = 'drop'
  )

print(class_break_summary)

# 10. BFAST可视化函数 ----
# 绘制BFAST分解结果
plot_bfast_decomposition <- function(bfast_result, title = "BFAST分解结果") {
  
  if(is.null(bfast_result) || is.null(bfast_result$output)) {
    cat("无BFAST结果可绘制\n")
    return(NULL)
  }
  
  tryCatch({
    # 获取时间序列和分解结果
    original_ts <- bfast_result$Yt
    
    if(length(bfast_result$output) > 0) {
      final_output <- bfast_result$output[[length(bfast_result$output)]]
      
      # 提取各分量
      trend <- if(!is.null(final_output$Tt)) final_output$Tt else rep(mean(original_ts, na.rm = TRUE), length(original_ts))
      seasonal <- if(!is.null(final_output$St)) final_output$St else rep(0, length(original_ts))
      remainder <- if(!is.null(final_output$Nt)) final_output$Nt else original_ts - trend - seasonal
      fitted <- trend + seasonal
      
      # 获取断点
      breakpoints <- NULL
      if(!is.null(final_output$bp.Vt)) {
        if(is.list(final_output$bp.Vt) && !is.null(final_output$bp.Vt$breakpoints)) {
          breakpoints <- final_output$bp.Vt$breakpoints
        } else if(is.numeric(final_output$bp.Vt) && length(final_output$bp.Vt) > 0) {
          breakpoints <- final_output$bp.Vt
        }
      }
      
      # 创建绘图数据
      time_points <- as.numeric(time(original_ts))
      plot_data <- data.frame(
        time = time_points,
        original = as.numeric(original_ts),
        trend = as.numeric(trend),
        seasonal = as.numeric(seasonal),
        remainder = as.numeric(remainder),
        fitted = as.numeric(fitted)
      )
      
      # 断点数据
      break_data <- NULL
      if(!is.null(breakpoints) && length(breakpoints) > 0) {
        ts_start <- start(original_ts)
        ts_freq <- frequency(original_ts)
        break_times <- ts_start[1] + (breakpoints - 1) / ts_freq
        break_data <- data.frame(
          break_time = break_times,
          break_label = paste("断点", 1:length(break_times))
        )
      }
      
      # 创建四个子图
      library(gridExtra)
      
      # 1. 原始数据 + 拟合值 + 断点
      p1 <- ggplot(plot_data, aes(x = time)) +
        geom_line(aes(y = original), color = "black", linewidth = 0.8, alpha = 0.7) +
        geom_line(aes(y = fitted), color = "blue", linewidth = 1) +
        labs(title = paste(title, "- 原始数据与拟合值"), 
             x = "年份", y = "NDVI") +
        theme_minimal()
      
      if(!is.null(break_data)) {
        p1 <- p1 + 
          geom_vline(data = break_data, aes(xintercept = break_time), 
                     color = "red", linetype = "dashed", linewidth = 1) +
          geom_text(data = break_data, 
                    aes(x = break_time, y = max(plot_data$original, na.rm = TRUE), 
                        label = round(break_time, 1)),
                    angle = 90, hjust = 1.1, vjust = -0.5, size = 3, color = "red")
      }
      
      # 2. 趋势分量
      p2 <- ggplot(plot_data, aes(x = time, y = trend)) +
        geom_line(color = "darkgreen", linewidth = 1) +
        labs(title = "趋势分量", x = "年份", y = "趋势值") +
        theme_minimal()
      
      if(!is.null(break_data)) {
        p2 <- p2 + geom_vline(data = break_data, aes(xintercept = break_time), 
                              color = "red", linetype = "dashed", alpha = 0.7)
      }
      
      # 3. 季节分量（如果存在）
      if(frequency(original_ts) > 1) {
        p3 <- ggplot(plot_data, aes(x = time, y = seasonal)) +
          geom_line(color = "orange", linewidth = 0.8) +
          labs(title = "季节分量", x = "年份", y = "季节值") +
          theme_minimal()
      } else {
        p3 <- ggplot() + 
          labs(title = "季节分量（年度数据无季节性）") + 
          theme_minimal() +
          theme(axis.title = element_blank(), axis.text = element_blank())
      }
      
      # 4. 残差分量
      p4 <- ggplot(plot_data, aes(x = time, y = remainder)) +
        geom_line(color = "gray50", linewidth = 0.6) +
        geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
        labs(title = "残差分量", x = "年份", y = "残差值") +
        theme_minimal()
      
      # 组合图表
      combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
      
      return(combined_plot)
      
    } else {
      cat("BFAST未产生有效分解结果\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("绘图过程中出错:", e$message, "\n")
    return(NULL)
  })
}

# 保存汇总表格
# write.csv(all_results, file.path(output_dir, "BFAST_断点检测汇总表.csv"), 
#           row.names = FALSE, fileEncoding = "UTF-8")

cat("正在创建可视化结果...\n")

# 1. 断点数量统计
breaks_summary <- all_results %>%
  group_by(数据类型) %>%
  summarise(
    总分析数 = n(),
    有断点数 = sum(断点数量 > 0),
    成功率 = round(有断点数 / 总分析数 * 100, 1),
    平均断点数 = round(mean(断点数量, na.rm = TRUE), 2),
    .groups = 'drop'
  )

p1 <- ggplot(breaks_summary, aes(x = 数据类型, y = 有断点数, fill = 数据类型)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(有断点数, "/", 总分析数, " (", 成功率, "%)")), 
            vjust = -0.3, size = 3) +
  theme_minimal() +
  labs(title = "BFAST断点检测结果统计",
       x = "数据类型", y = "检测到断点的分析单元数",
       subtitle = "数字表示：有断点数/总分析数 (成功率%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# 2. 断点时间分布
break_years <- all_results %>%
  filter(!断点时间 %in% c("无断点", "数据不足", "分析失败")) %>%
  separate_rows(断点时间, sep = "; ") %>%
  mutate(
    break_year = as.numeric(断点时间),
    source_type = ifelse(grepl("站点", 数据类型), "再野化站点", "土地类型")
  ) %>%
  filter(!is.na(break_year), break_year >= 1990, break_year <= 2025)

if(nrow(break_years) > 0) {
  p2 <- ggplot(break_years, aes(x = break_year, fill = source_type)) +
    geom_histogram(binwidth = 1, alpha = 0.7, position = "stack") +
    theme_minimal() +
    labs(title = "断点检测的时间分布",
         x = "年份", y = "断点数量", fill = "数据来源") +
    scale_x_continuous(breaks = seq(1990, 2025, 5)) +
    geom_vline(xintercept = c(1993, 2002, 2006, 2008), 
               linetype = "dashed", alpha = 0.7, color = "red") +
    annotate("text", x = c(1993, 2002, 2006, 2008), y = max(table(break_years$break_year)), 
             label = c("MW", "BB", "OW", "EW"), angle = 90, vjust = 1.2, color = "red", size = 3)
} else {
  p2 <- ggplot() + 
    labs(title = "没有检测到有效断点") +
    theme_minimal()
}

# 保存图表
# ggsave(file.path(output_dir, "断点数量统计.png"), p1, width = 12, height = 8, dpi = 300)
# ggsave(file.path(output_dir, "断点时间分布.png"), p2, width = 12, height = 8, dpi = 300)

# =============================================================================
# 9. 输出结果总结
# =============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("=== BFAST分析完成 ===\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat(paste("总共分析了", nrow(all_results), "个时间序列\n"))
cat(paste("检测到断点的序列数:", sum(all_results$断点数量 > 0), "\n"))
cat(paste("总断点数:", sum(all_results$断点数量), "\n\n"))

# 按类型汇总
cat("=== 分析结果按类型汇总 ===\n")
print(breaks_summary)

# 再野化站点结果
cat("\n=== 再野化站点断点分析 ===\n")
site_results <- all_results %>%
  filter(grepl("MW|BB|OW|EW", 分析单元))

for(site in c("MW", "BB", "OW", "EW")) {
  site_data <- site_results %>% filter(grepl(site, 分析单元))
  if(nrow(site_data) > 0) {
    cat(paste("\n站点", site, "(再野化开始:", rewilding_start_years[site], "年):\n"))
    for(i in 1:nrow(site_data)) {
      cat(paste("  ", site_data$数据类型[i], "- 断点数:", site_data$断点数量[i]))
      if(site_data$断点数量[i] > 0) {
        cat(paste(" | 断点时间:", site_data$断点时间[i]))
      }
      cat("\n")
    }
  }
}

# 土地类型结果汇总
cat("\n=== 土地类型断点统计 ===\n")
class_results <- all_results %>%
  filter(grepl("类型", 分析单元)) %>%
  mutate(
    class_category = case_when(
      grepl("Early", 分析单元) ~ "Early Rewilded",
      grepl("Late", 分析单元) ~ "Late Rewilded", 
      grepl("Stable", 分析单元) ~ "Stable",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(class_category) %>%
  summarise(
    类型数 = n(),
    有断点数 = sum(断点数量 > 0),
    平均断点数 = round(mean(断点数量), 2),
    .groups = 'drop'
  )

print(class_results)

cat(paste("\n结果文件已保存到:", output_dir, "\n"))
cat("包含文件:\n")
cat("- BFAST_断点检测汇总表.csv (详细结果表格)\n")
cat("- 断点数量统计.png (按数据类型统计)\n") 
cat("- 断点时间分布.png (断点的时间分布，含再野化起始年份标注)\n")

# =============================================================================
# 11. 创建BFAST分解可视化
# =============================================================================

cat("\n正在创建BFAST分解可视化...\n")

# 选择要可视化的案例
# 1. 有断点的案例优先
cases_with_breaks <- all_results[all_results$断点数量 > 0, ]

if(nrow(cases_with_breaks) > 0) {
  cat("找到有断点的案例，正在绘制前3个...\n")
  n_plots <- min(3, nrow(cases_with_breaks))
  
  for(i in 1:n_plots) {
    case_name <- cases_with_breaks$分析单元[i]
    bfast_result <- cases_with_breaks$bfast_result[[i]]
    
    if(!is.null(bfast_result)) {
      cat(paste("绘制案例", i, ":", case_name, "\n"))
      
      # 创建图表
      plot_result <- plot_bfast_decomposition(bfast_result, case_name)
      
      # 保存图表
      if(!is.null(plot_result)) {
        filename <- paste0("BFAST分解_", gsub("[^A-Za-z0-9_]", "_", case_name), ".png")
        ggsave(file.path(output_dir, filename), plot_result, 
               width = 14, height = 10, dpi = 300)
        cat(paste("  保存为:", filename, "\n"))
      }
    }
  }
} else {
  cat("没有检测到断点的案例，绘制代表性案例...\n")
  
  # 选择代表性案例：每种数据类型选一个
  representative_cases <- all_results %>%
    group_by(数据类型) %>%
    slice(1) %>%
    ungroup()
  
  for(i in 1:min(4, nrow(representative_cases))) {
    case_name <- representative_cases$分析单元[i]
    bfast_result <- representative_cases$bfast_result[[i]]
    
    if(!is.null(bfast_result)) {
      cat(paste("绘制代表性案例:", case_name, "\n"))
      
      plot_result <- plot_bfast_decomposition(bfast_result, case_name)
      
      if(!is.null(plot_result)) {
        filename <- paste0("BFAST分解_", gsub("[^A-Za-z0-9_]", "_", case_name), ".png")
        ggsave(file.path(output_dir, filename), plot_result, 
               width = 14, height = 10, dpi = 300)
        cat(paste("  保存为:", filename, "\n"))
      }
    }
  }
}

# 创建一个整体的参数敏感性分析图
cat("\n创建参数敏感性分析...\n")

# 为一个代表性案例尝试不同的h参数
if(nrow(all_results) > 0) {
  # 选择第一个站点年度数据进行参数测试
  test_site <- sites_annual %>% filter(UnitName == unique(sites_annual$UnitName)[1])
  ts_data <- create_timeseries(test_site, is_monthly = FALSE)
  
  if(!is.null(ts_data)) {
    h_values <- c(0.05, 0.10, 0.15, 0.20, 0.25)
    param_results <- data.frame()
    
    for(h_val in h_values) {
      tryCatch({
        bf_result <- bfast(ts_data, h = h_val, season = "none", max.iter = 3, breaks = 5)
        
        n_breaks <- 0
        if(!is.null(bf_result$output) && length(bf_result$output) > 0) {
          final_output <- bf_result$output[[length(bf_result$output)]]
          if(!is.null(final_output$bp.Vt)) {
            if(is.list(final_output$bp.Vt) && !is.null(final_output$bp.Vt$breakpoints)) {
              n_breaks <- length(final_output$bp.Vt$breakpoints)
            } else if(is.numeric(final_output$bp.Vt) && length(final_output$bp.Vt) > 0) {
              n_breaks <- length(final_output$bp.Vt)
            }
          }
        }
        
        param_results <- rbind(param_results, data.frame(h = h_val, n_breaks = n_breaks))
      }, error = function(e) {
        param_results <- rbind(param_results, data.frame(h = h_val, n_breaks = 0))
      })
    }
    
    # 绘制参数敏感性图
    p_param <- ggplot(param_results, aes(x = h, y = n_breaks)) +
      geom_line(linewidth = 1, color = "blue") +
      geom_point(size = 3, color = "red") +
      labs(title = paste("BFAST参数敏感性分析 -", unique(test_site$UnitName)[1]),
           x = "h 参数值 (最小段长度比例)", 
           y = "检测到的断点数量",
           subtitle = "显示不同h参数对断点检测结果的影响") +
      theme_minimal() +
      scale_x_continuous(breaks = h_values)
    
    ggsave(file.path(output_dir, "BFAST参数敏感性分析.png"), p_param, 
           width = 10, height = 6, dpi = 300)
    
    cat("参数敏感性分析图已保存\n")
  }
}

cat("\n=== 可视化完成 ===\n")
cat("可视化文件已保存到结果目录\n")

cat("\n分析完成！\n")









# 检查脚本 - 插入到数据预处理后
cat("=== 数据质量检查 ===\n")

# 检查站点数据
for(site in unique(sites_annual$UnitName)) {
  site_data <- sites_annual[sites_annual$UnitName == site, ]
  cat(paste("\n站点", site, ":\n"))
  cat(paste("- 数据点数:", nrow(site_data), "\n"))
  cat(paste("- 时间跨度:", min(site_data$Year), "-", max(site_data$Year), "\n"))
  cat(paste("- NDVI范围:", round(min(site_data$IndexValue, na.rm = TRUE), 3), 
            "-", round(max(site_data$IndexValue, na.rm = TRUE), 3), "\n"))
  cat(paste("- NDVI标准差:", round(sd(site_data$IndexValue, na.rm = TRUE), 4), "\n"))
  
  # 检查时间序列
  ts_data <- create_timeseries(site_data, is_monthly = FALSE)
  if(!is.null(ts_data)) {
    cat(paste("- 时间序列长度:", length(ts_data), "\n"))
    cat(paste("- 时间序列范围:", round(min(ts_data, na.rm = TRUE), 3), 
              "-", round(max(ts_data, na.rm = TRUE), 3), "\n"))
    
    # 简单可视化检查
    plot(ts_data, main = paste("站点", site, "NDVI时间序列"), 
         ylab = "NDVI", xlab = "年份")
    
    # 检查是否有明显趋势
    if(length(ts_data) > 10) {
      trend_test <- cor.test(1:length(ts_data), as.numeric(ts_data))
      cat(paste("- 线性趋势相关系数:", round(trend_test$estimate, 4), 
                "p值:", round(trend_test$p.value, 4), "\n"))
    }
  } else {
    cat("- 无法创建时间序列!\n")
  }
}



