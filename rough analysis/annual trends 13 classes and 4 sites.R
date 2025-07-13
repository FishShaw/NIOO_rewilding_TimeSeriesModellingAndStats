library(tidyverse)
library(trend)
library(nlme)
library(tidytext)
library(ggplot2)

# Annual data extraction and combination ----
# 1. Prepare the data ----
path_12class_data <- read_csv("E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_14Class_Annual.csv")
path_4sites_data <- read_csv("E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_4Sites_Annual.csv")

# For 12 classes data
annual_data_12class <- path_12class_data %>%
  separate(SourceFile, into = c("IndexType", "Year"), sep = "_") %>%
  rename(IndexValue = MEAN) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(
    ClassName = case_when(
      # Early Rewilding
      Value == 1  ~ "Early Rewilded Forest",
      Value == 2  ~ "Early Rewilded Natural Grassland",
      Value == 3  ~ "Early Rewilded Open/Wetland",
      Value == 4  ~ "Early Rewilded Unstable",
      # Late Rewilding
      Value == 11 ~ "Late Rewilded Forest",
      Value == 12 ~ "Late Rewilded Natural Grassland",
      Value == 13 ~ "Late Rewilded Open/Wetland",
      # Stable Natural
      Value == 31 ~ "Stable Original Forest",
      Value == 32 ~ "Stable Natural Grassland",
      Value == 33 ~ "Stable Natural Open/Wetland",
      # Stable Agriculture
      Value == 41 ~ "Stable Agriculture Grassland",
      Value == 42 ~ "Stable Agriculture Crops",
      # Other
      Value == 20 ~ "Water",
      Value == 21 ~ "Built-up",
      TRUE ~ "Unknown" 
    )
  ) %>%
  mutate(AnalysisType = "Regional Class") %>% 
  select(ClassName, AnalysisType, IndexType, Year, IndexValue)
  
# For 4 sites data
annual_data_4sites <- path_4sites_data %>%
  separate(SourceFile, into = c("IndexType", "Year"), sep = "_", remove = FALSE) %>%
  rename(
    ClassName = Site_ID,      
    IndexValue = MEAN         
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(AnalysisType = "Specific Site") %>%
  select(AnalysisType, ClassName, IndexType, Year, IndexValue)

print(head(annual_data_4sites))


# Combine both datasets
annual_data_combined <- bind_rows(annual_data_12class, annual_data_4sites) %>%
  mutate(
    CategoryGroup = case_when(
      str_detect(ClassName, "Rewilded") ~ "Rewilded",
      str_detect(ClassName, "Stable Agriculture") ~ "Stable Agriculture",
      str_detect(ClassName, "Stable Natural|Stable Original") ~ "Stable Natural",
      TRUE ~ "Specific Site" 
    )
  )


# # Monthly data extraction and combination ----
# # 1. Prepare the data ----
# path_12class_data <- read_csv("E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_14Class_Monthly.csv")
# path_4sites_data <- read_csv("E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_4Sites_Monthly.csv")
# 
# # For 12 classes data
# annual_data_12class <- path_12class_data %>%
#   separate(SourceFile, into = c("IndexType", "Year", "Month"), sep = "_") %>%
#   rename(IndexValue = MEAN) %>%
#   mutate(Year = as.numeric(Year)) %>%
#   mutate(Month = as.numeric(Month)) %>%
#   mutate(
#     ClassName = case_when(
#       # Early Rewilding
#       Value == 1  ~ "Early Rewilded Forest",
#       Value == 2  ~ "Early Rewilded Natural Grassland",
#       Value == 3  ~ "Early Rewilded Open/Wetland",
#       Value == 4  ~ "Early Rewilded Unstable",
#       # Late Rewilding
#       Value == 11 ~ "Late Rewilded Forest",
#       Value == 12 ~ "Late Rewilded Natural Grassland",
#       Value == 13 ~ "Late Rewilded Open/Wetland",
#       # Stable Natural
#       Value == 31 ~ "Stable Original Forest",
#       Value == 32 ~ "Stable Natural Grassland",
#       Value == 33 ~ "Stable Natural Open/Wetland",
#       # Stable Agriculture
#       Value == 41 ~ "Stable Agriculture Grassland",
#       Value == 42 ~ "Stable Agriculture Crops",
#       # Other
#       Value == 20 ~ "Water",
#       Value == 21 ~ "Built-up",
#       TRUE ~ "Unknown"
#     )
#   ) %>%
#   mutate(AnalysisType = "Regional Class") %>%
#   select(ClassName, AnalysisType, IndexType, Year, Month, IndexValue)
# 
# # For 4 sites data
# annual_data_4sites <- path_4sites_data %>%
#   separate(SourceFile, into = c("IndexType", "Year", "Month"), sep = "_", remove = FALSE) %>%
#   rename(
#     ClassName = Site_ID,
#     IndexValue = MEAN
#   ) %>%
#   mutate(Year = as.numeric(Year)) %>%
#   mutate(Month = as.numeric(Month)) %>%
#   mutate(AnalysisType = "Specific Site") %>%
#   select(AnalysisType, ClassName, IndexType, Year, Month, IndexValue)
# 
# print(head(annual_data_4sites))
# 
# # Combine both datasets
# annual_data_combined <- bind_rows(annual_data_12class, annual_data_4sites) %>%
# filter(!ClassName %in% c("Water", "Built-up", "Unknown"))%>%
#   mutate(
#      CategoryGroup = case_when(
#        str_detect(ClassName, "Rewilded") ~ "Rewilded",
#        str_detect(ClassName, "Stable Agriculture") ~ "Stable Agriculture",
#        str_detect(ClassName, "Stable Natural|Stable Original") ~ "Stable Natural",
#        TRUE ~ "Specific Site"
#      )
#    )
# 
# write.csv(annual_data_combined, "E:/WUR_Intern/RewildingProject_RawData/NDVI/monthly_data_combined.csv", row.names = FALSE)


  
# 2. Calculate all the 4 metrics ----
#### Index1 & 2: Sen's Slope and average NDVI level ####
metrics_part1 <- annual_data_combined %>%
  filter(!ClassName %in% c("Water", "Built-up", "Unknown")) %>%
  group_by(ClassName, AnalysisType, CategoryGroup, IndexType) %>%
  summarise(
    slope_result = list(sens.slope(IndexValue, conf.level = 0.95)),
    Mean_Value = mean(IndexValue, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  mutate(
    Slope = map_dbl(slope_result, ~ .x$estimates),
    Lower_CI = map_dbl(slope_result, ~ .x$conf.int[1]),
    Upper_CI = map_dbl(slope_result, ~ .x$conf.int[2])
  ) %>%
  select(AnalysisType, CategoryGroup, ClassName, IndexType, Slope, Lower_CI, Upper_CI, Mean_Value)

#### Index3: Absolute Increase ####
metrics_part2 <- annual_data_combined %>%
  filter(!ClassName %in% c("Water", "Built-up", "Unknown")) %>%
  filter(Year == min(Year) | Year == max(Year)) %>%
  pivot_wider(names_from = Year, values_from = IndexValue) %>%
  mutate(
    Absolute_Increase = .[[as.character(max(annual_data_combined$Year))]] - .[[as.character(min(annual_data_combined$Year))]]
  ) %>%
  select(ClassName, IndexType, Absolute_Increase)

#### Index4: Coefficient of Variation - Stability ####
metrics_part3 <- annual_data_combined %>%
  filter(!ClassName %in% c("Water", "Built-up", "Unknown")) %>%
  group_by(ClassName, IndexType) %>%
  summarise(
    model_residuals = list(lm(IndexValue ~ Year)$residuals),
    mean_of_values = mean(IndexValue, na.rm = TRUE),
    sd_of_residuals = sd(unlist(model_residuals), na.rm = TRUE),
    CV_Stability = (sd_of_residuals / mean_of_values) * 100,
    .groups = 'drop'
  ) %>%
  select(ClassName, IndexType, CV_Stability)

# mean statistics
mean_statistics <- annual_data_combined %>%
  filter(!ClassName %in% c("Water", "Built-up", "Unknown")) %>%
  group_by(ClassName, IndexType) %>%
  summarise(
    Mean_SE = sd(IndexValue, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )


#### Combine all metrics into a final table ####
final_metrics_table <- metrics_part1 %>%
  left_join(metrics_part2, by = c("ClassName", "IndexType")) %>%
  left_join(metrics_part3, by = c("ClassName", "IndexType")) %>%
  left_join(mean_statistics, by = c("ClassName", "IndexType"))

print(final_metrics_table)



# 3. Visualization ----
#### 3.1 time series ####
class_colors_classes <- c(
  "Early Rewilded Forest" = "#006d2c",
  "Early Rewilded Natural Grassland" = "#41ab5d",
  "Early Rewilded Open/Wetland" = "#a1d99b",
  "Early Rewilded Unstable" = "#ac61a4",
  "Late Rewilded Forest" = "#8c2d04",
  "Late Rewilded Natural Grassland" = "#d94801",
  "Late Rewilded Open/Wetland" = "#fec44f",
  "Stable Agriculture Grassland" = "#fee08b",
  "Stable Agriculture Crops" = "#fdae61",
  "Stable Original Forest" = "#2b8cbe",
  "Stable Natural Grassland" = "#41b6c4",
  "Stable Natural Open/Wetland" = "#a1dab4"
)

class_color_sites <- c(
  "MW" = "#d95f02",
  "BB" = "#7570b3",
  "OW" = "#1b9e77",
  "EW" = "#e7298a"
)

# Determine the min and max year for axis scaling
min_year <- min(annual_data_combined$Year)
max_year <- max(annual_data_combined$Year)

# plot for the 12 classes
ggplot(data = annual_data_combined %>% filter(!ClassName %in% c("Water", "Built-up", "Unknown","MW", "BB", "OW", "EW")),
       aes(x = Year, y = IndexValue, color = ClassName, group = ClassName)) +
  geom_line(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.5, color = "grey20") +
  facet_wrap(~ IndexType, scales = "free_y", ncol = 1) +
  scale_color_manual(values = class_colors_classes) +
  scale_x_continuous(breaks = seq(min_year, max_year, by = 4)) +
  labs(
    title = "Annual Trend for Land Cover Trajectories (1993-2024)",
    x = "Year",
    y = "Annual Growing Season Mean Index Value",
    color = "Advanced Class Name"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# plot for the 4 sites
ggplot(data = annual_data_combined %>% filter(ClassName %in% c("MW", "BB", "OW", "EW")),
       aes(x = Year, y = IndexValue, color = ClassName, group = ClassName)) +
  geom_line(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.5, color = "grey20") +
  facet_wrap(~ IndexType, scales = "free_y", ncol = 1) +
  scale_color_manual(values = class_color_sites) +
  scale_x_continuous(breaks = seq(min_year, max_year, by = 4)) +
  labs(
    title = "Annual Trend for Specific Rewilding Sites (1993-2024)",
    x = "Year",
    y = "Annual Growing Season Mean Index Value",
    color = "Site Name"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


#### 3.2 The Average State ####
ggplot(data = final_metrics_table, aes(x = reorder(ClassName, Mean_Value), y = Mean_Value, fill = CategoryGroup)) +
  geom_col() + 
  geom_errorbar(
    aes(ymin = Mean_Value - Mean_SE, ymax = Mean_Value + Mean_SE),
    width = 0.4, 
    color = "grey30", 
    size = 0.5 
  ) +
  coord_flip() +
  facet_wrap(~ IndexType, scales = "free_x") +
  labs(
    title = " Average NDVI (1993-2024)",
    x = "Class Name / Site",
    y = "Mean Annual NDVI ± SE",
    fill = "Category Group"
  ) +
  theme_minimal()


#### 3.3 The Rate of Change - Synthesis Plot ####
ggplot(data = final_metrics_table, aes(x = Slope, y = reorder(ClassName, Slope), color = AnalysisType)) +
  geom_pointrange(aes(xmin = Lower_CI, xmax = Upper_CI)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  facet_grid(CategoryGroup ~ IndexType, scales = "free_y", space = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Rate of Change (Sen Slope)",
    x = "Trend Slope (Annual Rate of Index Change)",
    y = "", color = "Analysis Type"
  ) +
  theme_minimal() + theme(legend.position = "top")


#### 3.4 The Magnitude of Change ####
ggplot(data = final_metrics_table, aes(x = reorder(ClassName, Absolute_Increase), y = Absolute_Increase, fill = CategoryGroup)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ IndexType, scales = "free_x") +
  labs(
    title = "Absolute NDVI Increase (1993-2024)",
    x = "Class Name / Site",
    y = "Absolute Increase in NDVI",
    fill = "Category Group"
  ) +
  theme_minimal()

#### 3.5 The Stability of Change ####
ggplot(data = final_metrics_table, aes(x = reorder(ClassName, -CV_Stability), y = CV_Stability, fill = CategoryGroup)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ IndexType, scales = "free_x") +
  labs(
    title = "Stability (Coefficient of Variation)",
    subtitle = "Lower CV indicates higher stability (less inter-annual variability)",
    x = "Class Name / Site",
    y = "Coefficient of Variation (%)",
    fill = "Category Group"
  ) +
  theme_minimal()

# 4. Significance test ----
#### 4.1 Sen's Slope ####
slope_significance <- annual_data_combined %>%
  filter(!ClassName %in% c("Water", "Built-up", "Unknown")) %>%
  group_by(ClassName, AnalysisType, CategoryGroup, IndexType) %>%
  summarise(
    slope_test = list(sens.slope(IndexValue, conf.level = 0.95)),
    .groups = 'drop'
  ) %>%
  mutate(
    Slope_p_value = map_dbl(slope_test, ~ .x$p.value)
  ) %>%
  select(ClassName, IndexType, Slope_p_value)

#### 4.2 std error ####
mean_statistics <- annual_data_combined %>%
  filter(!ClassName %in% c("Water", "Built-up", "Unknown")) %>%
  group_by(ClassName, IndexType) %>%
  summarise(
    Mean_SE = sd(IndexValue, na.rm = TRUE) / sqrt(n()),
    n_years = n(),
    .groups = 'drop'
  )

#### 4.3 merge all the stats ####
summary_table <- final_metrics_table %>%
  left_join(slope_significance, by = c("ClassName", "IndexType")) %>%
  left_join(mean_statistics, by = c("ClassName", "IndexType")) %>%
  mutate(
    Slope_sig = case_when(
      Slope_p_value < 0.001 ~ "***",
      Slope_p_value < 0.01 ~ "**",
      Slope_p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    Mean_Value_text = sprintf("%.3f ± %.3f", Mean_Value, Mean_SE),
    Slope_text = sprintf("%.4f (%.4f, %.4f)%s", Slope, Lower_CI, Upper_CI, Slope_sig),
    Absolute_Increase_text = sprintf("%.3f", Absolute_Increase),
    CV_Stability_text = sprintf("%.2f%%", CV_Stability)
  ) 

#### 4.4 create tables ####
ndvi_summary <- summary_table %>%
  filter(IndexType == "NDVI") %>%
  select(
    AnalysisType,
    CategoryGroup,
    ClassName,
    Mean_Value_text,
    Slope_text,
    Absolute_Increase_text,
    CV_Stability_text,
    Slope_p_value,
    Mean_Value,  
    Slope,       
    Absolute_Increase,  
    CV_Stability  
  )

print("=== Summary Table for NDVI (1993-2024) ===")
print(ndvi_summary)

ndvi_display_table <- ndvi_summary %>%
  select(
    `Analysis Type` = AnalysisType,
    `Category` = CategoryGroup,
    `Class/Site Name` = ClassName,
    `Mean NDVI ± SE` = Mean_Value_text,
    `Trend Slope (95% CI)` = Slope_text,
    `Absolute Increase` = Absolute_Increase_text,
    `CV (%)` = CV_Stability_text
  )
print(ndvi_display_table)

# csv
# write.csv(ndvi_display_table, "NDVI_summary_table_with_significance.csv", row.names = FALSE)

