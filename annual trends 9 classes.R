library(tidyverse)
library(trend)

# 1. Prepare the data ----
annual_data <- read_csv("E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStats_9Class_Annual.csv")
head(annual_data)

annual_data_final <- annual_data %>%
  mutate(
    ClassName = case_when(
      Value == 1 ~ "Rewilded Forest",
      Value == 2 ~ "Rewilded Natural Grassland",
      Value == 3 ~ "Rewilded Open/Wetland",
      Value == 4 ~ "Stable Agriculture Grassland",
      Value == 5 ~ "Stable Agriculture Crops",
      Value == 6 ~ "Stable Original Forest",
      Value == 7 ~ "Stable Original Open/Grass/Wetland",
      Value == 8 ~ "Water",
      Value == 9 ~ "Built-up",
      TRUE ~ "Unknown" 
    )
  ) 

# Convert the data to long format for easier analysis
annual_data_long <- annual_data_final %>%
    pivot_longer(
    cols = starts_with("NDVI_") | starts_with("NIRv_"),
    names_to = c("IndexType", "Year"),
    names_sep = "_",
    values_to = "IndexValue"             
  ) %>%
  
  mutate(Year = as.numeric(Year) ) %>%
  select(Value, ClassName, IndexType, Year, IndexValue)

print(head(annual_data_long))
  
# 2. Perform the Mann-Kendall trend test for each class ----
trend_results <- annual_data_long %>%
  filter(!ClassName %in% c("Water", "Built-up")) %>%
  group_by(Value, ClassName,IndexType) %>%
  summarise(
    p_value = mk.test(IndexValue)$p.value,
    sen_slope = sens.slope(IndexValue)$estimates,
    .groups = 'drop' 
  ) %>%
  mutate(
    Significance = if_else(p_value < 0.05, "Significant (p < 0.05)", "Not Significant")
  )

print(trend_results %>% arrange(IndexType, desc(sen_slope)))


# 3. Visualization ----
#### 3.1 time series ####
class_colors <- c(
  "Rewilded Forest" = "#1a9850", 
  "Rewilded Natural Grassland" = "#91cf60", 
  "Rewilded Open/Wetland" = "#d9ef8b", 
  "Stable Agriculture Grassland" = "#fee08b", 
  "Stable Agriculture Crops" = "#fdae61", 
  "Stable Original Forest" = "#2c7bb6",
  "Stable Original Open/Grass/Wetland" = "#abd9e9"
)

# Determine the min and max year for axis scaling
min_year <- min(annual_data_long$Year)
max_year <- max(annual_data_long$Year)


# Create the time series plot
ggplot(data = annual_data_long %>% filter(!ClassName %in% c("Water", "Built-up")), 
       aes(x = Year, y = IndexValue, color = ClassName, group = ClassName)) +
  geom_line(alpha = 0.8, size = 1) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.7, color = "grey20") + 
  
  # Use facet_wrap to create separate panels for NDVI and NIRv
  facet_wrap(~ IndexType, scales = "free_y", ncol = 1) +
  
  scale_color_manual(values = class_colors) + 
  scale_x_continuous(breaks = seq(min_year, max_year, by = 2)) + 
  
  labs(
    title = "Annual Trend for Different Land Cover Trajectories (1993-2024)", # A clear main title
    subtitle = "Comparing NDVI and NIRv Trends",
    x = "Year",
    y = "Annual Growing Season Mean Index Value",
    color = "Land Cover Class" 
  ) +
  
  theme_bw() + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
    strip.text = element_text(face = "bold", size = 12) 
  )

#### 3.2 Sen's Slope Comparison Bar Chart ####
# Grouping the classes for better visualization
trend_results_grouped <- trend_results %>%
  mutate(
    CategoryGroup = case_when(
      str_detect(ClassName, "Rewilded") ~ "A: Rewilded",
      str_detect(ClassName, "Stable Agriculture") ~ "B: Stable Agriculture",
      str_detect(ClassName, "Stable Original") ~ "C: Stable Natural",
      TRUE ~ "Other"
    )
  )

# Create the bar chart with facets for each category group
ggplot(data = trend_results_grouped, aes(x = reorder(ClassName, sen_slope), y = sen_slope, fill = Significance)) +
  geom_col() +
  coord_flip() +
  facet_grid(CategoryGroup ~ IndexType, scales = "free_y") +
  scale_fill_manual(values = c("Significant (p < 0.05)" = "steelblue", "Not Significant" = "grey80")) +
  labs(
    # title = "Comparison of Long-Term Trend Rates (Sen's Slope)",
    # subtitle = "Comparing NDVI and NIRv results across logical groups",
    x = "Individual Class Name",
    y = "Sen's Slope (Annual Rate of Index Change)",
    fill = "Significance"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    panel.spacing = unit(1.5, "lines"),
    axis.text.y = element_text(size = 8)
  )

