library(tidyverse)
library(trend)
library(nlme)
library(tidytext)

# 1. Prepare the data ----
path_9class_data <- read_csv("E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_13Class_Annual.csv")
path_4sites_data <- read_csv("E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_4Sites_Annual.csv")

# For 9 classes data
annual_data_9class <- path_9class_data %>%
  separate(SourceFile, into = c("IndexType", "Year"), sep = "_") %>%
  rename(IndexValue = MEAN) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(
    ClassName = case_when(
      # Early Rewilding
      Value == 1  ~ "Early Rewilded Forest",
      Value == 2  ~ "Early Rewilded Natural Grassland",
      Value == 3  ~ "Early Rewilded Open/Wetland",
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
annual_data_combined <- bind_rows(annual_data_9class, annual_data_4sites) %>%
  mutate(
    CategoryGroup = case_when(
      str_detect(ClassName, "Rewilded") ~ "Rewilded",
      str_detect(ClassName, "Stable Agriculture") ~ "Stable Agriculture",
      str_detect(ClassName, "Stable Natural|Stable Original") ~ "Stable Natural",
      TRUE ~ "Specific Site" 
    )
  )

  
# 2. Perform the Mann-Kendall trend test for each class ----
trend_results_all <- annual_data_combined %>%
  filter(!ClassName %in% c("Water", "Built-up", "Unknown")) %>%
  group_by(ClassName, AnalysisType, CategoryGroup, IndexType) %>%
  summarise(
    slope_result = list(sens.slope(IndexValue, conf.level = 0.95)),
    .groups = 'drop'
  ) %>%
  mutate(
    Slope = map_dbl(slope_result, ~ .x$estimates),
    Lower_CI = map_dbl(slope_result, ~ .x$conf.int[1]),
    Upper_CI = map_dbl(slope_result, ~ .x$conf.int[2])
  ) %>%
  mutate(
    Significance = if_else(Lower_CI > 0 | Upper_CI < 0, "Significant (p < 0.05)", "Not Significant")
  ) %>%
  select(AnalysisType, CategoryGroup, ClassName, IndexType, Slope, Lower_CI, Upper_CI, Significance)

print(trend_results_all %>% arrange(IndexType, desc(Slope)))


# 3. Visualization ----
#### 3.1 time series ####
class_colors_classes <- c(
  "Early Rewilded Forest" = "#006d2c",
  "Early Rewilded Natural Grassland" = "#41ab5d",
  "Early Rewilded Open/Wetland" = "#a1d99b",
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

# plot for the 9 classes
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


#### 3.2 Sen's Slope Comparison Bar Chart ####
# Create the bar chart with facets for each category group
ggplot(data = trend_results_all, aes(x = reorder(ClassName, Slope), y = Slope, fill = Significance)) +
  geom_col() +
  coord_flip() +
  facet_grid(CategoryGroup ~ IndexType, scales = "free_y") +
  scale_fill_manual(values = c("Significant (p < 0.05)" = "steelblue", "Not Significant" = "grey80")) +
  labs(
    title = "Comparison of Long-Term Trend Rates (Sen's Slope)",
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

#### 3.3 Synthesis Plot (for better demo if bar plot is not clear enough) ####
trend_results_all_reordered <- trend_results_all %>%
  mutate(ClassNameOrdered = reorder_within(ClassName, Slope, list(CategoryGroup, IndexType)))

ggplot(data = trend_results_all_reordered, 
       aes(x = Slope, y = ClassNameOrdered, color = AnalysisType)) +
  geom_pointrange(aes(xmin = Lower_CI, xmax = Upper_CI), size = 0.8, fatten = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  facet_grid(CategoryGroup ~ IndexType, scales = "free_y", space = "free_y") +
  scale_y_reordered() +
  scale_color_manual(values = c("Regional Class" = "navy", "Specific Site" = "firebrick")) +
  labs(
    title = "Synthesis: Comparing Trend Slopes of Regional Classes and Specific Sites",
    subtitle = "Points represent the Sen's Slope estimate; lines represent the 95% confidence interval.",
    x = "Trend Slope (Annual Rate of Index Change)",
    y = "Land Cover Class / Rewilding Site",
    color = "Analysis Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 9)
  )

#### 3.4 Variability Comparison Plot ####
# For 3 general class
ggplot(data = annual_data_combined %>% 
         filter(CategoryGroup %in% c("Rewilded", "Stable Agriculture", "Stable Natural")),
       aes(x = CategoryGroup, y = IndexValue, fill = CategoryGroup)) +
  geom_boxplot() +
  facet_wrap(~ IndexType, scales = "free_y") +
  labs(
    title = "Variability of NDVI across Major Land Cover Groups",
    # subtitle = "Boxplots show the distribution of all annual values from 1993-2024",
    x = "Logical Group",
    y = "Annual Growing Season Mean Index Value",
    fill = "Logical Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# For 4 specific sites
ggplot(data = annual_data_combined %>% 
         filter(ClassName %in% c("MW", "BB", "OW", "EW")),
       aes(x = ClassName, y = IndexValue, fill = ClassName)) +
  geom_boxplot() +
  facet_wrap(~ IndexType, scales = "free_y") +
  labs(
    title = "Variability of NDVI/NIRv across Specific Rewilding Sites",
    # subtitle = "Boxplots show the distribution of all annual values from 1993-2024 for each site",
    x = "Specific Site", 
    y = "Annual Growing Season Mean Index Value",
    fill = "Site Name" 
  ) +
  theme_minimal() +
  theme(
    legend.position = "none" 
  )



