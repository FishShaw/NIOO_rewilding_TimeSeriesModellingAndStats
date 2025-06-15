library(tidyverse)
library(trend)
library(nlme)
library(tidytext)

path_9class_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStats_9Class_Annual.csv"
path_4sites_data <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStats_4Sites_Annual.csv"

# 1. Data Extraction & Calculation ----
#=========================================================
#### 1.1 extract outcome from 9 classes ####
data_9class_long <- read_csv(path_9class_data) %>%
  mutate(ClassName = case_when(Value == 1 ~ "Rewilded Forest", Value == 2 ~ "Rewilded Natural Grassland", Value == 3 ~ "Rewilded Open/Wetland", Value == 4 ~ "Stable Agriculture Grassland", Value == 5 ~ "Stable Agriculture Crops", Value == 6 ~ "Stable Original Forest", Value == 7 ~ "Stable Original Open/Grass/Wetland", TRUE ~ "Other")) %>%
  pivot_longer(cols = starts_with("NDVI_"), names_to = "Year_Raw", values_to = "NDVI") %>%
  mutate(Year = as.numeric(str_extract(Year_Raw, "\\d{4}"))) %>%
  select(ClassName, Year, NDVI)

results_9class <- data_9class_long %>%
  filter(!ClassName %in% c("Other")) %>%
  group_by(ClassName) %>%
  summarise(
    slope_result = list(sens.slope(NDVI, conf.level = 0.95)),
    .groups = 'drop'
  ) %>%
  mutate(
    AnalysisType = "Regional Average",
    Slope = map_dbl(slope_result, ~ .x$estimates),
    Lower_CI = map_dbl(slope_result, ~ .x$conf.int[1]),
    Upper_CI = map_dbl(slope_result, ~ .x$conf.int[2])
  ) %>%
  select(AnalysisType, ClassName, Slope, Lower_CI, Upper_CI)

print(results_9class)

#### 1.2 extract slope from 4 sites ####

intervention_years <- tibble(SiteName = c("MW", "BB", "OW", "EW"), StartYear = c(1993, 2002, 2006, 2008))
sites_itsa_data <- read_csv(path_4sites_data) %>%
  rename(SiteName = names(.)[1]) %>%
  pivot_longer(cols = -SiteName, names_to = c("IndexType", "Year"), names_sep = "_", values_to = "IndexValue") %>%
  filter(IndexType == "NDVI") %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(intervention_years, by = "SiteName") %>%
  mutate(Time = Year - 1993 + 1, Intervention = if_else(Year >= StartYear, 1, 0), Post_Intervention_Time = if_else(Intervention == 1, Time - (match(StartYear, 1993:2024) - 1), 0))

itsa_models <- list()
for (site in unique(sites_itsa_data$SiteName)) {
  site_data <- sites_itsa_data %>% filter(SiteName == site)
  if (first(site_data$StartYear) == 1993) {
    model <- gls(IndexValue ~ Time, data = site_data, correlation = corAR1(form = ~ Time), na.action = na.omit)
  } else {
    model <- gls(IndexValue ~ Time + Intervention + Post_Intervention_Time, data = site_data, correlation = corAR1(form = ~ Time), na.action = na.omit)
  }
  itsa_models[[site]] <- model
}

# extract the outcome
results_4sites_list <- list()
for (site in names(itsa_models)) {
  model <- itsa_models[[site]]
  conf_intervals <- intervals(model, level = 0.95, which = "coef")$coef
  
  if ("Post_Intervention_Time" %in% rownames(conf_intervals)) {
    slope_estimate <- coef(model)["Time"] + coef(model)["Post_Intervention_Time"]
    lower_ci <- conf_intervals["Time", "lower"] + conf_intervals["Post_Intervention_Time", "lower"]
    upper_ci <- conf_intervals["Time", "upper"] + conf_intervals["Post_Intervention_Time", "upper"]
  } else {
    slope_estimate <- coef(model)["Time"]
    lower_ci <- conf_intervals["Time", "lower"]
    upper_ci <- conf_intervals["Time", "upper"]
  }
  
  results_4sites_list[[site]] <- tibble(
    ClassName = site,
    Slope = slope_estimate,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci
  )
}

results_4sites <- bind_rows(results_4sites_list) %>%
  mutate(AnalysisType = "Specific Site") 

print(results_4sites)

#### 1.3 merge the outcome ####
final_results <- bind_rows(results_9class, results_4sites)


# 2. The Synthesis Plot ----
#=======================================================

# Step 2.1: Create a logical grouping variable for faceting
final_results_grouped <- final_results %>%
  mutate(
    # Create a new column 'PlotGroup' to define the panels for our plot
    PlotGroup = case_when(
      str_detect(ClassName, "Rewilded")         ~ "A: Rewilded",
      str_detect(ClassName, "Stable Agriculture") ~ "B: Stable Agriculture",
      str_detect(ClassName, "Stable Original")    ~ "C: Stable Natural",
      TRUE                                     ~ "D: Specific Rewilding Sites" 
    ),
    ClassNameOrdered = reorder_within(ClassName, Slope, PlotGroup)
  )

# Step 2.2: Create the final faceted plot
ggplot(data = final_results_grouped, 
       aes(x = Slope, y = ClassNameOrdered, color = AnalysisType)) +
  geom_pointrange(aes(xmin = Lower_CI, xmax = Upper_CI), size = 0.8, fatten = 2) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  
  facet_grid(PlotGroup ~ ., scales = "free_y", space = "free_y") +
  
  scale_y_reordered() +
  
  scale_color_manual(values = c("Regional Average" = "navy", "Specific Site" = "firebrick")) +
  
  labs(
    title = "Synthesis: Comparing Trend Slopes of Regional Classes and Specific Sites",
    subtitle = "Points represent the trend slope estimate; lines represent the 95% confidence interval.",
    x = "Trend Slope (Annual Rate of NDVI Change)",
    y = "Land Cover Class / Rewilding Site", 
    color = "Analysis Type"
  ) +
  
  theme_minimal(base_size = 12) + 
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(), 
    strip.text.y = element_text(face = "bold", angle = 0), 
    axis.text.y = element_text(size = 10)
  )

