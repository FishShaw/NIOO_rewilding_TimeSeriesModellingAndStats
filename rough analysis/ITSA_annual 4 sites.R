library(tidyverse)
library(nlme) 
library(ggplot2)

sites_file_path <- "E:/WUR_Intern/RewildingProject_RawData/NDVI/ZonalStatsNew_4Sites_Annual.csv"
intervention_years <- tibble(
  SiteName = c("MW", "BB", "OW", "EW"),
  StartYear = c(1993, 2002, 2006, 2008)
)

sites_annual_data <- read_csv(sites_file_path)

# 1. Data Preparation for ITSA ----
#=======================================
sites_long_data <- sites_annual_data %>%
  separate(SourceFile, into = c("IndexType", "Year"), sep = "_", remove = FALSE) %>%
  rename(
    SiteName = Site_ID,
    IndexValue = MEAN
  ) %>%
  mutate(Year = as.numeric(Year))

sites_itsa_data <- sites_long_data %>%
  left_join(intervention_years, by = "SiteName") %>%
  filter(!is.na(StartYear)) %>%
  mutate(
    Time = Year - 1993 + 1,
    Intervention = if_else(Year >= StartYear, 1, 0),
    Post_Intervention_Time = if_else(Intervention == 1, Time - (match(StartYear, 1993:2024) - 1), 0)
  )

print(head(sites_itsa_data))


# 2. ITSA Modeling & Analysis (Focusing on NDVI) ----
#====================================================

itsa_models <- list()
site_names <- unique(sites_itsa_data$SiteName)

for (site in site_names) {
  print(paste("--- Building model for site:", site, "(NDVI) ---"))
  
  site_data <- sites_itsa_data %>% filter(SiteName == site, IndexType == "NDVI")
  start_year_for_site <- intervention_years$StartYear[intervention_years$SiteName == site]
  
  if (start_year_for_site == 1993) {
    # For MW, where intervention starts at year 1, use a simple trend model.
    print("Intervention at start. Using a simple trend model.")
    model <- gls(
      IndexValue ~ Time, 
      data = site_data,
      correlation = corAR1(form = ~ Time),
      na.action = na.omit
    )
  } else {
    # For all other sites, use the full ITSA model.
    print("Intervention after start. Using full ITSA model.")
    model <- gls(
      IndexValue ~ Time + Intervention + Post_Intervention_Time,
      data = site_data,
      correlation = corAR1(form = ~ Time),
      na.action = na.omit
    )
  }
  
  itsa_models[[site]] <- model
}

# View the detailed results for any site.
print("--- Model Summary for Site: BB (full ITSA model) ---")
print(summary(itsa_models$OW))
print("--- Model Summary for Site: MW (simple trend model) ---")
print(summary(itsa_models$MW))


# 3. Visualization ----
#========================================================================

# --- Step 4.1: Create a data frame with significance labels ---
summary_text_list <- list()

get_significance_symbols <- function(p_value) {
  if (is.na(p_value)) {
    return("N/A")
  } else if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("ns") # ns = not significant
  }
}

for (site in site_names) {
  model_summary <- summary(itsa_models[[site]])
  coef_table <- model_summary$tTable
  
  # Check if the site is MW (or any site with a start year of 1993)
  start_year_for_site <- intervention_years$StartYear[intervention_years$SiteName == site]
  
  if (start_year_for_site == 1993) {
    # For MW, we only show the overall trend significance
    p_trend <- coef_table["Time", "p-value"]
    trend_sig_symbol <- get_significance_symbols(p_trend)
    label <- paste("Overall Trend Sig.:", trend_sig_symbol)
  } else {
    # For all other sites, we show all three components
    p_pre_trend <- coef_table["Time", "p-value"]
    p_level_change <- coef_table["Intervention", "p-value"]
    p_slope_change <- coef_table["Post_Intervention_Time", "p-value"]
    
    pre_trend_symbol <- get_significance_symbols(p_pre_trend)
    level_sig_symbol <- get_significance_symbols(p_level_change)
    slope_sig_symbol <- get_significance_symbols(p_slope_change)
    
    label <- paste(
      "Pre-Trend Sig.    :", pre_trend_symbol,
      "\nLevel Change Sig. :", level_sig_symbol,
      "\nSlope Change Sig.:", slope_sig_symbol
    )
  }
  
  # Add the site name and the label to our list
  summary_text_list[[site]] <- tibble(SiteName = site, label_text = label)
}

# Combine the list into a single data frame for plotting
summary_labels <- bind_rows(summary_text_list)


# --- Step 4.2: Generate predictions for trend lines ---
ndvi_data_to_predict <- sites_itsa_data %>% filter(IndexType == "NDVI")
predictions_list <- list()

for (site in site_names) {
  current_site_data <- ndvi_data_to_predict %>% filter(SiteName == site)
  current_model <- itsa_models[[site]]
  current_site_data$PredictedValue <- predict(current_model, newdata = current_site_data)
  predictions_list[[site]] <- current_site_data
}

sites_itsa_data_predicted <- bind_rows(predictions_list)


# --- Step 4.3: Create the plot ---
sites_itsa_data_predicted <- sites_itsa_data_predicted %>%
  mutate(
    SiteName = factor(SiteName, levels = c("MW", "BB", "OW", "EW"))
  )
summary_labels <- summary_labels %>%
  mutate(
    SiteName = factor(SiteName, levels = c("MW", "BB", "OW", "EW"))
  )

custom_breaks <- sort(unique(c(seq(1995, 2025, by = 5), intervention_years$StartYear)))

ggplot(data = sites_itsa_data_predicted, aes(x = Year, y = IndexValue)) +
  geom_point(alpha = 0.6, color = "grey40") + 
  geom_line(aes(y = PredictedValue), color = "firebrick", size = 1.2) + 
  geom_vline(aes(xintercept = StartYear), linetype = "dashed", color = "royalblue") + 
  
  # Add text labels to the plot
  geom_text(
    data = summary_labels,
    aes(x = -Inf, y = Inf, label = label_text),
    hjust = -0.05, vjust = 1.2, size = 2.5, 
    family = "mono", 
    inherit.aes = FALSE
  ) +
  
  facet_wrap(~ SiteName, scales = "free_y", ncol = 2) + 
  scale_x_continuous(breaks = custom_breaks) +
  labs(
    # title = "Interrupted Time Series Analysis (ITSA) of Rewilding Sites for NDVI",
    # subtitle = "Blue dashed line indicates the start year of rewilding intervention.",
    x = "Year",
    y = "Annual Growing Season Mean NDVI"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

