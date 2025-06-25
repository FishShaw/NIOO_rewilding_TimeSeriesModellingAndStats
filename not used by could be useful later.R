#### 3.3 Sen's Slope Comparison Bar Chart ####
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
    title = "Variability of NDVI across Specific Rewilding Sites",
    # subtitle = "Boxplots show the distribution of all annual values from 1993-2024 for each site",
    x = "Specific Site", 
    y = "Annual Growing Season Mean Index Value",
    fill = "Site Name" 
  ) +
  theme_minimal() +
  theme(
    legend.position = "none" 
  )