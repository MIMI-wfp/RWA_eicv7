################################################################################
############ SCRIPT FOR EXPLORATORY ANALYSIS OF FOOD GROUP SOURCES #############
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 08-09-2025
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "ggplot2", "patchwork",
                 "cowplot")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# A script to explore the sources of energy intake by food group in the 
# population of Rwanda, using the Rwanda EICV7.

#-------------------------------------------------------------------------------

# READ DATA:
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")
food_groups <- read_csv("metadata/food_groups.csv")
food_consumption <- read_csv("processed_data/rwa_eicv2324_food_consumption.csv")
fc_table <- read_csv("processed_data/rwa_eicv2324_fct.csv")

#-------------------------------------------------------------------------------

# SPECIFY SUB-POPULATION FOR ANALYSIS:
sub_population <- "Rural" # "all", "Urban", "Rural" (CAPITALISE FIRST LETTER)

food_consumption <- if(sub_population == "all") {

  food_consumption

} else {

  if(sub_population == "Urban") {

    food_consumption <- food_consumption |> 
      left_join(hh_information |> 
        dplyr::select(hhid, res), by = "hhid") |>
      filter(res == "Urban")

    food_consumption

  } else if(sub_population == "Rural") {

    food_consumption <- food_consumption |> 
      left_join(hh_information |> 
        dplyr::select(hhid, res), by = "hhid") |>
      filter(res == "Rural")

    food_consumption
    
  } else {
    print("INVALID - Please specify one of the following: 'all', 'Urban', or 'Rural'.")
  }
}

#-------------------------------------------------------------------------------

# CALCULATE HOUSEHOLD NUTRIENT INTAKE BY FOOD ITEM:

# Merge food consumption data with nutrient composition data: 
nutrient_intake <- food_consumption |> 
  dplyr::select(-quantity_g) |> 
  left_join(fc_table |> 
    dplyr::select(-c(survey, iso3)), by = "item_code") |> 
  # Calculate nutrient contributions of each food item:
  mutate(across(energy_kcal:zn_mg, ~ . * quantity_100g)) |> 
  # Add food group names:
  left_join(food_groups |> 
    dplyr::select(item_code, alternative_group), by = "item_code") |> 
  # Re-group some food items as "Nutritious foods":
  mutate(
    food_group_new = case_when(alternative_group %in% c("dairy", "nuts_seeds", 
        "fruit_vegetables", "pulses", "green_leafy_veg", "vita_fruit_veg",
        "meat_fish_eggs") ~ "nutritious_foods",
      TRUE ~ alternative_group)
  )

#--------------------------------------------------------------------------------

# CALCULATE HOUSEHOLD NUTRIENT INTAKE BY FOOD GROUP:
energy_intake_grouped <- nutrient_intake |> 
  group_by(food_group_new) |> 
  summarise(total_energy_kcal = sum(energy_kcal, na.rm = TRUE),
  .groups = "drop")

overall_energy <- sum(energy_intake_grouped$total_energy_kcal, na.rm = TRUE)

# Percentage energy contribution by food group:
energy_intake_grouped <- energy_intake_grouped |> 
  filter(total_energy_kcal > 0) |>
  mutate(energy_pct = (total_energy_kcal / overall_energy) * 100) |> 
  arrange(desc(energy_pct))

# Rename foods for plot: 
energy_intake_grouped <- energy_intake_grouped |> 
  mutate(food_group_new = case_when(
    food_group_new == "grains_roots_tubers" ~ "Grains, roots and tubers",
    food_group_new == "edible_oil" ~ "Edible oils",
    food_group_new == "sugars" ~ "Sugars",
    food_group_new == "others" ~ "Other foods",
    food_group_new == "nutritious_foods" ~ "Nutritious foods"
  ))

# Reorder factor levels in reverse order: 
energy_intake_grouped$food_group_new <- factor(
  energy_intake_grouped$food_group_new,
  levels = rev(energy_intake_grouped$food_group_new)
)

# BREAKDOWN FOR NUTRITIOUS FOODS:

# Firstly group together some of the nutritious foods so that there are fewer
# categories:
nutrient_intake <- nutrient_intake |> 
  mutate(alternative_group = case_when(
    alternative_group %in% c("nuts_seeds", "pulses") ~ "Nuts seeds and pulses",
    alternative_group %in% c("meat_fish_eggs", "dairy") ~ "Animal source foods",
    alternative_group %in% c("vita_fruit_veg", "green_leafy_veg") ~ "Vitamin A rich fruit and vegetables",
    alternative_group == "fruit_vegetables" ~ "Other fruit and vegetables",
    TRUE ~ alternative_group
  ))

nutritious_breakdown <- nutrient_intake |> 
  filter(food_group_new == "nutritious_foods") |> 
  group_by(alternative_group) |> 
  summarise(
    total_energy_kcal = sum(energy_kcal, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(energy_pct = (total_energy_kcal / overall_energy) * 100) |> 
  arrange(desc(energy_pct)) |> 
  rename(food_group = alternative_group)

# Reorder factor levels
nutritious_breakdown$food_group <- factor(
  nutritious_breakdown$food_group,
  levels = rev(nutritious_breakdown$food_group)
)

#------------------------------------------------------------------------------- 

# CREATE PLOTS: 

# Specify colour palette:
main_palette <- c(
  "Grains, roots and tubers"   = "#E69F00", # orange
  "Edible oils"             = "#F0E442", # yellow
  "Sugars"           = "#CC79A7", # reddish purple
  "Other foods"           = "#999999", # grey
  "Nutritious foods" = "#009E73"  # bluish green
)

nutritious_colors <- colorRampPalette(c("#D9F0E6", "#009E73"))(length(unique(nutritious_breakdown$food_group)))
names(nutritious_colors) <- levels(nutritious_breakdown$food_group)

# Main plot:
main_plot <- ggplot(energy_intake_grouped, aes(x = 1, y = energy_pct, fill = food_group_new)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(energy_pct, 0), "%")),
            position = position_stack(vjust = 0.5), color = "black", size = 4) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = main_palette) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    axis.line.x = element_line(color = "black", size = 0.5)
  ) +
  ggtitle(paste0("% dietary energy contributions, by food group for ", sub_population, " households"))

main_plot

# Nutritious foods breakdown plot:
breakdown_plot <- ggplot(nutritious_breakdown, aes(x = 1, y = energy_pct, fill = food_group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(energy_pct, 0), "%")),
            position = position_stack(vjust = 0.5), color = "black", size = 3) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = nutritious_colors) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    axis.line.x = element_line(color = "black", size = 0.5)
  )

breakdown_plot

# Combine plots:
combined <- plot_grid(
  main_plot,
  plot_grid(NULL, breakdown_plot, NULL, ncol = 3, rel_widths = c(1, 4, 1)),
  ncol = 1,
  rel_heights = c(0.8, 0.5)
)

combined

# Save: 
# ggsave(
#   combined,
#   filename = "SPECIFY FILE NAME AND PATH HERE",
#   width = 10, height = 8, dpi = 300
# )

# Clear environment: 
rm(list = ls())

################################################################################
################################ END OF SCRIPT #################################
################################################################################