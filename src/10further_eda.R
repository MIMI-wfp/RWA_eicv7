################################################################################
##################### SCRIPT FOR FURTHER EXPLORATORY ANALYSES ##################
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

#-------------------------------------------------------------------------------

# READ DATA: 
base_ai <- read_csv("processed_data/rwa_eicv2324_base_ai.csv")
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")

#-------------------------------------------------------------------------------

# CREATE ANALYSIS DATAFRAME: 
analysis_df <- base_ai |> 
  left_join(hh_information |> 
    dplyr::select(hhid, res), by = "hhid")

#-------------------------------------------------------------------------------

# Calculate median energy intake for all households:
median_kcal <- analysis_df |> 
  pull(energy_kcal) |>
  median(na.rm = TRUE)

# Calculate median energy intake for urban/rural populations: 
urban_kcal <- analysis_df |> 
  filter(res == "Urban") |> 
  pull(energy_kcal) |>
  median(na.rm = TRUE)

rural_kcal <- analysis_df |>
  filter(res == "Rural") |> 
  pull(energy_kcal) |>
  median(na.rm = TRUE)

#-------------------------------------------------------------------------------

# Histogram of energy intake: 
ggplot(analysis_df, aes(x = energy_kcal)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Energy Intake",
       x = "Energy Intake (kcal)",
       y = "Frequency") +
  xlim(0, 10000) +
  theme_minimal() +
  geom_vline(xintercept = median_kcal, linetype = "dashed", color = "red") +
  annotate("text", x = median_kcal + 4000, y = 500, 
           label = paste("Median:", round(median_kcal, 1), "kcal/day"), 
           color = "red", fontface = "bold")

ggsave("figures/energy_intake_histogram.png", width = 8, height = 5)

# Multi-histogram stratified by urban/rural residence:
 ggplot(analysis_df, aes(x = energy_kcal, fill = res)) +
  geom_histogram(bins = 50, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of Energy Intake for Urban and Rural Households",
       x = "Energy Intake (kcal)",
       y = "Frequency") +
  xlim(0, 10000) +
  theme_minimal() +
  scale_fill_manual(values = c("Urban" = "steelblue", "Rural" = "orange")) +
  geom_vline(xintercept = urban_kcal, linetype = "dashed", color = "red") +
  geom_vline(xintercept = rural_kcal, linetype = "dashed", color = "black") +
  annotate("text", x = urban_kcal + 4000, y = 350, 
           label = paste("Urban Median:", round(urban_kcal, 1), "kcal/day"), 
           color = "red", fontface = "bold") +
  annotate("text", x = rural_kcal + 4000, y = 300,
           label = paste("Rural Median:", round(rural_kcal, 1), "kcal/day"), 
           color = "black", fontface = "bold") +
  guides(fill = guide_legend(title = NULL))

ggsave("figures/energy_intake_urbrur.png", width = 8, height = 5)

# Clear environment: 
rm(list = ls())

################################################################################
############################## END OF SCRIPT ###################################
################################################################################