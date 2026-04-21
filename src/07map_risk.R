################################################################################
########## SCRIPT FOR MAPPING RISK OF INADEQUATE MICRONUTRIENT INTAKE ##########
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 20-07-2025
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "spdep", "sf", "wesanderson",
                 "srvyr", "cowplot")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA: 
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")
base_ai <- read_csv("processed_data/rwa_eicv2324_base_ai.csv")
source("src/05base_model_functions.R")

rm(list = setdiff(ls(), c("base_ai", "allen_ear", "hh_information",
                         "plot_map", "fe_prob_inadequacy")))

# Read in shapefile data: 
rwa_adm1 <- readRDS("shapefiles/rwa_adm1.rds")
rwa_adm2 <- readRDS("shapefiles/rwa_adm2.rds")

# Household locations: 
rwa_hh_adm <- readRDS("shapefiles/rwa_hh_adm.rds")

# Add locations to base_ai:
base_ai <- base_ai |> 
  left_join(rwa_hh_adm, by = "hhid")

#-------------------------------------------------------------------------------

# BINARISE RISK OF INADEQUATE MICRONUTRIENT INTAKE: 
micronutrients <- c("vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg", 
                    "folate_mcg", "vitb12_mcg", "zn_mg")

for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[allen_ear$nutrient == i]
  new_col <- paste0(i, "_inadequate")
  base_ai[[new_col]] <- ifelse(base_ai[[i]] < ear_value, 1, 0)
  
}

rm(allen_ear, ear_value, i, new_col)

#-------------------------------------------------------------------------------

# PROBABILITY OF INADEQUACY (FOR IRON): 

# Due to skewed iron intake distribution, we will use a full probability approach
# instead of using a fixed-cutpoint (EAR) to determine inadequacy. 

base_ai <- fe_prob_inadequacy(base_ai, bio_avail = 5) 
# Low bioavailability of iron assumed due to low consumption of ASF in Rwanda, 
# as well as a largely unrefined diet (high dietary phytate).

rm(fe_prob_inadequacy)

#-------------------------------------------------------------------------------

# CALCULATE PREVALENCE AT RISK OF INADEQUATE INTAKE (AT ADM2 LEVEL)

analysis_df <- base_ai |> 
  left_join(hh_information |> 
              dplyr::select(hhid, survey_wgt), 
            by = "hhid") |>
  as_survey_design(weights = survey_wgt)

mn_inadequacy <- analysis_df |> 
  group_by(adm2) |> 
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            thia_inadequacy = survey_mean(thia_mg_inadequate, na.rm = T, vartype = NULL),
            ribo_inadequacy = survey_mean(ribo_mg_inadequate, na.rm = T, vartype = NULL),
            niac_inadequacy = survey_mean(niac_mg_inadequate, na.rm = T, vartype = NULL),
            folate_inadequacy = survey_mean(folate_mcg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            fe_inadequacy = survey_mean(fe_prob_inad, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL)) |> 
  left_join(rwa_adm2, by = "adm2")

# Multiply by 100 and round to 1 decimal place:
mn_inadequacy <- mn_inadequacy |> 
  mutate(across(-c(adm2, geometry), ~ .x * 100)) |> 
  mutate(across(-c(adm2, geometry), ~ round(.x, digits = 1)))

# Convert to correct object class: 
mn_inadequacy <- st_as_sf(mn_inadequacy)

# Write to Rds:
write_rds(mn_inadequacy, "processed_data/mn_inadequacy.rds")

# Also compute national mn inadequacy for labelling maps:
national_mn_inadequacy <- analysis_df |> 
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            thia_inadequacy = survey_mean(thia_mg_inadequate, na.rm = T, vartype = NULL),
            ribo_inadequacy = survey_mean(ribo_mg_inadequate, na.rm = T, vartype = NULL),
            niac_inadequacy = survey_mean(niac_mg_inadequate, na.rm = T, vartype = NULL),
            folate_inadequacy = survey_mean(folate_mcg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            fe_inadequacy = survey_mean(fe_prob_inad, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL)) |> 
  mutate(across(everything(), ~ .x * 100)) |> 
  mutate(across(everything(), ~ round(.x, digits = 1)))

# VITAMIN A: 
vita_map <- plot_map(data = mn_inadequacy,
         col = "vita_inadequacy",
         title = paste0("Vitamin A (", national_mn_inadequacy$vita_inadequacy, "%)"),
         metric = "Risk of inadequate intake (%)", 
         add_labels = FALSE,
         outline_sf = rwa_adm1) +
  theme(legend.position = "none")

vita_map 

# ggsave("maps/percentages_labelled/vita_risk.png", width = 16, height = 12)

# THIAMINE:
thia_map <- plot_map(data = mn_inadequacy,
         col = "thia_inadequacy",
         title = paste0("Thiamine (", national_mn_inadequacy$thia_inadequacy, "%)"),
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1, 
         add_labels = FALSE) + 
  theme(legend.position = "none")

thia_map

# ggsave("maps/percentages_labelled/thia_risk.png", width = 16, height = 12)

# RIBOFLAVIN:
ribo_map <- plot_map(data = mn_inadequacy,
         col = "ribo_inadequacy",
         title = paste0("Riboflavin (", national_mn_inadequacy$ribo_inadequacy, "%)"),
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = FALSE) +
  theme(legend.position = "none")

ribo_map

# ggsave("maps/percentages_labelled/ribo_risk.png", width = 16, height = 12)

# NIACIN:
niac_map <- plot_map(data = mn_inadequacy,
         col = "niac_inadequacy",
         title = paste0("Niacin (", national_mn_inadequacy$niac_inadequacy, "%)"),
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = FALSE) +
  theme(legend.position = "none")

niac_map

# ggsave("maps/percentages_labelled/niac_risk.png", width = 16, height = 12)

# FOLATE:
folate_map <- plot_map(data = mn_inadequacy,
         col = "folate_inadequacy",
         title = paste0("Folate (", national_mn_inadequacy$folate_inadequacy, "%)"),
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = FALSE) +
  theme(legend.position = "none")

folate_map

# ggsave("maps/percentages_labelled/folate_risk.png", width = 16, height = 12)

# VITAMIN B12:
vitb12_map <- plot_map(data = mn_inadequacy,
         col = "vitb12_inadequacy",
         title = paste0("Vitamin B12 (", national_mn_inadequacy$vitb12_inadequacy, "%)"),
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = FALSE) +
  theme(legend.position = "none")

vitb12_map

# ggsave("maps/percentages_labelled/vitb12_risk.png", width = 16, height = 12)

# IRON:
fe_map <- plot_map(data = mn_inadequacy,
         col = "fe_inadequacy",
         title = paste0("Iron (", national_mn_inadequacy$fe_inadequacy, "%)"),
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = FALSE) +
  theme(legend.position = "none")

fe_map

# ggsave("maps/fe_risk.png", width = 16, height = 12)

# ZINC:
zn_map <- plot_map(data = mn_inadequacy,
         col = "zn_inadequacy",
         title = paste0("Zinc (", national_mn_inadequacy$zn_inadequacy, "%)"),
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = FALSE) +
  theme(legend.position = "none")

zn_map

# ggsave("maps/percentages_labelled/zn_risk.png", width = 16, height = 12)

#-------------------------------------------------------------------------------

# Arrange maps in a grid:
grid_plot <- plot_grid(vita_map, thia_map, ribo_map, niac_map, folate_map, vitb12_map, fe_map, zn_map,
          ncol = 4)

grid_plot

#-------------------------------------------------------------------------------

# Grab a standalone legend: 
legend <- get_legend(
  plot_map(data = mn_inadequacy,
           col = "vita_inadequacy",
           title = "Vitamin A",
           metric = "Risk of inadequate intake (%)", 
           outline_sf = rwa_adm1,
           add_labels = FALSE) +
    theme(legend.position = "bottom")
)

ggdraw(legend)

# Add legend to grid plot:
final_plot <- plot_grid(grid_plot, legend, ncol = 1, rel_heights = c(1, 0.1))

final_plot

ggsave("maps/all_micronutrients_grid.png", width = 15, height = 8)

# clear environment: 
rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################