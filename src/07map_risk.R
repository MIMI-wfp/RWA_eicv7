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
                 "srvyr")

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

rm(list = setdiff(ls(), c("base_ai", "allen_ear", "hh_information")))

# Read in shapefile data: 
rwa_adm1 <- readRDS("shapefiles/rwa_adm1.rds")
rwa_adm2 <- readRDS("shapefiles/rwa_adm2.rds")

# Household locations: 
rwa_hh_adm <- readRDS("shapefiles/rwa_hh_adm.rds")

# Add locations to base_ai:
base_ai <- base_ai |> 
  left_join(rwa_hh_adm, by = "hhid")

#-------------------------------------------------------------------------------

# Define map function: 
plot_map <- function(data, col, title, metric, outline_sf, palette = "Zissou1", 
                     n = 100, limits = c(0,100)) {
  ggplot() +
    # fill the states by your chosen variable
    geom_sf(data = data,
            aes_string(fill = col),
            color = NA) +
    # add a single black outline
    geom_sf(data = outline_sf,
            fill = NA,
            color = "black",
            size = 1) +
    # continuous palette
    scale_fill_gradientn(
      colours = wes_palette(palette, n = n, type = "continuous"),
      limits = limits,
      name  = metric) +
    labs(title   = title) +
    theme_minimal() +
    theme(
      plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.caption     = element_text(hjust = 0.5),
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.title     = element_text(hjust = 0.5),
      legend.key.width = unit(1.35, "cm"),
      legend.key.height= unit(0.6, "cm")
    )
}

#-------------------------------------------------------------------------------

# BINARISE RISK OF INADEQUATE MICRONUTRIENT INTAKE: 
micronutrients <- c("vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg", 
                    "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg")

for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[allen_ear$nutrient == i]
  new_col <- paste0(i, "_inadequate")
  base_ai[[new_col]] <- ifelse(base_ai[[i]] < ear_value, 1, 0)
  
}

rm(allen_ear, ear_value, i, new_col)

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
            fe_inadequacy = survey_mean(fe_mg_inadequate, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL)) |> 
  left_join(rwa_adm2, by = "adm2")

# Mutate across variables except "adm2" and "geometry" to multiply by 100:
mn_inadequacy <- mn_inadequacy |> 
  mutate(across(-c(adm2, geometry), ~ .x * 100))

# Convert to correct object class: 
mn_inadequacy <- st_as_sf(mn_inadequacy)

# VITAMIN A: 
plot_map(data = mn_inadequacy,
         col = "vita_inadequacy",
         title = "Vitamin A",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1)

ggsave("maps/vita_risk.png", width = 8, height = 6)

# THIAMINE:
plot_map(data = mn_inadequacy,
         col = "thia_inadequacy",
         title = "Thiamine",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1)

ggsave("maps/thia_risk.png", width = 8, height = 6)

# RIBOFLAVIN:
plot_map(data = mn_inadequacy,
         col = "ribo_inadequacy",
         title = "Riboflavin",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1)

ggsave("maps/ribo_risk.png", width = 8, height = 6)

# NIACIN:
plot_map(data = mn_inadequacy,
         col = "niac_inadequacy",
         title = "Niacin",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1)

ggsave("maps/niac_risk.png", width = 8, height = 6)

# FOLATE:
plot_map(data = mn_inadequacy,
         col = "folate_inadequacy",
         title = "Folate",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1)

ggsave("maps/folate_risk.png", width = 8, height = 6)

# VITAMIN B12:
plot_map(data = mn_inadequacy,
         col = "vitb12_inadequacy",
         title = "Vitamin B12",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1)

ggsave("maps/vitb12_risk.png", width = 8, height = 6)

# IRON:
plot_map(data = mn_inadequacy,
         col = "fe_inadequacy",
         title = "Iron",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1)

ggsave("maps/fe_risk.png", width = 8, height = 6)

# ZINC:
plot_map(data = mn_inadequacy,
         col = "zn_inadequacy",
         title = "Zinc",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1)

ggsave("maps/zn_risk.png", width = 8, height = 6)

#-------------------------------------------------------------------------------

# clear environment: 
rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################