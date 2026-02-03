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
                 "srvyr", "plotly")

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
                         "plot_map")))

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
                    "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg")

for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[allen_ear$nutrient == i]
  new_col <- paste0(i, "_inadequate")
  base_ai[[new_col]] <- ifelse(base_ai[[i]] < ear_value, 1, 0)
  
}

rm(allen_ear, ear_value, i, new_col)

#-------------------------------------------------------------------------------

# IF IN FUTURE WE WANT TO CREATE AN INTERACTIVE MAP WITH PLOTLY, THEN WE CAN APPLY 
# THIS CORRECTIVE FUNCTION SO THAT THE TOOLTIP CORRECTLY SHOWS VALUES WHEN HOVERING
# OVER AREAS INSTEAD OF LINES:
#  
# fixer <- function(gp) {
#   lapply(1:length(gp$x$data), \(m) {
#     gp$x$data[[m]]$hoveron <<- "fills"               # hover on the fill, not line
#     if(length(gp$x$data[[m]]$text > 1)) {
#       gp$x$data[[m]]$text <<- gp$x$data[[m]]$text[1] # only one tooltip per area
#     }
#   })
#   gp
# }

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

# Multiply by 100 and round to 1 decimal place:
mn_inadequacy <- mn_inadequacy |> 
  mutate(across(-c(adm2, geometry), ~ .x * 100)) |> 
  mutate(across(-c(adm2, geometry), ~ round(.x, digits = 1)))

# Convert to correct object class: 
mn_inadequacy <- st_as_sf(mn_inadequacy)

# Write to Rds:
write_rds(mn_inadequacy, "processed_data/mn_inadequacy.rds")

# VITAMIN A: 
plot_map(data = mn_inadequacy,
         col = "vita_inadequacy",
         title = "Vitamin A",
         metric = "Risk of inadequate intake (%)", 
         add_labels = TRUE,
         outline_sf = rwa_adm2)

ggsave("maps/percentages_labelled/vita_risk.png", width = 16, height = 12)

# THIAMINE:
plot_map(data = mn_inadequacy,
         col = "thia_inadequacy",
         title = "Thiamine",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1, 
         add_labels = TRUE)

ggsave("maps/percentages_labelled/thia_risk.png", width = 16, height = 12)

# RIBOFLAVIN:
plot_map(data = mn_inadequacy,
         col = "ribo_inadequacy",
         title = "Riboflavin",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = TRUE)

ggsave("maps/percentages_labelled/ribo_risk.png", width = 16, height = 12)

# NIACIN:
plot_map(data = mn_inadequacy,
         col = "niac_inadequacy",
         title = "Niacin",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = TRUE)

ggsave("maps/percentages_labelled/niac_risk.png", width = 16, height = 12)

# FOLATE:
plot_map(data = mn_inadequacy,
         col = "folate_inadequacy",
         title = "Folate",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = TRUE)

ggsave("maps/percentages_labelled/folate_risk.png", width = 16, height = 12)

# VITAMIN B12:
plot_map(data = mn_inadequacy,
         col = "vitb12_inadequacy",
         title = "Vitamin B12",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = TRUE)

ggsave("maps/percentages_labelled/vitb12_risk.png", width = 16, height = 12)

# IRON:
plot_map(data = mn_inadequacy,
         col = "fe_inadequacy",
         title = "Iron",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = TRUE)

ggsave("maps/percentages_labelled/fe_risk.png", width = 16, height = 12)

# ZINC:
plot_map(data = mn_inadequacy,
         col = "zn_inadequacy",
         title = "Zinc",
         metric = "Risk of inadequate intake (%)", 
         outline_sf = rwa_adm1,
         add_labels = TRUE)

ggsave("maps/percentages_labelled/zn_risk.png", width = 16, height = 12)

#-------------------------------------------------------------------------------

# clear environment: 
rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################