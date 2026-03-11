################################################################################
#################### SCRIPT TO PREPARE DATA FOR MIMI DATABASE ##################
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 30-09-2025
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA:
base_ai <- read_csv("processed_data/rwa_eicv2324_base_ai.csv")
food_groups <- read_csv("metadata/food_groups.csv")

#-------------------------------------------------------------------------------

# TRANSFORM BASE_AI: 
base_ai <- base_ai |> 
  # ADD COLUMNS WITH NULL VALUES: 
  mutate(vitb6_mg = NA_real_,
         vitd_mcg = NA_real_,
         vitc_mg = NA_real_,
         ca_mg = NA_real_) |> 
  # Define order of variables: 
  dplyr::select(iso3, survey, hhid, energy_kcal, vita_rae_mcg, thia_mg, ribo_mg,
                niac_mg, vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, vitc_mg,
                ca_mg, fe_mg, zn_mg)

#--------------------------------------------------------------------------------

# TRANSFORM FOOD GROUPS:
food_groups <- food_groups |> 
  mutate(iso3 = "RWA",
         survey = "eicv2324") |>
  dplyr::select(iso3, survey, item_code, food_group)

#--------------------------------------------------------------------------------

# WRITE DATA: 
# write_csv(base_ai, "processed_data/rwa_eicv2324_base_ai.csv")
# write_csv(food_groups, "processed_data/rwa_eicv2324_food_groups.csv")

rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################