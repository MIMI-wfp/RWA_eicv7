################################################################################
############## SCRIPT FOR DATA TRIANGULATION WITH RWA DHS 2019-2020 ############
################################################################################

# Author: Mo Osman
# Date created: 02-02-2026
# Last edited: 

# Data Sources: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) &
# Rwanda Demographic and Health Survey 2019-2020 (DHS 2019-2020)

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# SOURCE FUNCTIONS:
source("src/05base_model_functions.R")

rm(list = setdiff(ls(), c("allen_ear")))

#-------------------------------------------------------------------------------

# READ DATA:
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")
base_ai <- read_csv("processed_data/rwa_eicv2324_base_ai.csv")

# Household locations: 
rwa_hh_adm <- readRDS("shapefiles/rwa_hh_adm.rds")

# Add locations to base_ai:
base_ai <- base_ai |> 
  left_join(rwa_hh_adm, by = "hhid")

#-------------------------------------------------------------------------------

# SPECIFY MICRONUTRIENTS FOR TRIANGULATION:
micronutrients <- c("vita_rae_mcg", "fe_mg")

#-------------------------------------------------------------------------------

# CREATE ANALYSIS DATAFRAME: 

analysis_df <- base_ai |> 
  dplyr::select(hhid, vita_rae_mcg, fe_mg, adm1, adm2) |> 
  left_join(hh_information |> 
    dplyr::select(
      hhid, 
      res, 
      sep_quintile, 
      survey_wgt
    ), 
by = "hhid") |>
  as_survey_design(
    ids = hhid,
    weights = survey_wgt,
    nest = TRUE
  )

#-------------------------------------------------------------------------------

# NATIONAL INADEQUACY ESTIMATES FOR EACH MICRONUTRIENT:
vita_national <- analysis_df |> 
  summarise(
    vita_rae_mcg_inadequate = survey_mean(
      vita_rae_mcg < allen_ear$ear_value[allen_ear$nutrient == "vita_rae_mcg"],
      na.rm = TRUE,
      vartype = "ci"
    )
  )

fe_national <- analysis_df |>
  summarise(
    fe_mg_inadequate = survey_mean(
      fe_mg < allen_ear$ear_value[allen_ear$nutrient == "fe_mg"],
      na.rm = TRUE,
      vartype = "ci"
    )
  )


#-------------------------------------------------------------------------------

# AT ADM1 LEVEL:
vita_adm1 <- analysis_df |> 
  group_by(adm1) |> 
  summarise(
    vita_rae_mcg_inadequate = survey_mean(
      vita_rae_mcg < allen_ear$ear_value[allen_ear$nutrient == "vita_rae_mcg"],
      na.rm = TRUE,
      vartype = "ci"
    )
  )

fe_adm1 <- analysis_df |>
  group_by(adm1) |>
  summarise(
    fe_mg_inadequate = survey_mean(
      fe_mg < allen_ear$ear_value[allen_ear$nutrient == "fe_mg"],
      na.rm = TRUE,
      vartype = "ci"
    )
  )

#-------------------------------------------------------------------------------

# BY URBAN/RURAL STATUS:
vita_res <- analysis_df |> 
  group_by(res) |> 
  summarise(
    vita_rae_mcg_inadequate = survey_mean(
      vita_rae_mcg < allen_ear$ear_value[allen_ear$nutrient == "vita_rae_mcg"],
      na.rm = TRUE,
      vartype = "ci"
    )
  )

fe_res <- analysis_df |>
  group_by(res) |>
  summarise(
    fe_mg_inadequate = survey_mean(
      fe_mg < allen_ear$ear_value[allen_ear$nutrient == "fe_mg"],
      na.rm = TRUE,
      vartype = "ci"
    )
  )

#-------------------------------------------------------------------------------

# BY SEP QUINTILE:
vita_sep <- analysis_df |> 
  group_by(sep_quintile) |> 
  summarise(
    vita_rae_mcg_inadequate = survey_mean(
      vita_rae_mcg < allen_ear$ear_value[allen_ear$nutrient == "vita_rae_mcg"],
      na.rm = TRUE,
      vartype = "ci"
    )
  )

fe_sep <- analysis_df |>
  group_by(sep_quintile) |>
  summarise(
    fe_mg_inadequate = survey_mean(
      fe_mg < allen_ear$ear_value[allen_ear$nutrient == "fe_mg"],
      na.rm = TRUE,
      vartype = "ci"
    )
  )

#-------------------------------------------------------------------------------


