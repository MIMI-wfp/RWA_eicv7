################################################################################
################# SCRIPT FOR EXTRACTING HOUESHOLD INFORMATION ##################
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 17-07-2025
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA:
household_summary <- read_dta("raw_data/CS_EICV7_poverty_file.dta")
afe <- read_csv("processed_data/rwa_eicv2324_afe.csv")

#-------------------------------------------------------------------------------

# EXTRACT HOUSEHOLD INFORMATION:
household_summary <- household_summary |>
  dplyr::select(hhid,
                clust,
                province,
                district,
                ur,
                weight,
                cons1ae) |> 
  # Month and year variables have not been made available in the public dataset
  rename(ea = clust,
         adm1 = province,
         adm2 = district,
         res = ur,
         survey_wgt = weight,
         pc_expenditure = cons1ae)

# Label urban and rural: 
household_summary <- household_summary |>
  mutate(res = case_when(
    res == 1 ~ "Urban",
    res == 2 ~ "Rural"
  ))

#-------------------------------------------------------------------------------

# CONSUMPTION/EXPENDITURE QUANTILES: 

 # Calculate quintiles based on per capita expenditure:
household_summary <- household_summary |>
  mutate(sep_quintile = ntile(pc_expenditure, 5)) |> 
  # And also quintiles stratified by res: 
  mutate(res_quintile = case_when(
    res == "Urban" ~ ntile(pc_expenditure, 5),
    res == "Rural" ~ ntile(pc_expenditure, 5)
  ))

#-------------------------------------------------------------------------------

# ADD REMAINING VARIABLES AND RE-ORDER: 
hh_information <- household_summary |>
  left_join(afe, by = "hhid") |>
  mutate(iso3 = "RWA",
         survey = "eicv2324",
         year = NA,
         month = NA,
         zone = NA) |> 
  dplyr::select(iso3, survey, hhid, zone, adm1, adm2, ea, res, sep_quintile,
                res_quintile, year, month, survey_wgt, afe, pc_expenditure)

#-------------------------------------------------------------------------------

# WRITE CSV: 
write_csv(hh_information, "processed_data/rwa_eicv2324_hh_information.csv")

rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################