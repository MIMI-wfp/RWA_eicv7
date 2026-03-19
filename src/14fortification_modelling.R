################################################################################
################### SCRIPT FOR FORTIFICATION SCENARIO MODELLING ################
################################################################################

# Author: Mo Osman
# Date created: 11-Mar-2026
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "readxl", "haven",
                 "wesanderson", "sf")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# Note that the definition for edible oil is broad in Rwanda: 
# "edible vegetable
# Source: Regulations N° CBD/TRG/003 Rev. N°1 Governing Food Fortification in Rwanda

#-------------------------------------------------------------------------------

# READ DATA:
food_consumption <- read_csv("processed_data/rwa_eicv2324_food_consumption.csv")
fortification_factors <- read_excel("metadata/fortification/fortification_factors.xls", 
                                    sheet = "fortification_factors")

# Raw food consumption module: 
food_cons8b <- read_dta("raw_data/CS_S8B_Food_Expenditure_Consumption.dta")
conversion_factors <- read_csv("metadata/conversion_factors.csv") |> 
  dplyr::select(item_code, unit, conv_fac)

# Base apparent intake: 
base_ai <- read_csv("processed_data/rwa_eicv2324_base_ai.csv")

# AFE values: 
afe <- read_csv("processed_data/rwa_eicv2324_hh_information.csv") |> 
  dplyr::select(hhid, afe)

#--------------------------------------------------------------------------------

# COMPUTE PROPORTION OF FOOD QUANTITIES MARKET ACQUIRED:

# Vectorise conversion factors for matching: 
cf_vec <- setNames(
  conversion_factors$conv_fac,
  paste(conversion_factors$item_code, conversion_factors$unit)
)

# Compute proportion of food vehicles acquired from the market for each food item, by household:
proportion_market <- food_cons8b |>
  # Filter to include only fortifiable food vehicles:
  filter(s8bq0 %in% fortification_factors$item_code) |>
  mutate(
    cf_purchased_v2 = cf_vec[paste(s8bq0, s8bq3b_v2)],
    cf_purchased_v3 = cf_vec[paste(s8bq0, s8bq3b_v3)],
    cf_purchased_v4 = cf_vec[paste(s8bq0, s8bq3b_v4)],
    cf_purchased_v5 = cf_vec[paste(s8bq0, s8bq3b_v5)],
    cf_total_v2 = cf_vec[paste(s8bq0, s8bq7b_v2)],
    cf_total_v3 = cf_vec[paste(s8bq0, s8bq7b_v3)],
    cf_total_v4 = cf_vec[paste(s8bq0, s8bq7b_v4)],
    cf_total_v5 = cf_vec[paste(s8bq0, s8bq7b_v5)]
  ) |>
  group_by(hhid, s8bq0) |>
  # Compute quantities coming from market vs. totals: 
  summarise(
    qty_market = sum(s8bq3a_v2 * cf_purchased_v2, s8bq3a_v3 * cf_purchased_v3,
                     s8bq3a_v4 * cf_purchased_v4, s8bq3a_v5 * cf_purchased_v5, na.rm = TRUE),
    qty_total = sum(s8bq7a_v2 * cf_total_v2, s8bq7a_v3 * cf_total_v3,
                    s8bq7a_v4 * cf_total_v4, s8bq7a_v5 * cf_total_v5, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  # Filter for non-zero quantities: 
  filter(qty_total > 0) |>
  # Compute proportion market acquired:
  mutate(proportion_market = qty_market / qty_total) |> 
  # Cap proportions at 1:
  mutate(proportion_market = ifelse(proportion_market > 1, 1, proportion_market)) |>
  dplyr::select(hhid, s8bq0, proportion_market) |>
  rename(item_code = s8bq0)

rm(cf_vec, food_cons8b)

#--------------------------------------------------------------------------------

# JOIN FOOD CONSUMPTION, PROPORTION MARKET ACQUIRED, AND FORTIFICATION FACTORS DATAFRAMES:
vehicle_consumption <- food_consumption |> 
  left_join(proportion_market, by = c("hhid", "item_code")) |>
  left_join(fortification_factors, by = "item_code") |> 
  # Filter to include only fortifiable food vehicles:
  filter(!is.na(vehicle)) |> 
  # Select required variables:
  dplyr::select(hhid, item_code, item_name, vehicle, quantity_100g, proportion_market, prop_fortifiable) 

#--------------------------------------------------------------------------------

# CREATE VEHICLE QUANTITIES DATAFRAME:
vehicle_quantities <- vehicle_consumption |> 
  mutate(quantity_100g = quantity_100g * proportion_market * prop_fortifiable) |>
  group_by(hhid, vehicle) |>
  summarise(quantity_100g = sum(quantity_100g, na.rm = TRUE))

rm(list = setdiff(ls(), c("vehicle_quantities", "afe", "base_ai")))

#--------------------------------------------------------------------------------

# MICRONUTIENT CONTENT OF FORTIFIED VEHICLES:
vehicle100g_intake <- read_excel("metadata/fortification/fortification_factors.xls", 
                                sheet = "vehicle100g_intake") |> 
  pivot_longer(
    cols      = -micronutrient_unit,
    names_to  = "vehicle",
    values_to = "value"
  ) |> 
  pivot_wider(
    names_from  = micronutrient_unit,
    values_from = value
  )

#--------------------------------------------------------------------------------

# JOIN VEHICLE QUANTITIES WITH MICRONUTRIENT CONTENT: 

fortification <- vehicle_quantities |> 
  left_join(vehicle100g_intake, by = "vehicle")

#--------------------------------------------------------------------------------

# COMPUTE MICRONUTRIENT INTAKE FROM FORTIFYING VEHICLES:

# Edible oil:
edible.oil_fortification <- fortification |> 
  left_join(afe, by = "hhid") |>
  filter(vehicle == "edible_oil") |> 
  mutate(eo.vita_rae_mcg = coalesce((quantity_100g * vita_rae_mcg) / afe, 0)) |> 
  dplyr::select(hhid, eo.vita_rae_mcg) 

# Wheat flour:
wheat.flour_fortification <- fortification |>
  left_join(afe, by = "hhid") |>
  filter(vehicle == "wheat_flour") |> 
  mutate(across(
    c(vita_rae_mcg, thia_mg, ribo_mg, niac_mg, vitb6_mg, folate_mcg, vitb12_mcg, fe_mg, zn_mg),
    ~ coalesce((quantity_100g * .x) / afe, 0),
    .names = "wf.{.col}"
  )) |>
  dplyr::select(hhid, starts_with("wf."))

# Maize flour:
maize.flour_fortification <- fortification |>
  left_join(afe, by = "hhid") |>
  filter(vehicle == "maize_flour") |> 
  mutate(across(
    c(vita_rae_mcg, thia_mg, ribo_mg, niac_mg, vitb6_mg, folate_mcg, vitb12_mcg, fe_mg, zn_mg),
    ~ coalesce((quantity_100g * .x) / afe, 0),
    .names = "mf.{.col}"
  )) |>
  dplyr::select(hhid, starts_with("mf."))

rm(afe)

#-------------------------------------------------------------------------------

# FORTIFICATION SCENARIOS:
# Add apparent intake from fortifying vehicles to baseline apparent intake

# Scenario 1: Edible oil fortification only: 
edible.oil_scenario <- base_ai |> 
  left_join(edible.oil_fortification, by = "hhid") |> 
  mutate(vita_rae_mcg = coalesce(vita_rae_mcg + eo.vita_rae_mcg, vita_rae_mcg)) |> 
  dplyr::select(-eo.vita_rae_mcg)

# Scenario 2: Fortification of wheat flour and milled maize products alone: 
flour_scenario <- base_ai |> 
  left_join(wheat.flour_fortification, by = "hhid") |> 
  left_join(maize.flour_fortification, by = "hhid") |> 
  mutate(
    vita_rae_mcg = coalesce(vita_rae_mcg + wf.vita_rae_mcg + mf.vita_rae_mcg, vita_rae_mcg),
    thia_mg = coalesce(thia_mg + wf.thia_mg + mf.thia_mg, thia_mg),
    ribo_mg = coalesce(ribo_mg + wf.ribo_mg + mf.ribo_mg, ribo_mg),
    niac_mg = coalesce(niac_mg + wf.niac_mg + mf.niac_mg, niac_mg),
    vitb6_mg = coalesce(vitb6_mg + wf.vitb6_mg + mf.vitb6_mg, vitb6_mg),
    folate_mcg = coalesce(folate_mcg + wf.folate_mcg + mf.folate_mcg, folate_mcg),
    vitb12_mcg = coalesce(vitb12_mcg + wf.vitb12_mcg + mf.vitb12_mcg, vitb12_mcg),
    fe_mg = coalesce(fe_mg + wf.fe_mg + mf.fe_mg, fe_mg),
    zn_mg = coalesce(zn_mg + wf.zn_mg + mf.zn_mg, zn_mg)
  ) |> 
  dplyr::select(-starts_with("wf."), -starts_with("mf."))

# Scenario 3: Fortification of all three vehicles:
all_vehicles_scenario <- base_ai |> 
  left_join(edible.oil_fortification, by = "hhid") |> 
  left_join(wheat.flour_fortification, by = "hhid") |> 
  left_join(maize.flour_fortification, by = "hhid") |> 
  mutate(
    vita_rae_mcg = coalesce(vita_rae_mcg + eo.vita_rae_mcg + wf.vita_rae_mcg + mf.vita_rae_mcg, vita_rae_mcg),
    thia_mg = coalesce(thia_mg + wf.thia_mg + mf.thia_mg, thia_mg),
    ribo_mg = coalesce(ribo_mg + wf.ribo_mg + mf.ribo_mg, ribo_mg),
    niac_mg = coalesce(niac_mg + wf.niac_mg + mf.niac_mg, niac_mg),
    vitb6_mg = coalesce(vitb6_mg + wf.vitb6_mg + mf.vitb6_mg, vitb6_mg),
    folate_mcg = coalesce(folate_mcg + wf.folate_mcg + mf.folate_mcg, folate_mcg),
    vitb12_mcg = coalesce(vitb12_mcg + wf.vitb12_mcg + mf.vitb12_mcg, vitb12_mcg),
    fe_mg = coalesce(fe_mg + wf.fe_mg + mf.fe_mg, fe_mg),
    zn_mg = coalesce(zn_mg + wf.zn_mg + mf.zn_mg, zn_mg)
  ) |> 
  dplyr::select(-eo.vita_rae_mcg, -starts_with("wf."), -starts_with("mf."))

rm(list = setdiff(ls(), c("base_ai", "edible.oil_scenario", "flour_scenario", "all_vehicles_scenario")))

#-------------------------------------------------------------------------------

# MAP SCENARIOS: 

# Need to read in additional data required for mapping:
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")

# Shapefiles: 
rwa_adm1 <- readRDS("shapefiles/rwa_adm1.rds")
rwa_adm2 <- readRDS("shapefiles/rwa_adm2.rds")
rwa_hh_adm <- readRDS("shapefiles/rwa_hh_adm.rds")

# Source R functions: 
source("src/05base_model_functions.R")

rm(path_to_file, apparent_intake, read_in_survey)

# Define new function for mapping fortification scenarios: 

map_fortification_scenario <- function(scenario_df, 
                                       scenario_name,
                                       output_folder,
                                       bio_avail_fe = 5,
                                       mar_nutrients = c("vita_rae_mcg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg")) {
  
  micronutrients <- c("vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg",
                      "folate_mcg", "vitb12_mcg", "zn_mg")
  
  # Join survey weights and household locations: 
  df <- scenario_df |>
    left_join(hh_information |> dplyr::select(hhid, survey_wgt), by = "hhid") |>
    left_join(rwa_hh_adm, by = "hhid")

  # Binarise inadequacy for micronutrients using EAR approach:
  for (mn in micronutrients) {
    ear_value <- allen_ear$ear_value[allen_ear$nutrient == mn]
    new_col <- paste0(mn, "_inadequate")
    df[[new_col]] <- ifelse(!is.na(df[[mn]]) & df[[mn]] < ear_value, 1, 0)
  }

  # Probability of inadeuqacy for iron: 
  df <- fe_prob_inadequacy(df, bio_avail = bio_avail_fe)

  # survey design and ADM2 prevalence (micronutrients)
  svy <- df |> as_survey_design(weights = survey_wgt)

  # Compute ADM2 level inadequacy and join values to shapefiles: 
  mn_inadequacy <- svy |>
    group_by(adm2) |>
    summarise(
      vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = TRUE, vartype = NULL),
      thia_inadequacy = survey_mean(thia_mg_inadequate, na.rm = TRUE, vartype = NULL),
      ribo_inadequacy = survey_mean(ribo_mg_inadequate, na.rm = TRUE, vartype = NULL),
      niac_inadequacy = survey_mean(niac_mg_inadequate, na.rm = TRUE, vartype = NULL),
      folate_inadequacy = survey_mean(folate_mcg_inadequate, na.rm = TRUE, vartype = NULL),
      vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = TRUE, vartype = NULL),
      fe_inadequacy = survey_mean(fe_prob_inad, na.rm = TRUE, vartype = NULL),
      zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = TRUE, vartype = NULL)
    ) |>
    ungroup() |>
    left_join(rwa_adm2, by = "adm2") |>
    mutate(across(c(vita_inadequacy, thia_inadequacy, ribo_inadequacy, niac_inadequacy,
                    folate_inadequacy, vitb12_inadequacy, fe_inadequacy, zn_inadequacy),
                  ~ round(.x * 100, 1)))
  
  mn_inadequacy <- st_as_sf(mn_inadequacy)

  # Plot maps:
  # VITAMIN A: 
  plot_map(data = mn_inadequacy, col = "vita_inadequacy",
           title = paste("Vitamin A — ", scenario_name), metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, outline_sf = rwa_adm2)
  
  ggsave(paste0("maps/", output_folder, "/vita_inadequacy.png"), width = 8, height = 6)
  
  # THIAMINE:
  plot_map(data = mn_inadequacy, col = "thia_inadequacy",
           title = paste("Thiamine — ", scenario_name), metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, outline_sf = rwa_adm2)
  
  ggsave(paste0("maps/", output_folder, "/thia_inadequacy.png"), width = 8, height = 6)
  
  # RIBOFLAVIN:
  plot_map(data = mn_inadequacy, col = "ribo_inadequacy",
           title = paste("Riboflavin — ", scenario_name), metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, outline_sf = rwa_adm2)
  
  ggsave(paste0("maps/", output_folder, "/ribo_inadequacy.png"), width = 8, height = 6)

  # NIACIN:
  plot_map(data = mn_inadequacy, col = "niac_inadequacy",
           title = paste("Niacin — ", scenario_name), metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, outline_sf = rwa_adm2)
  
  ggsave(paste0("maps/", output_folder, "/niac_inadequacy.png"), width = 8, height = 6)
  
  # FOLATE:
  plot_map(data = mn_inadequacy, col = "folate_inadequacy",
           title = paste("Folate — ", scenario_name), metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, outline_sf = rwa_adm2)
  
  ggsave(paste0("maps/", output_folder, "/folate_inadequacy.png"), width = 8, height = 6)
  
  # VITAMIN B12:
  plot_map(data = mn_inadequacy, col = "vitb12_inadequacy",
           title = paste("Vitamin B12 — ", scenario_name), metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, outline_sf = rwa_adm2)
  
  ggsave(paste0("maps/", output_folder, "/vitb12_inadequacy.png"), width = 8, height = 6)

  # IRON:
  plot_map(data = mn_inadequacy, col = "fe_inadequacy",
           title = paste("Iron — ", scenario_name), metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, outline_sf = rwa_adm2)
  
  ggsave(paste0("maps/", output_folder, "/fe_inadequacy.png"), width = 8, height = 6)
  
  # ZINC:
  plot_map(data = mn_inadequacy, col = "zn_inadequacy",
           title = paste("Zinc — ", scenario_name), metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, outline_sf = rwa_adm2)
  
  ggsave(paste0("maps/", output_folder, "/zn_inadequacy.png"), width = 8, height = 6)
  
  # --- Compute overall MARs ---

  # compute NARs, cap at 1, compute MAR and MAR inadequacy
  nar_df <- df |>
    mutate(
      va_nar   = vita_rae_mcg / allen_ear$ear_value[allen_ear$nutrient == "vita_rae_mcg"],
      fol_nar  = folate_mcg / allen_ear$ear_value[allen_ear$nutrient == "folate_mcg"],
      vb12_nar = vitb12_mcg / allen_ear$ear_value[allen_ear$nutrient == "vitb12_mcg"],
      fe_nar   = fe_mg / allen_ear$ear_value[allen_ear$nutrient == "fe_mg"],
      zn_nar   = zn_mg / allen_ear$ear_value[allen_ear$nutrient == "zn_mg"]
    ) |>
    mutate(across(c(va_nar, fol_nar, vb12_nar, fe_nar, zn_nar), ~ pmin(.x, 1))) |>
    # Compute MAR: 
    mutate(
      mar = rowMeans(across(c(va_nar, fol_nar, vb12_nar, fe_nar, zn_nar)), na.rm = TRUE),
      mar_inadequate = ifelse(mar < 0.75, 1, 0)
    ) 
  
  svy2 <- nar_df |> as_survey_design(ids = hhid, weights = survey_wgt, nest = TRUE)

  # Aggregate MAR at the ADM2 level: 
  mar_adm2 <- svy2 |>
    group_by(adm2) |>
    summarise(mar_inadequate = survey_mean(mar_inadequate, na.rm = TRUE, vartype = NULL)) |>
    ungroup() |>
    mutate(mar_inadequate = round(mar_inadequate * 100, 1)) |>
    left_join(rwa_adm2, by = "adm2") |>
    st_as_sf()
  
  # PLOT MAR: 
  plot_map(
    data = mar_adm2, col = "mar_inadequate",
    title = paste0("Mean Adequacy Ratio (MAR) — ", scenario_name),
    metric = "% At risk of inadequate intake (MAR < 0.75)",
    outline_sf = rwa_adm2
  )

  ggsave(paste0("maps/", output_folder, "/mar_inadequacy.png"), width = 8, height = 6)
  invisible(list(mn_inadequacy = mn_inadequacy, mar_adm2 = mar_adm2))
}

# Example calls:
map_fortification_scenario(edible.oil_scenario, "Scenario 1: Edible oil fortification", "fortification_edible_oil")
map_fortification_scenario(flour_scenario, "Scenario 2: Wheat and Maize flour fortification", "fortification_flours")
map_fortification_scenario(all_vehicles_scenario, "Scenario 3: All vehicles fortification", "fortification_all_vehicles")
