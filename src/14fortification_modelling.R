################################################################################
################### SCRIPT FOR FORTIFICATION SCENARIO MODELLING ################
################################################################################

# Author: Mo Osman
# Date created: 11-Mar-2026
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "readxl", "haven",
                 "wesanderson", "sf", "cowplot", "gt")

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
                                    sheet = "fortification_factors") |> 
  dplyr::select(-source)

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

rm(cf_vec, food_cons8b, conversion_factors)

#--------------------------------------------------------------------------------

# JOIN FOOD CONSUMPTION, PROPORTION MARKET ACQUIRED, AND FORTIFICATION FACTORS DATAFRAMES:
vehicle_consumption <- food_consumption |> 
  left_join(proportion_market, by = c("hhid", "item_code")) |>
  left_join(fortification_factors, by = c("item_code")) |> 
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

rm(list = setdiff(ls(), c("vehicle_quantities", "cflour_quantities", "afe", "base_ai")))

#--------------------------------------------------------------------------------

# READ IN MICRONUTIENT CONTENT OF FORTIFIED VEHICLES:
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

# COMPUTE MICRONUTRIENT INTAKE FROM FORTIFYING INDIVIDUAL VEHICLES:

# Edible oil:
edible.oil_fortification <- fortification |> 
  left_join(afe, by = "hhid") |>
  filter(vehicle == "edible_oil") |> 
  mutate(eo.vita_rae_mcg = coalesce((quantity_100g * vita_rae_mcg) / afe, 0)) |> 
  dplyr::select(hhid, eo.vita_rae_mcg) 

edible.oil_scenario <- base_ai |> 
  left_join(edible.oil_fortification, by = "hhid") |> 
  mutate(vita_rae_mcg = vita_rae_mcg + eo.vita_rae_mcg) |> 
  dplyr::select(-eo.vita_rae_mcg)

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

wheat.flour_scenario <- base_ai |> 
  left_join(wheat.flour_fortification, by = "hhid") |> 
  mutate(
    vita_rae_mcg = vita_rae_mcg + wf.vita_rae_mcg,
    thia_mg = thia_mg + wf.thia_mg,
    ribo_mg = ribo_mg + wf.ribo_mg,
    niac_mg = niac_mg + wf.niac_mg,
    vitb6_mg = vitb6_mg + wf.vitb6_mg,
    folate_mcg = folate_mcg + wf.folate_mcg,
    vitb12_mcg = vitb12_mcg + wf.vitb12_mcg,
    fe_mg = fe_mg + wf.fe_mg,
    zn_mg = zn_mg + wf.zn_mg
  ) |> 
  dplyr::select(-starts_with("wf."))

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

maize.flour_scenario <- base_ai |>
  left_join(maize.flour_fortification, by = "hhid") |> 
  mutate(
    vita_rae_mcg = vita_rae_mcg + mf.vita_rae_mcg,
    thia_mg = thia_mg + mf.thia_mg,
    ribo_mg = ribo_mg + mf.ribo_mg,
    niac_mg = niac_mg + mf.niac_mg,
    vitb6_mg = vitb6_mg + mf.vitb6_mg,
    folate_mcg = folate_mcg + mf.folate_mcg,
    vitb12_mcg = vitb12_mcg + mf.vitb12_mcg,
    fe_mg = fe_mg + mf.fe_mg,
    zn_mg = zn_mg + mf.zn_mg
  ) |> 
  dplyr::select(-starts_with("mf."))

# Composite flours: 
composite.flour_fortification <- fortification |>
  left_join(afe, by = "hhid") |>
  filter(vehicle == "composite_flour") |> 
  mutate(across(
    c(vita_rae_mcg, thia_mg, ribo_mg, niac_mg, vitb6_mg, folate_mcg, vitb12_mcg, fe_mg, zn_mg),
    ~ coalesce((quantity_100g * .x) / afe, 0),
    .names = "cf.{.col}"
  )) |>
  dplyr::select(hhid, starts_with("cf."))

 composite.flour_scenario <- base_ai |>
  left_join(composite.flour_fortification, by = "hhid") |> 
  mutate(
    vita_rae_mcg = vita_rae_mcg + cf.vita_rae_mcg,
    thia_mg = thia_mg + cf.thia_mg,
    ribo_mg = ribo_mg + cf.ribo_mg,
    niac_mg = niac_mg + cf.niac_mg,
    vitb6_mg = vitb6_mg + cf.vitb6_mg,
    folate_mcg = folate_mcg + cf.folate_mcg,
    vitb12_mcg = vitb12_mcg + cf.vitb12_mcg,
    fe_mg = fe_mg + cf.fe_mg,
    zn_mg = zn_mg + cf.zn_mg
  ) |> 
  dplyr::select(-starts_with("cf."))

# Rice: 
rice_fortification <- fortification |>
  left_join(afe, by = "hhid") |>
  filter(vehicle == "rice") |> 
  mutate(across(
    c(vita_rae_mcg, thia_mg, ribo_mg, niac_mg, vitb6_mg, folate_mcg, vitb12_mcg, fe_mg, zn_mg),
    ~ coalesce((quantity_100g * .x) / afe, 0),
    .names = "rice.{.col}"
  )) |>
  dplyr::select(hhid, starts_with("rice."))

rice_scenario <- base_ai |>
  left_join(rice_fortification, by = "hhid") |> 
  mutate(
    vita_rae_mcg = vita_rae_mcg + rice.vita_rae_mcg,
    thia_mg = thia_mg + rice.thia_mg,
    ribo_mg = ribo_mg + rice.ribo_mg,
    niac_mg = niac_mg + rice.niac_mg,
    vitb6_mg = vitb6_mg + rice.vitb6_mg,
    folate_mcg = folate_mcg + rice.folate_mcg,
    vitb12_mcg = vitb12_mcg + rice.vitb12_mcg,
    fe_mg = fe_mg + rice.fe_mg,
    zn_mg = zn_mg + rice.zn_mg
  ) |> 
  dplyr::select(-starts_with("rice."))

# Sorghum:
# ** NO STANDARDS DEVELOPED, THEREFORE NOT POSSIBLE TO MODEL AT PRESENT **

# Cassava flour:
cassava.flour_fortification <- fortification |>
  left_join(afe, by = "hhid") |>
  filter(vehicle == "cassava_flour") |> 
  mutate(across(
    c(vita_rae_mcg, thia_mg, ribo_mg, niac_mg, vitb6_mg, folate_mcg, vitb12_mcg, fe_mg, zn_mg),
    ~ coalesce((quantity_100g * .x) / afe, 0),
    .names = "cassava.f.{.col}"
  )) |> 
  dplyr::select(hhid, starts_with("cassava.f."))

cassava.flour_scenario <- base_ai |>
  left_join(cassava.flour_fortification, by = "hhid") |> 
  mutate(
    vita_rae_mcg = vita_rae_mcg + cassava.f.vita_rae_mcg,
    thia_mg = thia_mg + cassava.f.thia_mg,
    ribo_mg = ribo_mg + cassava.f.ribo_mg,
    niac_mg = niac_mg + cassava.f.niac_mg,
    vitb6_mg = vitb6_mg + cassava.f.vitb6_mg,
    folate_mcg = folate_mcg + cassava.f.folate_mcg,
    vitb12_mcg = vitb12_mcg + cassava.f.vitb12_mcg,
    fe_mg = fe_mg + cassava.f.fe_mg,
    zn_mg = zn_mg + cassava.f.zn_mg
  ) |> 
  dplyr::select(-starts_with("cassava.f."))

#-------------------------------------------------------------------------------

# BIOFORTIFIED FOODS:

# BEANS
bean_biofortification <- fortification |>
  left_join(afe, by = "hhid") |>
  filter(vehicle == "dry_bean") |> 
  mutate(across(
    c(vita_rae_mcg, thia_mg, ribo_mg, niac_mg, vitb6_mg, folate_mcg, vitb12_mcg, fe_mg, zn_mg),
    ~ coalesce((quantity_100g * .x) / afe, 0),
    .names = "beans.{.col}"
  )) |>
  dplyr::select(hhid, starts_with("beans."))

bean_scenario <- base_ai |>
  left_join(bean_biofortification, by = "hhid") |> 
  mutate(
    vita_rae_mcg = vita_rae_mcg + beans.vita_rae_mcg,
    thia_mg = thia_mg + beans.thia_mg,
    ribo_mg = ribo_mg + beans.ribo_mg,
    niac_mg = niac_mg + beans.niac_mg,
    vitb6_mg = vitb6_mg + beans.vitb6_mg,
    folate_mcg = folate_mcg + beans.folate_mcg,
    vitb12_mcg = vitb12_mcg + beans.vitb12_mcg,
    fe_mg = fe_mg + beans.fe_mg,
    zn_mg = zn_mg + beans.zn_mg
  ) |> 
  dplyr::select(-starts_with("beans."))

# SWEET POTATOES
sweet.potato_biofortification <- fortification |>
  left_join(afe, by = "hhid") |>
  filter(vehicle == "sweet_potato") |> 
  mutate(across(
    c(vita_rae_mcg, thia_mg, ribo_mg, niac_mg, vitb6_mg, folate_mcg, vitb12_mcg, fe_mg, zn_mg),
    ~ coalesce((quantity_100g * .x) / afe, 0),
    .names = "sweetpotato.{.col}"
  )) |>
  dplyr::select(hhid, starts_with("sweetpotato."))

sweet.potato_scenario <- base_ai |>
  left_join(sweet.potato_biofortification, by = "hhid") |> 
  mutate(
    vita_rae_mcg = vita_rae_mcg + sweetpotato.vita_rae_mcg,
    thia_mg = thia_mg + sweetpotato.thia_mg,
    ribo_mg = ribo_mg + sweetpotato.ribo_mg,
    niac_mg = niac_mg + sweetpotato.niac_mg,
    vitb6_mg = vitb6_mg + sweetpotato.vitb6_mg,
    folate_mcg = folate_mcg + sweetpotato.folate_mcg,
    vitb12_mcg = vitb12_mcg + sweetpotato.vitb12_mcg,
    fe_mg = fe_mg + sweetpotato.fe_mg,
    zn_mg = zn_mg + sweetpotato.zn_mg
  ) |> 
  dplyr::select(-starts_with("sweetpotato."))

rm(afe)

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

  # Also compute national level inadequacy for labelling maps: 
  national_mn_inadequacy <- svy |>
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
    mutate(across(everything(), ~ round(.x * 100, 1)))

  # Plot maps:
  # VITAMIN A: 
  vita_map <- plot_map(data = mn_inadequacy, 
           col = "vita_inadequacy",
           title = paste0("Vitamin A (", national_mn_inadequacy$vita_inadequacy, "%) — ", scenario_name), 
           metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, 
           outline_sf = rwa_adm1)
  
  vita_map
  
  ggsave(paste0("maps/", output_folder, "/vita_inadequacy.png"), width = 8, height = 6)
  
  # THIAMINE:
  thia_map <- plot_map(data = mn_inadequacy, 
           col = "thia_inadequacy",
           title = paste0("Thiamine (", national_mn_inadequacy$thia_inadequacy, "%) — ", scenario_name), 
           metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, 
           outline_sf = rwa_adm1)
  
  thia_map
  
  ggsave(paste0("maps/", output_folder, "/thia_inadequacy.png"), width = 8, height = 6)
  
  # RIBOFLAVIN:
  ribo_map <- plot_map(data = mn_inadequacy, 
           col = "ribo_inadequacy",
           title = paste0("Riboflavin (", national_mn_inadequacy$ribo_inadequacy, "%) — ", scenario_name), 
           metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, 
           outline_sf = rwa_adm1)
  
  ribo_map
  
  ggsave(paste0("maps/", output_folder, "/ribo_inadequacy.png"), width = 8, height = 6)

  # NIACIN:
  niac_map <- plot_map(data = mn_inadequacy, 
           col = "niac_inadequacy",
           title = paste0("Niacin (", national_mn_inadequacy$niac_inadequacy, "%) — ", scenario_name), 
           metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, 
           outline_sf = rwa_adm1)
  
  niac_map
  
  ggsave(paste0("maps/", output_folder, "/niac_inadequacy.png"), width = 8, height = 6)
  
  # FOLATE:
  folate_map <- plot_map(data = mn_inadequacy, 
           col = "folate_inadequacy",
           title = paste0("Folate (", national_mn_inadequacy$folate_inadequacy, "%) — ", scenario_name), 
           metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, 
           outline_sf = rwa_adm1)
  
  folate_map
  
  ggsave(paste0("maps/", output_folder, "/folate_inadequacy.png"), width = 8, height = 6)
  
  # VITAMIN B12:
  vitb12_map <- plot_map(data = mn_inadequacy, 
           col = "vitb12_inadequacy",
           title = paste0("Vitamin B12 (", national_mn_inadequacy$vitb12_inadequacy, "%) — ", scenario_name), 
           metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, 
           outline_sf = rwa_adm1)
  
  vitb12_map
  
  ggsave(paste0("maps/", output_folder, "/vitb12_inadequacy.png"), width = 8, height = 6)

  # IRON:
  fe_map <- plot_map(data = mn_inadequacy, 
           col = "fe_inadequacy",
           title = paste0("Iron (", national_mn_inadequacy$fe_inadequacy, "%) — ", scenario_name), 
           metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, 
           outline_sf = rwa_adm1)
  
  fe_map
  
  ggsave(paste0("maps/", output_folder, "/fe_inadequacy.png"), width = 8, height = 6)
  
  # ZINC:
  zn_map <- plot_map(data = mn_inadequacy, 
           col = "zn_inadequacy",
           title = paste0("Zinc (", national_mn_inadequacy$zn_inadequacy, "%) — ", scenario_name), 
           metric = "Risk of inadequate intake (%)",
           add_labels = FALSE, 
           outline_sf = rwa_adm1)
  
  zn_map
  
  ggsave(paste0("maps/", output_folder, "/zn_inadequacy.png"), width = 8, height = 6)

  grid_plot <- plot_grid(vita_map, thia_map, ribo_map, niac_map, folate_map, vitb12_map, fe_map, zn_map,
                         ncol = 4)
  
  grid_plot

  ggsave(paste0("maps/", output_folder, "/mn_inadequacy_grid.png"), width = 20, height = 8)
  
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

  # Also compute MAR nationally for labelling maps:
  national_mar_inadequacy <- svy2 |>
    summarise(mar_inadequate = survey_mean(mar_inadequate, na.rm = TRUE, vartype = NULL)) |>
    ungroup() |>
    mutate(mar_inadequate = round(mar_inadequate * 100, 1))
  
  # PLOT MAR: 
  plot_map(
    data = mar_adm2, col = "mar_inadequate",
    title = paste0("Mean Adequacy Ratio (MAR) < 0.75 (", national_mar_inadequacy$mar_inadequate, "%) — ", scenario_name),
    metric = "% At risk of inadequate intake (MAR < 0.75)",
    outline_sf = rwa_adm1
  )

  ggsave(paste0("maps/", output_folder, "/mar_inadequacy.png"), width = 8, height = 6)
  invisible(list(mn_inadequacy = mn_inadequacy, mar_adm2 = mar_adm2))

}

#--------------------------------------------------------------------------------

# MAP SCENARIOS:
map_fortification_scenario(edible.oil_scenario, "Edible Oil Fortification", "fortification_scenarios/edible_oil")
map_fortification_scenario(wheat.flour_scenario, "Wheat Flour Fortification", "fortification_scenarios/wheat_flour")
map_fortification_scenario(maize.flour_scenario, "Maize Flour Fortification", "fortification_scenarios/maize_flour")
map_fortification_scenario(composite.flour_scenario, "Composite Flour Fortification", "fortification_scenarios/composite_flour")
map_fortification_scenario(rice_scenario, "Rice Fortification", "fortification_scenarios/rice")
map_fortification_scenario(cassava.flour_scenario, "Cassava Flour Fortification", "fortification_scenarios/cassava_flour")
map_fortification_scenario(bean_scenario, "Bean Biofortification", "biofortification_scenarios/iron_beans")
map_fortification_scenario(sweet.potato_scenario, "Sweet Potato Biofortification", "biofortification_scenarios/sweet_potato")

#--------------------------------------------------------------------------------

# SUMMARISE SCENARIOS AT NATIONAL LEVEL IN TABULAR FORM:

national_inadequacy_summary <- function(scenario_df, scenario_name, bio_avail_fe = 5) {

  micronutrients <- c("vita_rae_mcg", "folate_mcg", "vitb12_mcg", "zn_mg")

  df <- scenario_df |>
    left_join(hh_information |> dplyr::select(hhid, survey_wgt), by = "hhid")

  # EAR cut-point inadequacy:
  for (mn in micronutrients) {
    ear_value <- allen_ear$ear_value[allen_ear$nutrient == mn]
    df[[paste0(mn, "_inadequate")]] <- ifelse(!is.na(df[[mn]]) & df[[mn]] < ear_value, 1, 0)
  }

  # Probability of inadequacy for iron:
  df <- fe_prob_inadequacy(df, bio_avail = bio_avail_fe)

  # NAR / MAR:
  df <- df |>
    mutate(
      va_nar = pmin(vita_rae_mcg / allen_ear$ear_value[allen_ear$nutrient == "vita_rae_mcg"], 1),
      fol_nar = pmin(folate_mcg / allen_ear$ear_value[allen_ear$nutrient == "folate_mcg"], 1),
      vb12_nar = pmin(vitb12_mcg / allen_ear$ear_value[allen_ear$nutrient == "vitb12_mcg"], 1),
      fe_nar = pmin(fe_mg / allen_ear$ear_value[allen_ear$nutrient == "fe_mg"], 1),
      zn_nar = pmin(zn_mg / allen_ear$ear_value[allen_ear$nutrient == "zn_mg"], 1)
    ) |>
    mutate(
      mar = rowMeans(across(c(va_nar, fol_nar, vb12_nar, fe_nar, zn_nar)), na.rm = TRUE),
      mar_inadequate = ifelse(mar < 0.75, 1, 0)
    )

  svy <- df |> as_survey_design(weights = survey_wgt)

  svy |>
    summarise(
      vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = TRUE, vartype = NULL),
      folate_inadequacy = survey_mean(folate_mcg_inadequate, na.rm = TRUE, vartype = NULL),
      vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = TRUE, vartype = NULL),
      fe_inadequacy = survey_mean(fe_prob_inad, na.rm = TRUE, vartype = NULL),
      zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = TRUE, vartype = NULL),
      mar_inadequacy = survey_mean(mar_inadequate, na.rm = TRUE, vartype = NULL)
    ) |>
    mutate(
      scenario = scenario_name,
      across(c(vita_inadequacy, folate_inadequacy, vitb12_inadequacy,
               fe_inadequacy, zn_inadequacy, mar_inadequacy),
             ~ round(.x * 100, 1))
    ) |>
    dplyr::select(scenario, everything())
}

# Compute national summaries for all scenarios:
scenario_summary <- bind_rows(
  national_inadequacy_summary(base_ai, "Base Case"),
  national_inadequacy_summary(edible.oil_scenario, "Edible Oil Fortification"),
  national_inadequacy_summary(wheat.flour_scenario, "Wheat Flour Fortification"),
  national_inadequacy_summary(maize.flour_scenario, "Maize Flour Fortification"),
  national_inadequacy_summary(composite.flour_scenario, "Composite Flour Fortification"),
  national_inadequacy_summary(rice_scenario, "Rice Fortification"),
  national_inadequacy_summary(cassava.flour_scenario, "Cassava Flour Fortification"),
  national_inadequacy_summary(bean_scenario, "Bean Biofortification"),
  national_inadequacy_summary(sweet.potato_scenario, "Sweet Potato Biofortification")
)

# Produce gt summary table:
scenario_table <- scenario_summary |>
  gt() |>
  cols_label(
    scenario = "Scenario",
    vita_inadequacy = "Vitamin A",
    folate_inadequacy = "Folate",
    vitb12_inadequacy = "Vitamin B12",
    fe_inadequacy = "Iron",
    zn_inadequacy = "Zinc",
    mar_inadequacy = "MAR < 0.75"
  ) |>
  tab_spanner(
    label   = md("**Households at risk of inadequate dietary micronutrient intake (%) — National level**"),
    columns = c(vita_inadequacy, folate_inadequacy, vitb12_inadequacy,
                fe_inadequacy, zn_inadequacy, mar_inadequacy)
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

scenario_table

# Save table as image:
gtsave(scenario_table, "maps/fortification_scenarios/scenario_comparison_table.png")

# Clear environment: 
rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################
