################################################################################
########### SCRIPT FOR EXTRACTING AND CLEANING CONSUMPTION QUANTITIES ##########
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 07-07-2025
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN DATA: 

# Read in food items dictionary: 
food_items <- read_csv("metadata/food_items.csv")

# Read in conversion factors for non-standard units:
conversion_factors <- read_csv("metadata/conversion_factors.csv") |> 
  dplyr::select(item_code, unit, conv_fac)

# Read in edible portions: 
edible_portions <- read_csv("metadata/edible_portions.csv") |> 
  dplyr::select(item_code, edible_portion)

# Food consumption module: 
# Note that 7-day food consumption was captured over 4 visits in a 7-day period:
rwa_food_consumption <- read_dta("raw_data/CS_S8B_Food_Expenditure_Consumption.dta") |> 
  dplyr::select(hhid,
                s8bq0, # Item code
                s8bq6_v2, # Binary consumption (visit 2)
                s8bq7a_v2, # Quantity consumed (visit 2)
                s8bq7b_v2, # Unit (visit 2)
                s8bq6_v3, # Binary consumption (visit 3)
                s8bq7a_v3, # Quantity consumed (visit 3)
                s8bq7b_v3, # Unit (visit 3)
                s8bq6_v4, # Binary consumption (visit 4)
                s8bq7a_v4, # Quantity consumed (visit 4)
                s8bq7b_v4, # Unit (visit 4)
                s8bq6_v5, # Binary consumption (visit 5)
                s8bq7a_v5, # Quantity consumed (visit 5)
                s8bq7b_v5 # Unit (visit 5)
                ) |> 
  rename(item_code = s8bq0,
         consumed_v2 = s8bq6_v2,
         quantity_v2 = s8bq7a_v2,
         unit_v2 = s8bq7b_v2,
         consumed_v3 = s8bq6_v3,
         quantity_v3 = s8bq7a_v3,
         unit_v3 = s8bq7b_v3,
         consumed_v4 = s8bq6_v4,
         quantity_v4 = s8bq7a_v4,
         unit_v4 = s8bq7b_v4,
         consumed_v5 = s8bq6_v5,
         quantity_v5 = s8bq7a_v5,
         unit_v5 = s8bq7b_v5) |> 
  dplyr::left_join(food_items, by = "item_code") |>
  dplyr::select(hhid, item_code, item_name, everything())

rm(food_items)

#-------------------------------------------------------------------------------

# DIVIDE FOOD CONSUMPTION DATA INTO VISITS AND CALCULATE QUANTITIES:
food_consumption_v2 <- rwa_food_consumption |> 
  dplyr::select(hhid, item_code, item_name, consumed_v2, quantity_v2, unit_v2) |> 
  filter(!is.na(quantity_v2)) |>
  # Add unit-conversion factors: 
  left_join(conversion_factors, by = c("item_code" = "item_code", 
                                       "unit_v2" = "unit")) |> 
  # Filter to include consumed foods:
  filter(consumed_v2 == 1) |>
  # Calculate consumption quantities: 
  mutate(quantity_g = quantity_v2 * conv_fac) |> 
  dplyr::select(hhid, item_code, item_name, quantity_g)

# Visit 3: 
food_consumption_v3 <- rwa_food_consumption |> 
  dplyr::select(hhid, item_code, item_name, consumed_v3, quantity_v3, unit_v3) |> 
  filter(!is.na(quantity_v3)) |>
  # Add unit-conversion factors: 
  left_join(conversion_factors, by = c("item_code" = "item_code", 
                                       "unit_v3" = "unit")) |> 
  # Filter to include consumed foods:
  filter(consumed_v3 == 1) |>
  # Calculate consumption quantities:
  mutate(quantity_g = quantity_v3 * conv_fac) |>
  dplyr::select(hhid, item_code, item_name, quantity_g)

# Visit 4:
food_consumption_v4 <- rwa_food_consumption |> 
  dplyr::select(hhid, item_code, item_name, consumed_v4, quantity_v4, unit_v4) |> 
  filter(!is.na(quantity_v4)) |>
  # Add unit-conversion factors: 
  left_join(conversion_factors, by = c("item_code" = "item_code", 
                                       "unit_v4" = "unit")) |> 
  # Filter to include consumed foods:
  filter(consumed_v4 == 1) |>
  # Calculate consumption quantities:
  mutate(quantity_g = quantity_v4 * conv_fac) |>
  dplyr::select(hhid, item_code, item_name, quantity_g)

# Visit 5:
food_consumption_v5 <- rwa_food_consumption |> 
  dplyr::select(hhid, item_code, item_name, consumed_v5, quantity_v5, unit_v5) |> 
  filter(!is.na(quantity_v5)) |>
  # Add unit-conversion factors: 
  left_join(conversion_factors, by = c("item_code" = "item_code", 
                                       "unit_v5" = "unit")) |> 
  # Filter to include consumed foods:
  filter(consumed_v5 == 1) |>
  # Calculate consumption quantities:
  mutate(quantity_g = quantity_v5 * conv_fac) |>
  dplyr::select(hhid, item_code, item_name, quantity_g)

food_consumption <- bind_rows(food_consumption_v2,
                             food_consumption_v3,
                             food_consumption_v4,
                             food_consumption_v5)

# Note that conversion factors were missing for a very small number of items,
# in some cases due to failure to report the unit in the survey, or due to odd
# reporting such as "piece of ground pepper", or "piece of sugar".
# approximately 120 items out of >600,000. Consumption quantity of 
# these items will be NA. This will not result in substantial bias in final 
# results, therefore no further action taken.

# Group food consumption by household and item, and sum quantities:
food_consumption <- food_consumption |> 
  group_by(hhid, item_code, item_name) |> 
  summarise(quantity_g = sum(quantity_g, na.rm = TRUE)) |> 
  ungroup()

# Remove objects no longer required: 
rm(rwa_food_consumption, food_consumption_v2, food_consumption_v3,
   food_consumption_v4, food_consumption_v5, conversion_factors)

#-------------------------------------------------------------------------------

# INTEGRATE EDIBLE PORTION FACTORS: 
food_consumption <- food_consumption |> 
  left_join(edible_portions, by = "item_code") |> 
  mutate(quantity_g = quantity_g * edible_portion) |>
  dplyr::select(-edible_portion)

rm(edible_portions)

#-------------------------------------------------------------------------------

# WRITE CSV: 
write_csv(food_consumption, "processed_data/rwa_eicv2324_food_consumption.csv")

# clear environment 
rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################
