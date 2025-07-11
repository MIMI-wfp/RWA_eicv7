library(readr)
library(tidyverse)

# READ DATA: 
sen_nsu <- read_csv("metadata/senegal_factors/ehcvm_nsu_sen2021.csv")
mapping <- read_csv("metadata/senegal_factors/mapping.csv") |> 
  dplyr::select(item_code, name_eng, edible_portion)

# Add english names: 
sen_nsu <- sen_nsu |> 
  left_join(mapping, by = c("produitID" = "item_code"))

#-------------------------------------------------------------------------------

# Get conversion factors for pieces of particular food items: 

# GARLIC: 
garlic <- sen_nsu |> 
  filter(name_eng == "Garlic") |> 
  # Filter for pieces: 
  filter(uniteID == 147) |> 
  # Use medium pieces: 
  filter(tailleID == 2)

# Get mean weight
mean(garlic$poids) # 34.5grams

# EGGPLANT: 
eggplant <- sen_nsu |> 
  filter(name_eng == "Eggplant") |> 
  # Filter for pieces: 
  filter(uniteID == 147) |> 
  # Use medium pieces: 
  filter(tailleID == 2)

# Get mean weight
mean(eggplant$poids) # 383grams

# FRESH FISH: 
fresh_fish <- sen_nsu |> 
  filter(name_eng == "Other Fresh Fish") |> 
  # Filter for pieces: 
  filter(uniteID == 147) |> 
  # Use medium pieces: 
  filter(tailleID == 2)

# Get mean weight
mean(fresh_fish$poids) #978grams

# FRESH PEPPER: 
fresh_pepper <- sen_nsu |> 
  filter(name_eng == "Fresh Bell Pepper") |> 
  # Filter for pieces: 
  filter(uniteID == 147) |> 
  # Use medium pieces: 
  filter(tailleID == 2)

# Get mean weight
mean(fresh_pepper$poids) #75grams

# BOUILLON (MAGGI): 
bouillon <- sen_nsu |> 
  filter(produitID == 144) |> 
  # Filter for cubes:
  filter(uniteID == 274) |> 
  # Use medium pieces: 
  filter(tailleID == 2)

# Get mean weight
mean(bouillon$poids) # 10grams

# Lettuce: 
lettuce <- sen_nsu |> 
  filter(name_eng == "Lettuce") |> 
  # Filter for pieces: 
  filter(uniteID == 147) |> 
  # Use medium pieces: 
  filter(tailleID == 2)

# Get mean weight
mean(lettuce$poids) #290grams

# Other fruits: 
other_fruits <- sen_nsu |> 
  filter(name_eng == "Other Fruits") |> 
  # Filter for pieces: 
  filter(uniteID == 147) |> 
  # Use medium pieces: 
  filter(tailleID == 2)

# Get mean weight
mean(other_fruits$poids) # 150grams

rm(list = ls())
