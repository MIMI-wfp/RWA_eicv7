library(readxl)
library(tidyverse)

# Script for preparing unit conversion factors. Using Malawi IHS4 as data not
# available from Rwanda.

mwi_factors <- read_excel("metadata/malawi_factors/ihs4factors_v5.xls")
mwi_waste_factors <- read_excel("metadata/malawi_factors/mwi_waste_factor.xlsx")

# Select relevant variables: 
mwi_factors <- mwi_factors |> 
  dplyr::select(fcode, item, unit, ucode, ihs4factor_c, ihs4factor_n, ihs4factor_s) |> 
  filter(ucode == 9 | ucode == 15 | ucode == 51)

# Take average factor: 
mwi_factors <- mwi_factors |> 
  mutate(average_factor_kg = (ihs4factor_c + ihs4factor_n + ihs4factor_s) / 3) |> 
  dplyr::select(fcode, item, unit, ucode, average_factor_kg) |> 
  rename(item_code = fcode,
         unit_code = ucode)

# Join waste factors: 
mwi_factors <- mwi_factors |> 
  full_join(mwi_waste_factors, by = "item_code")

# Write csv: 
write_csv(mwi_factors, "metadata/malawi_factors/mwi_factors.csv")

rm(list = ls())

################################# END OF SCRIPT ################################