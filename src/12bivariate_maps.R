################################################################################
############# SCRIPT FOR PRODUCING MAPS - RISK AND BIVARIATE MAPS ##############
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 20-10-2025
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "spdep", "sf", "wesanderson",
                 "srvyr", "plotly", "biscale", "cowplot", "readxl")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ SHAPEFILES:
rwa_adm1 <- read_rds("shapefiles/rwa_adm1.rds")
rwa_adm2 <- read_rds("shapefiles/rwa_adm2.rds")

#--------------------------------------------------------------------------------- 

# READ DATA: 
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")
rwa_hh_adm <- readRDS("shapefiles/rwa_hh_adm.rds")

fortification_factors <- read_excel("metadata/fortification/fortification_factors.xls", 
                                    sheet = "fortification_factors")

#---------------------------------------------------------------------------------

# BIVARIATE MAPS - PROCESS FOOD CONSUMPTION DATA:
process_food_consumption2 <- function(hh_info_path, cons_path) {
  hh_info <- read.csv(hh_info_path)
  
  cons_data <- read.csv(cons_path) |> 
    left_join(hh_info, by = "hhid") |> 
    mutate(quantity_g = quantity_g / afe) |>
    left_join(fortification_factors |> 
      dplyr::select(item_code, vehicle, prop_fortifiable), 
    by = "item_code") |>
    mutate(quantity_g = quantity_g * prop_fortifiable) |>
    select(hhid, item_code, vehicle, quantity_g, adm2, survey_wgt, ea, res) |> 
    filter(!is.na(vehicle)) |> 
    group_by(hhid, vehicle, adm2, survey_wgt, ea, res) |>
    summarise(quantity_g = sum(quantity_g, na.rm = TRUE), .groups = "drop")
  
  return(cons_data)
}

#-----------------------------------------------------------------------------------

vehicle_consumption_quantities <- process_food_consumption2(
  hh_info_path = "processed_data/rwa_eicv2324_hh_information.csv",
  cons_path    = "processed_data/rwa_eicv2324_food_consumption.csv"
) |> 
  dplyr::select(-adm2) |> 
  left_join(
    rwa_hh_adm |> 
      dplyr::select(hhid, adm2), 
    by = "hhid"
  )

binary_vehicle_consumption <- vehicle_consumption_quantities |> 
  dplyr::select(hhid, vehicle) |> 
  pivot_wider(names_from = vehicle, values_from = vehicle, values_fill = list(vehicle = "0")) |> 
  mutate(across(-hhid, ~ifelse(. != "0", 1, 0))) |> 
  left_join(hh_information |> dplyr::select(hhid, survey_wgt, ea, res), by = "hhid") |> 
  left_join(rwa_hh_adm |> dplyr::select(hhid, adm2), by = "hhid")

#-------------------------------------------------------------------------------

# COMPUTE REACH AT ADM2 LEVEL: 

binary_vehicle_consumption.svy <- binary_vehicle_consumption |> 
  as_survey_design(weights = survey_wgt)

vehicle_reach <- binary_vehicle_consumption.svy |> 
  group_by(adm2) |> 
  summarise(
    across(beans:sorghum, ~ survey_mean(.x, proportion = TRUE) * 100)
  ) |> 
  dplyr::select(adm2, composite_flour, edible_oil, rice, wheat_flour, cassava_flour, 
                maize_flour, sorghum, sweet_potato, beans)

#-------------------------------------------------------------------------------

# COMPUTE CONSUMTION OF EACH VEHICLE AT ADM2 LEVEL:
vehicle_consumption_quantities.svy <- vehicle_consumption_quantities |> 
  as_survey_design(weights = survey_wgt)

vehicle_intake <- vehicle_consumption_quantities.svy |>
  group_by(adm2, vehicle) |> 
  summarise(mean_quantity_g = survey_mean(quantity_g, na.rm = TRUE), .groups = "drop") |> 
  select(-mean_quantity_g_se) |> 
  pivot_wider(names_from = vehicle, values_from = mean_quantity_g, values_fill = list(mean_quantity_g = 0))

# Tidy environment: 
rm(list = setdiff(ls(), c("vehicle_reach", "vehicle_intake", "rwa_adm1", "rwa_adm2", "rwa_hh_adm")))

#-------------------------------------------------------------------------------

# CREATE BICLASS: 

# For each vehicle: 
vehicle_list <- c("composite_flour", "edible_oil", "rice", "wheat_flour", "cassava_flour", 
                 "maize_flour", "sorghum", "sweet_potato", "beans")

# Create empty list to store bivariate data frames
bivariate_data_list <- list()

for (i in vehicle_list) {
  temp_data <- vehicle_reach |> 
    dplyr::select(adm2, i) |>
    rename(reach_pct = i) |>
    left_join(
      vehicle_intake |> 
        dplyr::select(adm2, i) |> 
        rename(mean_quantity_g = i), 
      by = "adm2"
    ) |>
    mutate(
      reach_bins = cut(
        reach_pct, 
        breaks = c(0, 25, 50, 75, 100), 
        include.lowest = TRUE
      ),
      intake_bins = cut(
        mean_quantity_g, 
        breaks = c(-Inf, 75, 149, 300, Inf),
        labels = c("<75",  "75–149",  "150–300",  ">300"),
        include.lowest = TRUE
      ),
      adm2 = as.character(adm2)
    ) |>
    left_join(rwa_adm2, by = "adm2") |>
    st_as_sf() |>
    bi_class(x = reach_bins, y = intake_bins, dim = 4)

  bivariate_data_list[[paste0("bivariate_", i)]] <- temp_data

}

#-------------------------------------------------------------------------------

# Iterate through list to create bivariate maps for each vehicle: 
for (i in vehicle_list) {
  temp_map <- ggplot() + 
    geom_sf(data = bivariate_data_list[[paste0("bivariate_", i)]], mapping = aes(fill = bi_class), color = NA, show.legend = F) +
    bi_scale_fill(pal = "BlueGold", dim = 4) +
    bi_theme() +
    geom_sf(data = rwa_adm1, fill= NA, color = 'black', lwd = 1) + 
    # geom_sf_text(data = rwa_adm1, aes(label = adm1), size = 3, color = 'black', fontface = 'bold') +
    labs(subtitle = tools::toTitleCase(str_replace_all(i, "_", " ")))
  
  print(temp_map)

  # Save: 
  ggsave(filename = paste0("maps/bivariate/", i, "_bivariate_map.png"),
    plot = temp_map,
    width = 10,
    height = 8,
    dpi = 300)

  rm(temp_map)

}

#-------------------------------------------------------------------------------

# CREATE LEGEND:

# create a df of the breaks for each exis
break_vals <- bi_class_breaks(.data = bivariate_data_list[["bivariate_cassava_flour"]], x =reach_bins , y = intake_bins, dim = 4 )

#create a bivariate legend
bivariate_legend <- bi_legend(pal = "BlueGold",
                          dim = 4,
                          xlab = "Higher Reach (%) ",
                          ylab = "Higher Consumption (g) ",
                          size = 8, 
                          breaks = break_vals)

print(bivariate_legend)

ggsave(filename = "maps/bivariate/bivariate_legend.png",
  plot = bivariate_legend,
  width = 3,
  height = 3,
  dpi = 300)

#--------------------------------------------------------------------------------

# Also create a grid of all the bivariate maps with the legend:
# Firstly for LSFF vehicles only: 
lsff_vehicles <- c("wheat_flour", "composite_flour", "maize_flour", "rice",
                   "cassava_flour", "sorghum")

lsff_bivariate_maps <- lapply(lsff_vehicles, function(i) {
  ggplot() + 
    geom_sf(data = bivariate_data_list[[paste0("bivariate_", i)]], mapping = aes(fill = bi_class), color = NA, show.legend = F) +
    bi_scale_fill(pal = "BlueGold", dim = 4) +
    bi_theme() +
    geom_sf(data = rwa_adm1, fill= NA, color = 'black', lwd = 1) + 
    labs(subtitle = tools::toTitleCase(str_replace_all(i, "_", " ")))
})

lsff_bivariate_grid <- plot_grid(plotlist = lsff_bivariate_maps, ncol = 3)

lsff_bivariate_grid

ggsave(filename = "maps/bivariate/lsff_bivariate_grid.png",
  plot = lsff_bivariate_grid,
  width = 15,
  height = 8,
  dpi = 300)

# Then for sweet potato and beans:
biofortification_foods <- c("sweet_potato", "beans")

biofort_bivariate_maps <- lapply(biofortification_foods, function(i) {
  ggplot() + 
    geom_sf(data = bivariate_data_list[[paste0("bivariate_", i)]], mapping = aes(fill = bi_class), color = NA, show.legend = F) +
    bi_scale_fill(pal = "BlueGold", dim = 4) +
    bi_theme() +
    geom_sf(data = rwa_adm1, fill= NA, color = 'black', lwd = 1) + 
    labs(subtitle = tools::toTitleCase(str_replace_all(i, "_", " ")))
})

biofort_bivariate_grid <- plot_grid(plotlist = biofort_bivariate_maps, ncol = 2)
biofort_bivariate_grid

ggsave(filename = "maps/bivariate/biofort_bivariate_grid.png",
  plot = biofort_bivariate_grid,
  width = 10,
  height = 8,
  dpi = 300)

rm(list = ls())

################################################################################
########################### END OF SCRIPT ######################################
################################################################################