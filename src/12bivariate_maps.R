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
                 "srvyr", "plotly", "biscale", "cowplot")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Source custom functions:
source("src/05base_model_functions.R")

rm(list = setdiff(ls(), c("plot_nutrient_map", "allen_ear")))

#-------------------------------------------------------------------------------

# READ SHAPEFILES:
rwa_adm1 <- read_rds("shapefiles/rwa_adm1.rds")
rwa_adm2 <- read_rds("shapefiles/rwa_adm2.rds")

#--------------------------------------------------------------------------------- 

# READ DATA: 
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")
rwa_hh_adm <- readRDS("shapefiles/rwa_hh_adm.rds")

#---------------------------------------------------------------------------------

# BIVARIATE MAPS - PROCESS FOOD CONSUMPTION DATA:
process_food_consumption2 <- function(hh_info_path, cons_path) {
  hh_info <- read.csv(hh_info_path)
  
  cons_data <- read.csv(cons_path) %>%
    left_join(hh_info, by = "hhid") %>%
    mutate(quantity_g = quantity_g / afe) %>%
    select(hhid, item_code, quantity_g, adm2, survey_wgt, ea, res)
  
  return(cons_data)
}

#-----------------------------------------------------------------------------------

# BIVARIATE MAPS - FUCTIONS: 

calculate_wheat_reach <- function(data, wheat_codes, hhid_col, adm1_col, survey_wgt_col, ea_col, res_col, item_code_col){
  
  # Mark wheat consumers
  wheat <- data %>%
    filter({{item_code_col}} %in% wheat_codes) %>%
    mutate(consumed_wheat = 1)
  
  # Mark non-wheat consumers and combine
  all_wheat <- data %>%
    filter(!( {{hhid_col}} %in% wheat[[rlang::as_name(enquo(hhid_col))]] )) %>%
    mutate(consumed_wheat = 0) %>%
    bind_rows(wheat)
  
  # Collapse to one row per household
  hh_wheat_status <- all_wheat %>%
    group_by({{hhid_col}}, {{adm1_col}}, {{survey_wgt_col}}, {{ea_col}}, {{res_col}}) %>%
    summarise(consumed_wheat = max(consumed_wheat), .groups = "drop")
  
  # Survey design and calculate reach
  reach_wheat <- hh_wheat_status %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, , nest = T) %>%
    group_by({{adm1_col}}) %>%
    summarise(wheat_reach_pct = survey_mean(consumed_wheat, proportion = TRUE) * 100) %>%
    select(-wheat_reach_pct_se)
  
  return(reach_wheat)
}

calculate_wheat_intake <- function(data,
                                   wheat_codes,
                                   adm1_col,
                                   quantity_col,
                                   survey_wgt_col,
                                   ea_col,
                                   res_col,
                                   item_code_col) {
  
  # define adjustment factors for codes 23-53; others default to 1
  adjustment_map <- c(
    `23` = 0.41,
    `24` = 0.75,
    `53` = 0.63
  )
  
  data |> 
    # keep only our codes
    filter({{item_code_col}} %in% wheat_codes) %>%
    # adjust quantity based on item code
    mutate(
      adj_quantity = {{quantity_col}} * 
        coalesce(adjustment_map[as.character({{item_code_col}})], 1)
    ) %>%
    # set up survey design
    as_survey_design(
      ids = {{ea_col}},
      strata = {{res_col}},
      weights = {{survey_wgt_col}},
      nest = TRUE
    ) %>%
    # compute mean of the adjusted quantity by region
    group_by({{adm1_col}}) %>%
    summarise(
      mean_wheat_g = survey_mean(adj_quantity),
      .groups = "drop"
    ) %>%
    select(-mean_wheat_g_se)
}

#----------------------------------------------------------------------------------

# BIVARIATE MAPS - WHEAT CONSUMPTION:
rwa_food_cons <- process_food_consumption2(
  hh_info_path = "processed_data/rwa_eicv2324_hh_information.csv",
  cons_path    = "processed_data/rwa_eicv2324_food_consumption.csv"
) |> 
  dplyr::select(-adm2) |> 
  left_join(
    rwa_hh_adm |> 
      dplyr::select(hhid, adm2), 
    by = "hhid"
  )

# RWA reach wheat: 
rwa_reach_wheat <- calculate_wheat_reach(
  data           = rwa_food_cons,
  wheat_codes    = c(23, 24, 50, 53),
  hhid_col       = hhid,
  adm1_col       = adm2, # Level of aggregation
  survey_wgt_col = survey_wgt,
  ea_col         = ea,
  res_col        = res,
  item_code_col  = item_code
)

# RWA intake wheat:
rwa_intake_wheat <- calculate_wheat_intake(
  data           = rwa_food_cons,
  wheat_codes    = c(23, 24, 50, 53),
  adm1_col       = adm2, # Level of aggregation
  quantity_col   = quantity_g,
  survey_wgt_col = survey_wgt,
  ea_col         = ea,
  res_col        = res,
  item_code_col  = item_code
)

rwa_reach_intake <- 
  rwa_reach_wheat %>% 
  left_join(rwa_intake_wheat, by = "adm2") %>% 
  mutate(
    across(everything(), ~ifelse(is.na(.), 0, .))
  ) %>% 
  mutate(
    # Create bins for the reach percentage from wheat reach
    reach_bins = cut(
      wheat_reach_pct, 
      breaks = c(0, 25, 50, 75, 100), 
      include.lowest = TRUE
    ),
    # Create bins for wheat intake (mean consumption in grams)
    intake_bins = cut(
      mean_wheat_g, 
      breaks = c(-Inf, 75, 149, 300, Inf),
      labels = c("<75",  "75–149",  "150–300",  ">300"),
      include.lowest = TRUE
    ),
    # Convert adm1 to character to match the shapefile's adm2 column
    adm2 = as.character(adm2)
  ) %>% 
  select(adm2, mean_wheat_g, reach_bins, intake_bins) %>% 
  left_join(rwa_adm2, by = "adm2") %>% 
  st_as_sf()

# create a bi classs
rwa_data_wheat <- bi_class(rwa_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )


# using ggplot and bi_scale, create a bivariate map
bi_map_wheat <- ggplot() + 
  geom_sf(data = rwa_data_wheat, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "BlueGold",dim = 4)+
  bi_theme()+
  geom_sf(data = rwa_adm1, fill= NA, color = 'black', lwd = 1) + 
  geom_sf_text(data = rwa_adm1, aes(label = adm1), size = 3, color = 'black', fontface = 'bold') +
  labs(subtitle = "Coverage and Consumption of wheat in Rwanda", )

# create a df of the breaks for each exis
break_vals <- bi_class_breaks(rwa_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )

#create a bivariate legend
legend_wheat <- bi_legend(pal = "BlueGold",
                          dim = 4,
                          xlab = "Higher Reach (%) ",
                          ylab = "Higher Consumption (g) ",
                          size = 8, 
                          breaks = break_vals)

# put legend and map together
rwa_wheat_bivariate <- ggdraw() +
  draw_plot(bi_map_wheat, 0, 0, 1, 1) +
  draw_plot(legend_wheat, 0.65, .2, 0.45, 0.2)

rwa_wheat_bivariate

rm(rwa_data_wheat, rwa_intake_wheat, rwa_reach_wheat, bi_map_wheat, 
   calculate_wheat_intake, calculate_wheat_reach)

#----------------------------------------------------------------------------------

# BIVARIATE MAPS - MAIZE CONSUMPTION:
calculate_corn_reach <- function(data, corn_codes, hhid_col, adm1_col, survey_wgt_col, ea_col, res_col, item_code_col){
  
  # Mark corn consumers
  corn <- data %>%
    filter({{item_code_col}} %in% corn_codes) %>%
    mutate(consumed_corn = 1)
  
  # Mark non-corn consumers and combine
  all_corn <- data %>%
    filter(!( {{hhid_col}} %in% corn[[rlang::as_name(enquo(hhid_col))]] )) %>%
    mutate(consumed_corn = 0) %>%
    bind_rows(corn)
  
  # Collapse to one row per household
  hh_corn_status <- all_corn %>%
    group_by({{hhid_col}}, {{adm1_col}}, {{survey_wgt_col}}, {{ea_col}}, {{res_col}}) %>%
    summarise(consumed_corn = max(consumed_corn), .groups = "drop")
  
  # Survey design and calculate reach
  reach_corn <- hh_corn_status %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, , nest = T) %>%
    group_by({{adm1_col}}) %>%
    summarise(corn_reach_pct = survey_mean(consumed_corn, proportion = TRUE) * 100) %>%
    select(-corn_reach_pct_se)
  
  return(reach_corn)
}

calculate_corn_intake <- function(data, corn_codes, adm1_col, quantity_col, survey_wgt_col, ea_col, res_col, item_code_col){
  # Filter corn items
  corn_quantity <- data %>%
    filter({{item_code_col}} %in% corn_codes)
  
  # Survey design
  corn_svy_design <- corn_quantity %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, nest = T)
  
  # Calculate survey-weighted mean corn consumption
  intake_corn <- corn_svy_design %>%
    group_by({{adm1_col}}) %>%
    summarise(mean_corn_g = survey_mean({{quantity_col}})) %>%
    select(-mean_corn_g_se)
  
  return(intake_corn)
}

rwa_reach_corn <- calculate_corn_reach(
  data = rwa_food_cons,
  corn_codes = c(10, 11),
  hhid_col = hhid,
  adm1_col = adm2, # Level of aggregation
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# Calculate corn intake
rwa_intake_corn <- calculate_corn_intake(
  data = rwa_food_cons,
  corn_codes = c(10, 11),
  adm1_col = adm2, # Level of aggregation
  quantity_col = quantity_g,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

rwa_reach_intake <- 
  rwa_reach_corn %>% 
  left_join(rwa_intake_corn, by = "adm2") %>% 
  mutate(
    across(everything(), ~ifelse(is.na(.), 0, .))
  ) %>% 
  mutate(
    # Create bins for the reach percentage from corn reach
    reach_bins = cut(
      corn_reach_pct, 
      breaks = c(0, 25, 50, 75, 100), 
      include.lowest = TRUE
    ),
    # Create bins for corn intake (mean consumption in grams)
    intake_bins = cut(
      mean_corn_g, 
      breaks = c(-Inf, 75, 149, 300, Inf),
      labels = c("<75",  "75–149",  "150–300",  ">300"),
      include.lowest = TRUE
    ),
    # Convert adm1 to character to match the shapefile's adm2 column
    adm2 = as.character(adm2)
  ) %>% 
  select(adm2, mean_corn_g, reach_bins, intake_bins) %>% 
  left_join(rwa_adm2, by = "adm2") %>% 
  st_as_sf()

# create a bi classs
rwa_data_corn <- bi_class(rwa_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )


# using ggplot and bi_scale, create a bivariate map
bi_map_corn <- ggplot() + 
  geom_sf(data = rwa_data_corn, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "BlueGold",dim = 4)+
  bi_theme()+
  geom_sf(data = rwa_adm1, fill= NA, color = 'black', lwd = 1)

bi_map_corn

rm(list = ls())

################################################################################
########################### END OF SCRIPT ######################################
################################################################################