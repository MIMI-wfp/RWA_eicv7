## FUNCTIONS TO COMPILE BASE MODELS

#-------------------------------------------------------------------------------

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "here")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

path_to_file <- here::here("processed_data/")

allen_ear <- data.frame(
  nutrient = c(
    "energy_kcal",
    "vita_rae_mcg",
    "thia_mg",
    "ribo_mg",
    "niac_mg",
    "vitb6_mg",
    "folate_mcg",
    "vitb12_mcg",
    "fe_mg",
    "ca_mg",
    "zn_mg"
  ),
  ear_value = c(
    2100,#who
    490, 
    0.9,
    1.3, 
    11, 
    1.3, 
    250, 
    2, 
    22.4, #low absorption
    860, 
    10.2# unrefined
  )
)

#-------------------------------------------------------------------------------

read_in_survey <- function(name_of_survey, path_to_file = here::here("processed_data/")){
  # given the name of the survey of country
  # the function reads in each part of the base model into general 
  # object names
  
  hh_info <<-  read.csv(paste0(path_to_file, paste0(name_of_survey, "_hh_information.csv")))
  food_consumption<<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_food_consumption.csv")))
  fc_table <<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_fct.csv")))
}

#-------------------------------------------------------------------------------

apparent_intake <- function(name_of_survey, path_to_file = here::here("processed_data//")){
  # Estimates apparent intake of nutrients based on consumed food items
  # and adult female equivalent unit of the household
  read_in_survey(name_of_survey, path_to_file)
  
  hh_info <- hh_info |> dplyr::select(-c("iso3", "survey"))
  food_consumption <- food_consumption |> dplyr::select(-c("iso3", "survey"))
  fc_table <- fc_table |> dplyr::select(-c("iso3", "survey"))
  
  x <- food_consumption |>  
    left_join(fc_table, by = "item_code") |> 
    mutate(
      across(
        -c(item_code, hhid, item_name, quantity_100g, quantity_g),
        ~.x*quantity_100g
      )
    ) |> 
    group_by(hhid) |> 
    summarise(
      across(-c(item_code,item_name,quantity_100g,quantity_g),
             ~sum(.,na.rm = T))
    ) |> 
    left_join(hh_info |> select(hhid, afe), by = "hhid") %>% 
    mutate(
      across(
        -c(hhid,afe),
        ~.x/afe
      )
    ) |> 
    ungroup() |>  
    select(-afe)
  x  

}

