################################################################################
######### SCRIPT TO CREATE AND EXPLORE INDICATOR FOR OVERALL INADEQUACY ########
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 25-Aug-2025
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "spdep", "sf", "wesanderson",
                 "srvyr", "gt", "webshot2")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Read in E-AR values: 
source("src/05base_model_functions.R")

rm(list = setdiff(ls(), c("allen_ear")))

# Read in base AI data:
base_ai <- read_csv("processed_data/rwa_eicv2324_base_ai.csv")
# hh_information: 
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")

# Household locations:
rwa_hh_adm <- readRDS("shapefiles/rwa_hh_adm.rds")

#-------------------------------------------------------------------------------

# MEAN ADEQUACY RATIO (MAR): 
mn_mar <- hh_information |> 
  dplyr::select(iso3, survey, hhid, survey_wgt, res, sep_quintile) |> 
  left_join(rwa_hh_adm, by = "hhid") |>
  left_join(base_ai, by = c("iso3", "survey", "hhid")) |> 
  # Nutrient adequacy ratio (NAR) for each micronutrient:
  mutate(va_nar = vita_rae_mcg /allen_ear[allen_ear$nutrient == "vita_rae_mcg", "ear_value"],
         fol_nar = folate_mcg /allen_ear[allen_ear$nutrient == "folate_mcg", "ear_value"],
         vb12_nar = vitb12_mcg /allen_ear[allen_ear$nutrient == "vitb12_mcg", "ear_value"],,
         fe_nar = fe_mg /allen_ear[allen_ear$nutrient == "fe_mg", "ear_value"],
         zn_nar = zn_mg /allen_ear[allen_ear$nutrient == "zn_mg", "ear_value"]) |> 
  # Truncate NAR values to 1:
  mutate(across(c(va_nar:zn_nar), ~ ifelse(. > 1, 1, .))) |>
  # Average values to obtain MAR:
  mutate(mar = rowMeans(across(c(va_nar:zn_nar)), na.rm = TRUE))

# Binarise MAR (inadequate if MAR < 0.75): 
mn_mar <- mn_mar |> 
  mutate(mar_inadequate = ifelse(mar < 0.75, 1, 0)) |> 
  dplyr::select(iso3, survey, hhid, res, sep_quintile, adm1, adm2, survey_wgt, mar_inadequate)

#-------------------------------------------------------------------------------

# MAR MAPS: 

# Read in map function: 
source("src/05base_model_functions.R")
rm(list = setdiff(ls(), c("plot_map", "mn_mar")))

# Create tbl_svy object:
mn_mar_svy <- mn_mar |> 
  as_survey_design(ids = hhid, 
                   weights = survey_wgt, 
                   nest = TRUE)

# Aggregate MAR to the ADM2 level: 
mar_adm2 <- mn_mar_svy |> 
  group_by(adm1, adm2) |> 
  summarise(mar_inadequate = survey_mean(mar_inadequate, 
                                         na.rm = TRUE, 
                                         vartype = NULL)) |> 
  ungroup() |> 
  mutate(mar_inadequate = round(mar_inadequate * 100, digits = 1))

# Read in shapefile data: 
rwa_adm1 <- readRDS("shapefiles/rwa_adm1.rds")
rwa_adm2 <- readRDS("shapefiles/rwa_adm2.rds") 

# Join MAR values to shapefile data:
mar_adm2 <- mar_adm2 |> 
  left_join(rwa_adm2, by = "adm2") |> 
  st_as_sf()

# Plot MAR map:
plot_map(data = mar_adm2,
         col = "mar_inadequate",
         title = "Mean Adequacy ratio (MAR)",
         metric = "% At risk of inadequate intake (MAR < 0.75)", 
         outline_sf = rwa_adm1) 

ggsave("maps/mar_risk.png", width = 8, height = 6)

#-------------------------------------------------------------------------------

# PRESENT DATA IN TABULAR FORMAT:

# Read in aggregate estimates for individual micronutrients:
mn_inadequacy <- read_csv("processed_data/mn_inadequacy.csv") |> 
  dplyr::select(-geometry)

mar_adm2 <- mar_adm2 |> 
  as.data.frame() |>
  dplyr::select(adm2, mar_inadequate)

# Join: 
mn_inadequacy <- mn_inadequacy |> 
  left_join(mar_adm2, by = "adm2")

inadequacy_table <- mn_inadequacy |>
  gt() |> 
  tab_header(title = md("Risk of inadequate micronutrient intake in Rwanda")) |> 
  # Bold column names:
  cols_label(adm2 = md("**District**"),
             vita_inadequacy = md("**Vitamin A**"),
             thia_inadequacy = md("**Thiamine**"),
             ribo_inadequacy = md("**Riboflavin**"),
             niac_inadequacy = md("**Niacin**"),
             folate_inadequacy = md("**Folate**"),
             vitb12_inadequacy = md("**Vitamin B12**"),
             fe_inadequacy = md("**Iron**"),
             zn_inadequacy = md("**Zinc**"),
             mar_inadequate = md("**Overall (MAR < 0.75)**")) |> 
  tab_footnote(footnote = "% At risk of inadequate intake",
               locations = cells_column_labels(columns = -adm2)) |>
  tab_footnote(footnote = "MAR = Mean Adequacy Ratio (Vitamin A, Folate, Vitamin B12, Iron, Zinc)",
               locations = cells_column_labels(columns = mar_inadequate)) |>
  tab_footnote("Rwanda Integrated Household Living Conditions Survey 7 (EICV7), 2023-24") |> 
  cols_align(align = "center", columns = -adm2) |>
  cols_width(everything() ~ px(90))


inadequacy_table

gtsave(inadequacy_table, "maps/inadequacy_table.png")
#-------------------------------------------------------------------------------

# DISAGGREGATION BY URB/RURAL, SEP: 

# Urban: 
urban_mar <- mn_mar_svy |> 
  filter(res == "Urban") |>
  group_by(sep_quintile) |> 
  summarise(mar_inadequate = survey_mean(mar_inadequate, 
                                         na.rm = TRUE, 
                                         vartype = NULL)) |>
  ungroup() |> 
  mutate(mar_inadequate = round(mar_inadequate * 100, digits = 1)) |> 
  # Change class of "sep_quintile" to factor:
  mutate(sep_quintile = recode(sep_quintile, 
                               `1` = "1 = poorest",
                               `2` = "2",
                               `3` = "3",
                               `4` = "4",
                               `5` = "5 = wealthiest"))

# Rural: 
rural_mar <- mn_mar_svy |> 
  filter(res == "Rural") |>
  group_by(sep_quintile) |> 
  summarise(mar_inadequate = survey_mean(mar_inadequate, 
                                         na.rm = TRUE, 
                                         vartype = NULL)) |>
  ungroup() |> 
  mutate(mar_inadequate = round(mar_inadequate * 100, digits = 1)) |> 
  # Change class of "sep_quintile" to factor:
  mutate(sep_quintile = recode(sep_quintile, 
                               `1` = "1 = poorest",
                               `2` = "2",
                               `3` = "3",
                               `4` = "4",
                               `5` = "5 = wealthiest"))

# Create geom_point plot: 
ggplot(data = urban_mar, aes(x = mar_inadequate, y = sep_quintile)) +
  geom_point(size = 3) +
  xlim(0, 100) +
  labs(title = "Urban populations",
       x = "% At risk of inadequate intake (MAR < 0.75)",
       y = "Socio-economic quntile")

ggplot(data = rural_mar, aes(x = mar_inadequate, y = sep_quintile)) +
  geom_point(size = 3) +
  xlim(0, 100) +
  labs(title = "Rural populations",
       x = "% At risk of inadequate intake (MAR < 0.75)",
       y = "Socio-economic quntile")


rm(list = ls())

################################################################################
################################ END OF SCRIPT #################################
################################################################################