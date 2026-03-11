################################################################################
##### SCRIPT FOR INADEQUACY SUMMARY STATISTICS TO USE IN DHS TRIANGULATION #####
################################################################################

# Author: Mo Osman
# Date created: 03-Feb-2026
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "gt", "webshot2")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA AND EAR VALUES:

# Read in E-AR values: 
source("src/05base_model_functions.R")

rm(list = setdiff(ls(), c("allen_ear")))

# Read in base AI data:
base_ai <- read_csv("processed_data/rwa_eicv2324_base_ai.csv")

# Household information: 
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")

# Household locations:
rwa_hh_adm <- readRDS("shapefiles/rwa_hh_adm.rds")

#-------------------------------------------------------------------------------

# PREPARE DATA WITH INADEQUACY INDICATORS:

# Join all data:
analysis_data <- hh_information |> 
  dplyr::select(iso3, survey, hhid, survey_wgt, res, sep_quintile) |> 
  left_join(rwa_hh_adm, by = "hhid") |>
  left_join(base_ai, by = c("iso3", "survey", "hhid"))

# Calculate inadequacy for Vitamin A and Iron:
analysis_data <- analysis_data |> 
  mutate(
    vita_inadequate = ifelse(vita_rae_mcg < allen_ear[allen_ear$nutrient == "vita_rae_mcg", "ear_value"], 1, 0),
    fe_inadequate = ifelse(fe_mg < allen_ear[allen_ear$nutrient == "fe_mg", "ear_value"], 1, 0),
    folate_inadequate = ifelse(folate_mcg < allen_ear[allen_ear$nutrient == "folate_mcg", "ear_value"], 1, 0),
    vitb12_inadequate = ifelse(vitb12_mcg < allen_ear[allen_ear$nutrient == "vitb12_mcg", "ear_value"], 1, 0)
  )

# Create survey design object:
analysis_svy <- analysis_data |> 
  as_survey_design(ids = hhid, 
                   weights = survey_wgt, 
                   nest = TRUE)

#-------------------------------------------------------------------------------

# VITAMIN A - CALCULATE PREVALENCE BY STRATIFICATION:

# National estimate:
vita_national <- analysis_svy |> 
  summarise(prevalence = survey_mean(vita_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "National",
    category = "Rwanda",
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence)

# ADM1 (Province) stratification:
vita_adm1 <- analysis_svy |> 
  group_by(adm1) |> 
  summarise(prevalence = survey_mean(vita_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Province",
    category = adm1,
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Urban/Rural stratification:
vita_residence <- analysis_svy |> 
  group_by(res) |> 
  summarise(prevalence = survey_mean(vita_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Residence",
    category = res,
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Socioeconomic quintile stratification:
vita_sep <- analysis_svy |> 
  group_by(sep_quintile) |> 
  summarise(prevalence = survey_mean(vita_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Socioeconomic Quintile",
    category = case_when(
      sep_quintile == 1 ~ "1 (Poorest)",
      sep_quintile == 2 ~ "2",
      sep_quintile == 3 ~ "3",
      sep_quintile == 4 ~ "4",
      sep_quintile == 5 ~ "5 (Wealthiest)",
      TRUE ~ as.character(sep_quintile)
    ),
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Combine all Vitamin A estimates:
vita_summary <- bind_rows(vita_national, vita_adm1, vita_residence, vita_sep)

# Clean up stratification column - show label only once per group:
vita_summary <- vita_summary |> 
  mutate(stratification_display = ifelse(
    stratification != lag(stratification, default = ""), 
    stratification, 
    ""
  ))

#-------------------------------------------------------------------------------

# IRON - CALCULATE PREVALENCE BY STRATIFICATION:

# National estimate:
fe_national <- analysis_svy |> 
  summarise(prevalence = survey_mean(fe_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "National",
    category = "Rwanda",
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence)

# ADM1 (Province) stratification:
fe_adm1 <- analysis_svy |> 
  group_by(adm1) |> 
  summarise(prevalence = survey_mean(fe_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Province",
    category = adm1,
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Urban/Rural stratification:
fe_residence <- analysis_svy |> 
  group_by(res) |> 
  summarise(prevalence = survey_mean(fe_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Residence",
    category = res,
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Socioeconomic quintile stratification:
fe_sep <- analysis_svy |> 
  group_by(sep_quintile) |> 
  summarise(prevalence = survey_mean(fe_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Socioeconomic Quintile",
    category = case_when(
      sep_quintile == 1 ~ "1 (Poorest)",
      sep_quintile == 2 ~ "2",
      sep_quintile == 3 ~ "3",
      sep_quintile == 4 ~ "4",
      sep_quintile == 5 ~ "5 (Wealthiest)",
      TRUE ~ as.character(sep_quintile)
    ),
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Combine all Iron estimates:
fe_summary <- bind_rows(fe_national, fe_adm1, fe_residence, fe_sep)

# Clean up stratification column - show label only once per group:
fe_summary <- fe_summary |> 
  mutate(stratification_display = ifelse(stratification != lag(stratification, default = ""), 
                                         stratification, 
                                         ""))

#-------------------------------------------------------------------------------

# FOLATE: 

# National estimate: 
folate_national <- analysis_svy |> 
  summarise(prevalence = survey_mean(folate_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "National",
    category = "Rwanda",
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence)

# ADM1 (Province) stratification:
folate_adm1 <- analysis_svy |> 
  group_by(adm1) |> 
  summarise(prevalence = survey_mean(folate_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Province",
    category = adm1,
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Urban/Rural stratification:
folate_residence <- analysis_svy |> 
  group_by(res) |> 
  summarise(prevalence = survey_mean(folate_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Residence",
    category = res,
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Socioeconomic quintile stratification:
folate_sep <- analysis_svy |> 
  group_by(sep_quintile) |> 
  summarise(prevalence = survey_mean(folate_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Socioeconomic Quintile",
    category = case_when(
      sep_quintile == 1 ~ "1 (Poorest)",
      sep_quintile == 2 ~ "2",
      sep_quintile == 3 ~ "3",
      sep_quintile == 4 ~ "4",
      sep_quintile == 5 ~ "5 (Wealthiest)",
      TRUE ~ as.character(sep_quintile)
    ),
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Combine all Folate estimates:
folate_summary <- bind_rows(folate_national, folate_adm1, folate_residence, folate_sep)

# Clean up stratification column - show label only once per group:
folate_summary <- folate_summary |> 
  mutate(stratification_display = ifelse(stratification != lag(stratification, default = ""), 
                                         stratification, 
                                         ""))

#-------------------------------------------------------------------------------

# VITAMIN B12:  

# National estimate:
vitb12_national <- analysis_svy |> 
  summarise(prevalence = survey_mean(vitb12_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "National",
    category = "Rwanda",
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence)

# ADM1 (Province) stratification:
vitb12_adm1 <- analysis_svy |> 
  group_by(adm1) |> 
  summarise(prevalence = survey_mean(vitb12_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Province",
    category = adm1,
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Urban/Rural stratification:
vitb12_residence <- analysis_svy |> 
  group_by(res) |> 
  summarise(prevalence = survey_mean(vitb12_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Residence",
    category = res,
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Socioeconomic quintile stratification:
vitb12_sep <- analysis_svy |> 
  group_by(sep_quintile) |> 
  summarise(prevalence = survey_mean(vitb12_inadequate, na.rm = TRUE, vartype = NULL)) |>
  mutate(
    stratification = "Socioeconomic Quintile",
    category = case_when(
      sep_quintile == 1 ~ "1 (Poorest)",
      sep_quintile == 2 ~ "2",
      sep_quintile == 3 ~ "3",
      sep_quintile == 4 ~ "4",
      sep_quintile == 5 ~ "5 (Wealthiest)",
      TRUE ~ as.character(sep_quintile)
    ),
    prevalence = round(prevalence * 100, digits = 1)
  ) |> 
  select(stratification, category, prevalence) |> 
  ungroup()

# Combine all Vitamin B12 estimates:
vitb12_summary <- bind_rows(vitb12_national, vitb12_adm1, vitb12_residence, vitb12_sep)

# Clean up stratification column - show label only once per group:
vitb12_summary <- vitb12_summary |> 
  mutate(stratification_display = ifelse(stratification != lag(stratification, default = ""), 
                                         stratification, 
                                         ""))

#-------------------------------------------------------------------------------

# CREATE VITAMIN A GT SUMMARY TABLE:

vita_table <- vita_summary |>
  select(stratification_display, category, prevalence) |>
  gt() |> 
  tab_header(title = md("**Risk of Inadequate Vitamin A Intake in Rwanda**"),
             subtitle = md("Stratified by population sub-group")) |> 
  cols_label(
    stratification_display = md(""),
    category = md(""),
    prevalence = md("**Inadequate intake (%)**")
  ) |> 
  tab_footnote(
    footnote = "apparent intake < EAR",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "EAR for Vitamin A: 490 μg RAE",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "Rwanda Integrated Household Living Conditions Survey 7 (EICV7), 2023-24"
  ) |> 
  cols_align(align = "left", columns = c(stratification_display, category)) |>
  cols_align(align = "center", columns = prevalence) |>
  cols_width(
    stratification_display ~ px(200),
    category ~ px(200),
    prevalence ~ px(150)
  ) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = stratification_display)
  )

# Display and save Vitamin A table:
vita_table

gtsave(vita_table, "figures/triangulation/vita_inadequacy_table.png")

#-------------------------------------------------------------------------------

# CREATE IRON GT SUMMARY TABLE:
fe_table <- fe_summary |>
  select(stratification_display, category, prevalence) |>
  gt() |> 
  tab_header(title = md("**Risk of Inadequate Iron Intake in Rwanda**"),
             subtitle = md("Stratified by population sub-group")) |> 
  cols_label(
    stratification_display = md(""),
    category = md(""),
    prevalence = md("**Inadequate intake (%)**")
  ) |> 
  tab_footnote(
    footnote = "apparent intake < EAR",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "EAR for Iron: 18 mg",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "Rwanda Integrated Household Living Conditions Survey 7 (EICV7), 2023-24"
  ) |> 
  cols_align(align = "left", columns = c(stratification_display, category)) |>
  cols_align(align = "center", columns = prevalence) |>
  cols_width(
    stratification_display ~ px(200),
    category ~ px(200),
    prevalence ~ px(150)
  ) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = stratification_display)
  )

# Display and save Iron table:
fe_table

gtsave(fe_table, "figures/triangulation/fe_inadequacy_table.png")

#-------------------------------------------------------------------------------

# CREATE FOLATE GT SUMMARY TABLE:
folate_table <- folate_summary |>
  select(stratification_display, category, prevalence) |>
  gt() |> 
  tab_header(title = md("**Risk of Inadequate Folate Intake in Rwanda**"),
             subtitle = md("Stratified by population sub-group")) |> 
  cols_label(
    stratification_display = md(""),
    category = md(""),
    prevalence = md("**Inadequate intake (%)**")
  ) |> 
  tab_footnote(
    footnote = "apparent intake < EAR",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "EAR for Folate: 320 μg DFE",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "Rwanda Integrated Household Living Conditions Survey 7 (EICV7), 2023-24"
  ) |> 
  cols_align(align = "left", columns = c(stratification_display, category)) |>
  cols_align(align = "center", columns = prevalence) |>
  cols_width(
    stratification_display ~ px(200),
    category ~ px(200),
    prevalence ~ px(150)
  ) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = stratification_display)
  )

# Display and save Folate table:
folate_table

gtsave(folate_table, "figures/triangulation/folate_inadequacy_table.png")

#-------------------------------------------------------------------------------

# VITAMIN B12 GT SUMMARY TABLE:
vitb12_table <- vitb12_summary |>
  select(stratification_display, category, prevalence) |>
  gt() |> 
  tab_header(title = md("**Risk of Inadequate Vitamin B12 Intake in Rwanda**"),
             subtitle = md("Stratified by population sub-group")) |> 
  cols_label(
    stratification_display = md(""),
    category = md(""),
    prevalence = md("**Inadequate intake (%)**")
  ) |> 
  tab_footnote(
    footnote = "apparent intake < EAR",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "EAR for Vitamin B12: 2.4 μg",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "Rwanda Integrated Household Living Conditions Survey 7 (EICV7), 2023-24"
  ) |> 
  cols_align(align = "left", columns = c(stratification_display, category)) |>
  cols_align(align = "center", columns = prevalence) |>
  cols_width(
    stratification_display ~ px(200),
    category ~ px(200),
    prevalence ~ px(150)
  ) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = stratification_display)
  )

# Display and save Vitamin B12 table:
vitb12_table

gtsave(vitb12_table, "figures/triangulation/vitb12_inadequacy_table.png")

# Clean environment:
rm(list = ls())

################################################################################
################################ END OF SCRIPT #################################
################################################################################
