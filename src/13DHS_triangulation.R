################################################################################
######### SCRIPT TO CREATE GT SUMMARY TABLES FOR VITAMIN A AND IRON ##########
################################################################################

# Author: GitHub Copilot
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
    fe_inadequate = ifelse(fe_mg < allen_ear[allen_ear$nutrient == "fe_mg", "ear_value"], 1, 0)
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

#-------------------------------------------------------------------------------

# CREATE VITAMIN A GT SUMMARY TABLE:

vita_table <- vita_summary |>
  gt() |> 
  tab_header(title = md("**Risk of Inadequate Vitamin A Intake in Rwanda**"),
             subtitle = md("Prevalence by stratification")) |> 
  cols_label(
    stratification = md("**Stratification**"),
    category = md("**Category**"),
    prevalence = md("**% At Risk**")
  ) |> 
  tab_footnote(
    footnote = "% At risk of inadequate intake (intake < EAR)",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "EAR for Vitamin A: 490 μg RAE",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "Rwanda Integrated Household Living Conditions Survey 7 (EICV7), 2023-24"
  ) |> 
  cols_align(align = "left", columns = c(stratification, category)) |>
  cols_align(align = "center", columns = prevalence) |>
  cols_width(
    stratification ~ px(200),
    category ~ px(200),
    prevalence ~ px(150)
  ) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12
  )

# Display and save Vitamin A table:
vita_table

gtsave(vita_table, "figures/vita_inadequacy_table.png")

#-------------------------------------------------------------------------------

# CREATE IRON GT SUMMARY TABLE:

fe_table <- fe_summary |>
  gt() |> 
  tab_header(title = md("**Risk of Inadequate Iron Intake in Rwanda**"),
             subtitle = md("Prevalence by stratification")) |> 
  cols_label(
    stratification = md("**Stratification**"),
    category = md("**Category**"),
    prevalence = md("**% At Risk**")
  ) |> 
  tab_footnote(
    footnote = "% At risk of inadequate intake (intake < EAR)",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "EAR for Iron: 22.4 mg (low absorption)",
    locations = cells_column_labels(columns = prevalence)
  ) |>
  tab_footnote(
    footnote = "Rwanda Integrated Household Living Conditions Survey 7 (EICV7), 2023-24"
  ) |> 
  cols_align(align = "left", columns = c(stratification, category)) |>
  cols_align(align = "center", columns = prevalence) |>
  cols_width(
    stratification ~ px(200),
    category ~ px(200),
    prevalence ~ px(150)
  ) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12
  )

# Display and save Iron table:
fe_table

gtsave(fe_table, "figures/fe_inadequacy_table.png")

#-------------------------------------------------------------------------------

# PRINT SUMMARY TO CONSOLE:
cat("\n=== VITAMIN A INADEQUACY SUMMARY ===\n")
print(vita_summary)

cat("\n=== IRON INADEQUACY SUMMARY ===\n")
print(fe_summary)

cat("\n=== TABLES SAVED ===\n")
cat("Vitamin A table: figures/vita_inadequacy_table.png\n")
cat("Iron table: figures/fe_inadequacy_table.png\n")

#-------------------------------------------------------------------------------

# Clean environment:
rm(list = ls())

################################################################################
################################ END OF SCRIPT #################################
################################################################################
