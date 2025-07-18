################################################################################
############# SCRIPT FOR ADULT FEMALE EQUIVALENT UNIT CALCULATIONS #############
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 14-07-2025
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

# LIST OF ASSUMPTIONS FOR AFE CALCULATIONS: 
# No anthropometric data available in the EICV7, therefore assumptions made for 
# weight: 
# Average men's weight = 65kg
mens_weight <- 65

# Average women's weight = 55kg
womens_weight <- 55

# 1 AFE = 2291kcal/day for a 55kg Woman of reproductive age.
AFE <- 2291
# Source: FAO/WHO/UNU report on human energy requirements, 2004.

# PAL = 1.64 (average for Rwanda based on reported occupations, see table 9.1 of 
# EICV7 methodological notes)
PAL <- 1.64

# DHS only included anthropometry data for children under 5, and a sub-set of
# women aged 15-49. Therefore, I have assumed average weights for both men and
# women.

# No pregnancy or lactation data included in the Rwanda EICV7.

#-------------------------------------------------------------------------------

# READ DATA: 
demographic <- read_dta("raw_data/CS_S0_S1_S2_S3_S4_S6A_S6B_S6C_Person.dta")

#-------------------------------------------------------------------------------

# IDENTIFY INDIVIDUALS BELONGING TO EACH DEMOGRAPHIC GROUP: 

# CHILDREN UNDER 2:
u2 <- demographic |> 
  filter(s1q3y < 2) |> 
  # Calculate their age in months: 
  mutate(age_months = s1q3y * 12 + s1q3m) |> 
  # Select relevant variables: 
  dplyr::select(hhid, pid, s1q1, age_months) |> 
  rename(sex = s1q1)

# CHILDREN 2-18: 
children_2_18 <- demographic |> 
  filter(s1q3y >= 2 & s1q3y < 18) |>
  # Select relevant variables: 
  dplyr::select(hhid, pid, s1q1, s1q3y) |> 
  rename(sex = s1q1, 
         age_years = s1q3y)

# ADULTS 18+:
adults <- demographic |>
  filter(s1q3y >= 18) |>
  # Select relevant variables:
  dplyr::select(hhid, pid, s1q1, s1q3y) |>
  rename(sex = s1q1, 
         age_years = s1q3y)

#-------------------------------------------------------------------------------

# CALCULATE AFE FOR EACH DEMOGRAPHIC GROUP:

# CHILDREN UNDER 2:

# Source:
# Book - Complementary feeding of young Children in Developing Countries, 
# Table 10, page 51.
# WHO 1998, edited by Kenneth Brown, Kathryn Dewey, Lindsay Allen

u2 <- u2 |> 
  mutate(kcalreq = case_when(
    age_months <= 2 ~ 0,   # only breast feeding - no food intake
    age_months >= 3 & age_months <= 5 ~ 76,  # energy from food is 76kcal per day for 3-5 months of age
    age_months >= 6 & age_months <= 8 ~ 269,  # 269kcal per day for 6-8 months of age
    age_months >= 9 & age_months <= 11 ~ 451,   # 451kcal per day for 9-11 months of age
    age_months >= 12 ~ 746)) # 746kcal per day for those aged 12-months - 2years

afe_u2 <- u2 |> 
  mutate(afe = kcalreq / AFE) |> 
  dplyr::select(hhid, pid, afe)

# CHILDREN 2-18:
# Source - kcal requirements based on tables 4.5 and 4.6 in the FAO/WHO/UNU 
# report.
# Assumed moderate physical activity level for both boys and girls.
children_2_18 <- children_2_18 |> 
  mutate(kcalreq = case_when(
    sex == 1 & age_years == 2 ~ 1125,
    sex == 1 & age_years == 3 ~ 1250,
    sex == 1 & age_years == 4 ~ 1350,
    sex == 1 & age_years == 5 ~ 1475, 
    sex == 1 & age_years == 6 ~ 1575,
    sex == 1 & age_years == 7 ~ 1700,
    sex == 1 & age_years == 8 ~ 1825, 
    sex == 1 & age_years == 9 ~ 1975, 
    sex == 1 & age_years == 10 ~ 2150,
    sex == 1 & age_years == 11 ~ 2350, 
    sex == 1 & age_years == 12 ~ 2550, 
    sex == 1 & age_years == 13 ~ 2775, 
    sex == 1 & age_years == 14 ~ 3000,
    sex == 1 & age_years == 15 ~ 3175, 
    sex == 1 & age_years == 16 ~ 3325, 
    sex == 1 & age_years == 17 ~ 3400,
    sex == 2 & age_years == 2 ~ 1050,
    sex == 2 & age_years == 3 ~ 1150, 
    sex == 2 & age_years == 4 ~ 1250,
    sex == 2 & age_years == 5 ~ 1325,
    sex == 2 & age_years == 6 ~ 1425, 
    sex == 2 & age_years == 7 ~ 1550,
    sex == 2 & age_years == 8 ~ 1700,
    sex == 2 & age_years == 9 ~ 1850,
    sex == 2 & age_years == 10 ~ 2000,
    sex == 2 & age_years == 11 ~ 2150, 
    sex == 2 & age_years == 12 ~ 2275, 
    sex == 2 & age_years == 13 ~ 2375, 
    sex == 2 & age_years == 14 ~ 2450, 
    sex == 2 & age_years > 15 & age_years < 18 ~ 2500
  ))

afe_children_2_18 <- children_2_18 |>
  mutate(afe = kcalreq / AFE) |> 
  dplyr::select(hhid, pid, afe)

# ADULTS 18+:
# Source: table 5.2 of the FAO/WHO/UNU report.
adults <- adults |> 
  # Firstly calculate BMR (Basal Metabolic Rate): 
  mutate(BMR = case_when(
    sex == 1 & age_years >= 18 & age_years < 30 ~ 15.057 * mens_weight + 692.2,
    sex == 1 & age_years >= 30 & age_years < 60 ~ 11.472 * mens_weight + 873.1, 
    sex == 1 & age_years >= 60 ~ 11.711  * mens_weight + 587.7,
    sex == 2 & age_years >= 18 & age_years < 30 ~ 14.818 * womens_weight + 486.6, 
    sex == 2 & age_years >= 30 & age_years < 60 ~ 8.126 * womens_weight + 845.6,
    sex == 2 & age_years >= 60 ~ 9.082 * womens_weight + 658.5
  )) |> 
  # Multiply by PAL to get total energy requirements:
  mutate(kcalreq = BMR * PAL)

afe_adults <- adults |>
  mutate(afe = kcalreq / AFE) |> 
  dplyr::select(hhid, pid, afe)

#-------------------------------------------------------------------------------

# CALCULATE TOTAL AFE PER HOUSEHOLD: 
hh_afe <- bind_rows(afe_u2, afe_children_2_18, afe_adults) |>
  group_by(hhid) |>
  summarise(afe = sum(afe, na.rm = TRUE)) |>
  ungroup()

# WRITE CSV: 
write_csv(hh_afe, "processed_data/rwa_eicv2324_afe.csv")

rm(list = ls())

################################################################################
################################ END OF SCRIPT #################################
################################################################################