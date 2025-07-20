################################################################################
######################### SCRIPT FOR COMPILING BASE MODEL ######################
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 19-07-2025
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# SOURCE REQUIRED FUNCTIONS: 
source("src/05base_model_functions.R")

#-------------------------------------------------------------------------------

# GET FCT DATA INTO THE CORRECT SHAPE: 
fct <- read_csv("metadata/rwa_eicv7_fct.csv")

fct <- fct |> 
  mutate(iso3 = "RWA",
         survey = "eicv2324") |> 
  dplyr::select(iso3, survey, item_code, item_name, energy_kcal, vita_rae_mcg,
                thia_mg, ribo_mg, niac_mg, folate_mcg, vitb12_mcg, fe_mg, zn_mg)

# Write to processed data folder:
# write_csv(fct, "processed_data/rwa_eicv2324_fct.csv")

rm(fct)

#-------------------------------------------------------------------------------

# COMPILE BASE MODEL:
base_ai <- apparent_intake("rwa_eicv2324")

rm(list = setdiff(ls(), c("base_ai")))

#-------------------------------------------------------------------------------

# SENSE CHECK NUTRIENT VALUES: 

# Start with energy intake: 
base_ai |> 
  ggplot(aes(x = energy_kcal)) +
  geom_density(fill="#667028", color="#e9ecef", alpha=0.8) +
  xlim(c(0, max(base_ai$energy_kcal))) + 
  xlab("Energy intake (kcal/day/AFE)")

# List micronutrient indicators:
micronutrients <- c("vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg", 
                    "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg")

plots <- list()

# Plot micronutrient distributions:
for (i in micronutrients) {
  p <- base_ai |>
    ggplot(aes(x = .data[[i]])) +
    geom_density(fill = "#667028", color = "#e9ecef", alpha = 0.8) +
    xlim(c(0, max(base_ai[[i]], na.rm = TRUE))) +
    xlab(paste(i, "intake"))
  
  plots[[i]] <- p
  print(plots[[i]])
  
}

# All micronutrient intake distributions appear plausible

rm(list = setdiff(ls(), c("base_ai")))

#-------------------------------------------------------------------------------

# Structure base_ai data frame so that it is ready for the MIMI database: 
base_ai <- base_ai |> 
  mutate(iso3 = "RWA",
         survey = "eicv2324") |> 
  dplyr::select(iso3, survey, everything())

# Write data: 
write_csv(base_ai, "processed_data/rwa_eicv2324_base_ai.csv")

rm(base_ai)

################################################################################
################################# END OF SCRIPT ################################
################################################################################



