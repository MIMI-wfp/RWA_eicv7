################################################################################
############ SCRIPT FOR QUALITY CHECKING AND FURTHER DATA CLEANING #############
################################################################################

# Author: Mo Osman
# Contributor: Uche Agu
# Date created: 17-07-2025
# Last edited: 

# Data Source: Rwanda Integrated Household Living Conditions Survey 7 (EICV7) 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "ggplot2")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA: 
food_consumption <- read_csv("processed_data/prelim_food_consumption.csv")
hh_afe <- read_csv("processed_data/rwa_eicv2324_afe.csv")
food_groups <- read_csv("metadata/food_groups.csv")

demographic <- read_dta("raw_data/CS_S0_S1_S2_S3_S4_S6A_S6B_S6C_Person.dta")

#-------------------------------------------------------------------------------

# QUALITY CHECKING AFE CALCULATIONS: 

# Calculate hh_size using demographic dataframe: 
hh_size <- demographic |>
  group_by(hhid) |>
  summarise(hh_size = n()) |>
  ungroup()

# Join hh_size with hh_afe:
hh_size <- hh_size |> 
  left_join(hh_afe, by = "hhid")


ggplot(hh_size, aes(x = hh_size, y = afe)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "AFE vs Household Size",
       x = "Household Size",
       y = "AFE") +
  theme_minimal(base_family = "", base_size = 11) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

ggsave("figures/afe_hh_size.png", width = 8, height = 6)

# Observed trends are re-assuring with almost a 1:1 relationship between AFE 
# and household size. For larger households, the AFE trends lower than the
# household size, which is expected as larger households tend to have more
# children.

rm(demographic, hh_size)

#-------------------------------------------------------------------------------

# GET CONSUMPTION QUANTITIES IN GRAMS/DAY/AFE: 
quantities_gdafe <- food_consumption |> 
  left_join(hh_afe, by = "hhid") |>
  mutate(quantity_gdafe = quantity_g / afe / 7)

#-------------------------------------------------------------------------------

# HANDLING OF EXTREME OUTLIERS: 
# (Defined as 3 standard deviations above the log transformed mean intake for 
# each food item)

# Log transform the quantity_gdafe:
quantities_gdafe <- quantities_gdafe |> 
  mutate(log_quantity_gdafe = log10(quantity_gdafe))

# Calculate the cutpoint for each item_code:
cutpoint <- quantities_gdafe |> 
  group_by(item_code) |> 
  summarise(mean_log = mean(log_quantity_gdafe, na.rm = TRUE),
            sd_log = sd(log_quantity_gdafe, na.rm = TRUE)) |> 
  mutate(cutpoint = mean_log + 3 * sd_log) |> 
  dplyr::select(item_code, cutpoint)

# Apply cutpoints to the data:
quantities_gdafe <- quantities_gdafe |> 
  left_join(cutpoint, by = "item_code") |> 
  mutate(quantity_gdafe = case_when(
    log_quantity_gdafe > cutpoint ~ NA_real_,
    TRUE ~ quantity_gdafe)) |> 
  dplyr::select(-log_quantity_gdafe, -cutpoint) |> 
  # Replace NA values with median reported intake 
  group_by(item_code) |>
  mutate(quantity_gdafe = ifelse(is.na(quantity_gdafe), 
                                  median(quantity_gdafe, na.rm = TRUE), 
                                  quantity_gdafe))

rm(cutpoint)

#-------------------------------------------------------------------------------

# DRAW OUT DISTRIBUTIONS FOR KEY FOOD ITEMS - SENSE CHECK: 

# Salt: 
quantities_gdafe |> 
  filter(item_code == 27) |> 
  ggplot(aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  xlim(c(0, 35)) + 
  xlab("Salt Consumption (grams/day/AFE)")

ggsave("figures/food_intake_distributions/salt.png")

# Sugar:
quantities_gdafe |> 
  filter(item_code == 25) |> 
  ggplot(aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  xlim(c(0, 150)) +
  xlab("Sugar Consumption (grams/day/AFE)")

ggsave("figures/food_intake_distributions/sugar.png")

# Intake distributions by food group: 
quantities_gdafe <- quantities_gdafe |> 
  left_join(food_groups |> 
              dplyr::select(item_code, food_group), 
            by = "item_code")

quantities_groups <- quantities_gdafe |> 
  group_by(food_group) |> 
  group_split()

# Name each object in quantities_groups according to food group:
quantities_groups <- setNames(quantities_groups, 
                              sapply(quantities_groups, function(x) unique(x$food_group)))

# Plot distributions for each food group:

# Grains, roots and tubers:
ggplot(quantities_groups$grains_roots_tubers, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 1000)) +
  labs(title = "Grains, Roots and Tubers Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/grains_roots_tubers.png", height = 6, width = 10)

# Pulses: 
ggplot(quantities_groups$pulses, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 1000)) +
  labs(title = "Pulses Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/pulses.png", height = 6, width = 10)

# Nuts and seeds: 
ggplot(quantities_groups$nuts_seeds, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 100)) +
  labs(title = "Nuts and Seeds Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/nuts_seeds.png", height = 4, width = 7)

# Dairy: 
ggplot(quantities_groups$dairy, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 500)) +
  labs(title = "Dairy Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/dairy.png", height = 6, width = 10)

# Meat, poultry and fish:
ggplot(quantities_groups$meat_poultry_fish, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 350)) +
  labs(title = "Meat, Poultry and Fish Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/meat_poultry_fish.png", height = 6, width = 10)

# Eggs:
ggplot(quantities_groups$eggs, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 100)) +
  labs(title = "Eggs Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/eggs.png")

# Dark leafy grains and vegetables: 
ggplot(quantities_groups$green_leafy_veg, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 250)) +
  labs(title = "Dark Leafy Vegetables Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/green_leafy_veg.png", height = 5, width = 10)

# Other vitamin A rich fruit and vegetables: 
ggplot(quantities_groups$vita_fruit_veg, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 450)) +
  labs(title = "Vitamin A Rich Fruit and Vegetables Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/vitamin_a_fruit_veg.png", height = 6, width = 10)

# Other vegetables: 
ggplot(quantities_groups$other_veg, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 450)) +
  labs(title = "Other Vegetables Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/other_vegetables.png", height = 6, width = 10)

# Other fruit: 
ggplot(quantities_groups$other_fruit, aes(x = quantity_gdafe)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(~ item_name, scales = "free_y", ncol = 5) +  # Facet by item_name within the food_group
  xlim(c(0, 500)) +
  labs(title = "Other Fruit Consumption",
       x = "Quantity (grams/day/AFE)",
       y = "Density") +
  theme_minimal()

ggsave("figures/food_intake_distributions/other_fruit.png", height = 6, width = 10)

# The examined distributions for the food items across the various food groups 
# appear to be reasonable and plausible. This reassures us that there are no
# significant data quality issues. Therefore we can proceed with analysis.

#-------------------------------------------------------------------------------

# UPDATE FOOD CONSUMPTION DATAFRAME WITH CLEANED VALUES:
food_consumption <- food_consumption |> 
  dplyr::select(-quantity_g) |> 
  left_join(quantities_gdafe |> 
              dplyr::select(hhid, item_code, afe, quantity_gdafe) |> 
              # Note that values need to be grams consumed per day - to remain 
              # consistent with other countries on the MIMI database:
              mutate(quantity_g = quantity_gdafe * afe) |> 
              dplyr::select(-c(quantity_gdafe, afe)),
            by = c("hhid", "item_code"))

#-------------------------------------------------------------------------------

# PREPARE FINAL FOOD_CONSUMPTION DATAFRAME FOR MIMI DATABASE: 
food_consumption <- food_consumption |> 
  mutate(iso3 = "RWA",
         survey = "eicv2324",
         quantity_100g = quantity_g / 100) |> 
  dplyr::select(iso3, survey, everything(), -item_name)

# WRITE CSV: 
write_csv(food_consumption, "processed_data/rwa_eicv2324_food_consumption.csv")

rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################
