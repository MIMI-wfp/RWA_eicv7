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

#-------------------------------------------------------------------------------

# Define map function: 
plot_map <- function(data, col, title, metric, outline_sf,
                     palette = "Zissou1", n = 100, limits = c(0, 100),
                     add_labels = FALSE) {

  p <- ggplot() +
    # fill the states by your chosen variable
    geom_sf(data = data,
            aes_string(fill = col),
            color = "black",
            size = 0.2) +
    # add a single black outline
    geom_sf(data = outline_sf,
            fill = NA,
            color = "black",
            size = 1) +
    # continuous palette
    scale_fill_gradientn(
      colours = wes_palette(palette, n = n, type = "continuous"),
      limits = limits,
      name  = metric) +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.caption     = element_text(hjust = 0.5),
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.title     = element_text(hjust = 0.5),
      legend.key.width = unit(1.35, "cm"),
      legend.key.height= unit(0.6, "cm")
    )

  if (isTRUE(add_labels)) {
    
    if (!"adm2" %in% names(data)) {
      stop("When add_labels = TRUE, 'data' must contain a column named 'adm2'.")
    }

    # Build labels: "Region Name\nXX%"; no rounding, use raw values + "%"
    label_data <- data
    nm   <- as.character(label_data[["adm2"]])
    vals <- label_data[[col]]

    # Keep rows with both a name and a value
    ok <- !is.na(nm) & !is.na(vals)
    label_data <- label_data[ok, , drop = FALSE]
    label_data$.__lab__ <- paste0(as.character(label_data[["adm2"]]),
                                  "\n",
                                  label_data[[col]], "%")

    # # Prepare labels (use raw values + %)
    # label_data <- data
    # label_data$.__lab__ <- paste0(label_data[["adm2"]], "\n", label_data[[col]], "%")
    # label_data <- label_data[!is.na(label_data$.__lab__), ]

    p <- p + geom_sf_text(
      data = label_data,
      aes(label = .__lab__),
      color = "white",
      size = 4,
      fontface = "bold",
      check_overlap = TRUE
    )
  }

  p
}


#-------------------------------------------------------------------------------

plot_nutrient_map <- function(data_sf, outline_sf, var, label,
                              palette = "Zissou1", n = 100,
                              limits = c(0,100)) {
  ggplot() +
    # fill the states by your chorwa variable
    geom_sf(data = data_sf,
            aes_string(fill = var),
            color = NA) +
    # add a single black outline
    geom_sf(data = outline_sf,
            fill = NA,
            color = "black",
            size = 1) +
    # continuous palette
    scale_fill_gradientn(
      colours = wes_palette(palette, n = n, type = "continuous"),
      limits = limits,
      name  = paste0(label, " Risk (%)")
    ) +
    geom_sf(data = rwa_adm1, fill= NA, color = 'black', lwd = 0.5) +
    # labs(
    #   title   = paste("Risk of Inadequate", label, "Intake"),
    #   caption = "Data Source: NLSS 2018/2019"
    # ) +
    theme_minimal() +
    theme(
      plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.caption     = element_text(hjust = 0.5),
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      legend.position  = "none",
      legend.direction = "horizontal",
      legend.title     = element_text(hjust = 0.5),
      legend.key.width = unit(1.35, "cm"),
      legend.key.height= unit(0.6, "cm")
    )
}

#-------------------------------------------------------------------------------

# Probability of iron inadequacy: 
# PROBABILITY OF IRON INADEQUACY:
fe_prob_inadequacy <- function(base_ai_df, bio_avail = 5) {
# Calculate probability of inadequacy for each household
# bioavailability:
base_ai_df <- base_ai_df |> 
  mutate(fe_prob_inad = case_when(
  bio_avail == 5 ~ 
    case_when(
      fe_mg <= 15 ~ 1,
      fe_mg > 15 & fe_mg <= 16.7 ~ 0.96,
      fe_mg > 16.7 & fe_mg <= 18.7 ~ 0.93,
      fe_mg > 18.7 & fe_mg <= 21.4 ~ 0.85,
      fe_mg > 21.4 & fe_mg <= 23.6 ~ 0.75,
      fe_mg > 23.6 & fe_mg <= 25.7 ~ 0.65,
      fe_mg > 25.7 & fe_mg <= 27.8 ~ 0.55,
      fe_mg > 27.8 & fe_mg <= 30.2 ~ 0.45,
      fe_mg > 30.2 & fe_mg <= 33.2 ~ 0.35,
      fe_mg > 33.2 & fe_mg <= 37.3 ~ 0.25,
      fe_mg > 37.3 & fe_mg <= 45.0 ~ 0.15,
      fe_mg > 45.0 & fe_mg <= 53.5 ~ 0.08,
      fe_mg > 53.5 & fe_mg <= 63.0 ~ 0.04,
      fe_mg > 63.0 ~ 0,
      TRUE ~ NA_real_
    ),
  bio_avail == 10 ~
    case_when(
      fe_mg <= 7.5 ~ 1,
      fe_mg > 7.5 & fe_mg <= 8.4 ~ 0.96,
      fe_mg > 8.4 & fe_mg <= 9.4 ~ 0.93,
      fe_mg > 9.4 & fe_mg <= 10.7 ~ 0.85,
      fe_mg > 10.7 & fe_mg <= 11.8 ~ 0.75,
      fe_mg > 11.8 & fe_mg <= 12.9 ~ 0.65,
      fe_mg > 12.9 & fe_mg <= 13.9 ~ 0.55,
      fe_mg > 13.9 & fe_mg <= 15.1 ~ 0.45,
      fe_mg > 15.1 & fe_mg <= 16.6 ~ 0.35,
      fe_mg > 16.6 & fe_mg <= 18.7 ~ 0.25,
      fe_mg > 18.7 & fe_mg <= 22.5 ~ 0.15,
      fe_mg > 22.5 & fe_mg <= 26.7 ~ 0.08,
      fe_mg > 26.7 & fe_mg <= 31.5 ~ 0.04,
      fe_mg > 31.5 ~ 0,
      TRUE ~ NA_real_
  ))) # SOURCE: WHO guidelines on food fortification with micronutrients (2006). Table 7.5, page 158

  return(base_ai_df)
}