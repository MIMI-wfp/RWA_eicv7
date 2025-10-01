################################################################################
################### SCRIPT FOR LOADING MIMI DATA INTO MYSQL DB #################
################################################################################

# Author: Mohammed Osman
# Date: 01-10-2025

rq_packages <- c("readr", "DBI", "RMySQL", "tidyverse", "getPass")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

con <- dbConnect(RMySQL::MySQL(),
                 dbname = "mimi_db",
                 host = "127.0.0.1",
                 port = 3306,
                 user = getPass("Enter username: "),
                 password = getPass("Enter password: "))

#-------------------------------------------------------------------------------

# READ DATA THAT REQUIRES LOADING INTO DB: 
base_ai <- read_csv("processed_data/rwa_eicv2324_base_ai.csv")
hh_information <- read_csv("processed_data/rwa_eicv2324_hh_information.csv")
food_consumption <- read_csv("processed_data/rwa_eicv2324_food_consumption.csv")
food_groups <- read_csv("processed_data/rwa_eicv2324_food_groups.csv")

#-------------------------------------------------------------------------------

# LOAD DATA INTO DB:

# base_ai
# dbWriteTable(con, name = "base_ai", value = base_ai, append = TRUE, 
#              row.names = FALSE)

rm(base_ai)

# hh_information
# dbWriteTable(con, name = "hh_information", value = hh_information, append = TRUE, 
#              row.names = FALSE)

rm(hh_information)

# food_consumption
# dbWriteTable(con, name = "food_consumption", value = food_consumption, append = TRUE,
#              row.names = FALSE)

rm(food_consumption)

# food_groups
# dbWriteTable(con, name = "food_groups", value = food_groups, append = TRUE,
#              row.names = FALSE)

rm(food_groups)

#-------------------------------------------------------------------------------

# CLOSE CONNECTION TO DB:
dbDisconnect(con)
rm(con)

################################################################################
################################# END OF SCRIPT ################################
################################################################################