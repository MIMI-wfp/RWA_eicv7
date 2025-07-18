# Date: 18/7/2025
# Author: Uchenna Agu



#1. Read in RWA eicv7 household roster, and recode adm1 and adm2 according to available shapefiles
# Keep only hhid, adm1 amd adm2
rwa_hh_adm <- read_dta("CS_S01_S5_S7_Household.dta") %>%
  select(hhid, province, district) %>%
  rename(adm1=province,
         adm2=district) %>%
  mutate(
    adm1 = as.integer(adm1),
    adm1 = recode(adm1,
                      "1" = "Kigali City",
                      "2" = "Southern Province",
                      "3" = "Western Province",
                      "4" = "Northern Province",
                      "5" = "Eastern Province"),
    
    adm2 = as.integer(adm2), 
    adm2 = recode(adm2,
                      "11" = "Nyarugenge",
                      "12" = "Gasabo",
                      "13" = "Kicukiro",
                      "21" = "Nyanza",
                      "22" = "Gisagara",
                      "23" = "Nyaruguru",
                      "24" = "Huye",
                      "25" = "Nyamagabe",
                      "26" = "Ruhango",
                      "27" = "Muhanga",
                      "28" = "Kamonyi",
                      "31" = "Karongi",
                      "32" = "Rutsiro",
                      "33" = "Rubavu",
                      "34" = "Nyabihu",
                      "35" = "Ngororero",
                      "36" = "Rusizi",
                      "37" = "Nyamasheke",
                      "41" = "Rulindo",
                      "42" = "Gakenke",
                      "43" = "Musanze",
                      "44" = "Burera",
                      "45" = "Gicumbi",
                      "51" = "Rwamagana",
                      "52" = "Nyagatare",
                      "53" = "Gatsibo",
                      "54" = "Kayonza",
                      "55" = "Kirehe",
                      "56" = "Ngoma",
                      "57" = "Bugesera")
  )



  
#2. Read in shapefiles, keep just adm and geometry for both
rwa_adm1 <- st_read("rwa_adm1_2006_NISR_WGS1984_20181002.shp") %>%
  select(ADM1_EN, geometry) %>%
  rename(adm1=ADM1_EN)

rwa_adm2 <- st_read("rwa_adm2_2006_NISR_WGS1984_20181002.shp") %>%
  select(ADM2_EN, geometry) %>%
  rename(adm2=ADM2_EN)




#3. Plot shapefiles
ggplot(data = rwa_adm1) +
  geom_sf(fill = "lightblue", color = "black", linewidth = 0.5) +
  geom_sf_text(aes(label = adm1),
               size = 3,
               color = "black") +
  labs(title = "Provinces of Rwanda") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),   
    panel.border = element_blank(),     
    plot.title = element_text(hjust = 0.5)
  )

ggplot(data = rwa_adm2) +
  geom_sf(fill = "lightblue", color = "black", linewidth = 0.5) +
  geom_sf_text(aes(label = adm2),
               size = 3,
               color = "black") +
  labs(title = "Districts of Rwanda") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),   
    panel.border = element_blank(),     
    plot.title = element_text(hjust = 0.5)
  )




#4. Save files
# saveRDS(rwa_hh_adm, file = "rwa_hh_adm.Rds")
# saveRDS(rwa_adm1, file = "rwa_adm1.Rds")
# saveRDS(rwa_adm2, file = "rwa_adm2.Rds")

