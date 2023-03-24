# 13 - Development Capacity
# Generate indicator dataset ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("wesanderson")
# install.packages("tigris")
library(tidyverse)
library(sf)
library(leaflet)
library(wesanderson)
library(tigris)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Load data
disp_2018 <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/13-Development Capacity/2018by_upd_meth/hh_at_displacement_risk-2021-11-29/hh_at_displacement_risk-2021-11-29.csv")
disp_2018_grp = disp_2018 %>%
  select(census_tract_id,hh_at_risk,hh_total) %>%
  group_by(census_tract_id) %>% 
  summarise(hh_at_risk_2018by = sum(hh_at_risk), hh_total_2018by = sum(hh_total),
            per_at_risk_2018by = as.double(hh_at_risk_2018by/hh_total_2018by) * 100) 

# Loading 2014by data to get GEOIDs
disp_risk_2014by <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/013_DevelopmentCapacity.csv")
geoid_info <- disp_risk_2014by %>% 
  select(census_tract_id, geoid10)

# Join GEOIDs with 2018by data
disp_2018_grp <- disp_2018_grp %>% 
  left_join(geoid_info, by = "census_tract_id") %>% 
  mutate(geoid10 = as.character(geoid10))

disp_2018_grp <- disp_2018_grp %>% select(-census_tract_id)

# Create spatial dataset
psrc_tracts_dc <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(disp_2018_grp, by = c("GEOID"= "geoid10"))

# Export datasets -----------------------------------------------
write_csv(disp_2018_grp, file = "./data/13-Development Capacity/13_DevelopmentCapacity.csv")
write_rds(psrc_tracts_dc, "./data/13-Development Capacity/tract_13_DevelopmentCapacity.rds")
