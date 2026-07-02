# Generate indicator data set

# Libraries --------------------------------------
# install.packages(tidyverse)
# install.packages(tidycensus)
library(tidyverse)
library(tidycensus)
library(readxl) #read_excel()
library(psrccensus)
library(psrcelmer) # access Elmer CHAS data
library(sf)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2026")


# Load data set -------------------------------------- 
# 2026 FLU update
devcap <- read.csv("./data/13-Development Capacity/08_proximityToTransit.csv")

## Transforming data ----




# Creating spatial data set --------------------------------------
# Connecting to ElmerGeo for census geographies through Portal, instead of saving spatial file to the project folder
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract, notwhite_moe_values,
               by.x="geoid20",
               by.y="GEOID",
               all.x=TRUE)

tract$per_notwhite <- round(tract$per_notwhite, digits = 2)


# Exporting data sets ----------------------------------------------- 

## Final data set for percentage by census tract, plus calculation components ----
race <- race_calc %>% select(GEOID, starts_with("est"), per_notwhite)
write_csv(race, file = "./data/13-Development Capacity/13_DevelopmentCapacity.csv")

## Final data set with tract information and MOE calculations ----
write_rds(notwhite_moe_values, "./data/13-Development Capacity/tract_13_DevelopmentCapacity.rds")

# To compare with 2021 update:
# rds_2021 <- readRDS("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/13-Development Capacity/tract_13_DevelopmentCapacity.rds")








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
