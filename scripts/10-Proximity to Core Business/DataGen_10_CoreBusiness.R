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
business <- read.csv("./data/10-Proximity to Core Business/tract_dist_amenity.csv")

# Separate out the different business types
str(business)

supermarket <- business %>% 
  select(GEOID, supermarket)
pharmacy <- business %>% 
  select(GEOID, pharmacy)
restaurant <- business %>% 
  select(GEOID, restaurant)

# Creating spatial data set --------------------------------------
# Connecting to ElmerGeo for census geographies through Portal, instead of saving spatial file to the project folder
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract, business,
               by.x="geoid20", by.y="GEOID",
               all.x=TRUE)

# Exporting data sets ----------------------------------------------- 
## Final data set for distance by census tract ----
write_csv(supermarket, file = "./data/10-Proximity to Core Business/10_a_ProximityCoreBusinessSupermarket.csv")
write_csv(pharmacy, file = "./data/10-Proximity to Core Business/10_b_ProximityCoreBusinessPharmacy.csv")
write_csv(restaurant, file = "./data/10-Proximity to Core Business/10_c_ProximityCoreBusinessRestaurant.csv")

## Final data set with tract information ----
write_rds(tract, "./data/10-Proximity to Core Business/tract_10_CoreBusinness.rds")
