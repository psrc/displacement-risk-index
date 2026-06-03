# Generate indicator data set
# Percent of population within ? mile to frequent or high capacity transit

# Libraries --------------------------------------
# install.packages("tidyverse")
# install.packages("tidycensus")
library(tidyverse)
library(tidycensus)
library(psrccensus)
library(sf)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2026") 


# Load data set ----------------------------------------------- 
# 2023 base year
ptt_2023 <- read.csv("./data/08-Proximity to Transit/08_proximityToTransit.csv")

# Creating spatial data set --------------------------------------
# Connecting to ElmerGeo for census geographies through Portal, instead of saving spatial file to the project folder
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract, ptt_2023,
               by="geoid20",
               all.x=TRUE)

tract$percent_pop_quarter_mile <- round(tract$percent_pop_quarter_mile, digits = 3)

# Exporting data sets ----------------------------------------------- 
## Final data set for distance by census tract, plus calculation components ----
# the csv requires no additional calculations 

## Final data set with tract information ----
write_rds(tract, "./data/08-Proximity to Transit/tract_08_proximityToTransit.rds")

# To compare with 2021 update:
# rds_2021 <- readRDS("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/08-Proximity to Transit/tract_08_proximityToTransit.rds")
