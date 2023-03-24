# Generate indicator dataset ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages("tidyverse")
# install.packages("tidycensus")
# install.packages("sf")
library(tidyverse)
library(tidycensus)
library(sf)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/")

# ACS key to access the survey data. Key can be obtained from: http://api.census.gov/data/key_signup.html 
# Run once to add CENSUS API key to .Renviron file
# census_api_key("16995506559e358a55d32e63541106a22b34acd7",install = TRUE)
readRenviron("~/.Renviron")

# Name of ACS datasets
v15 <- load_variables(2019, "acs5", cache = TRUE)
v15_16 <- load_variables(2016, "acs5", cache = TRUE)

# 1. Median gross rent by # bedrooms --------------------------------------

# Download data from api ACS
# Rename variables
names <- c("tract_median_rent","tract_0_rooms", "tract_1_rooms", "tract_2_rooms",
           "tract_3_rooms", "tract_4_rooms", "tract_5_rooms")
number <- paste(rep("00",7), as.character(seq(1:7)), sep = "")
dataset <- "B25031"

# Median rate by census tract and by number of bedrooms--------------------------------------
for(i in 1:length(names)) {
  a <- paste("total", names[i], sep = "_")
  assign(a, get_acs(geography = "tract", variables = paste(dataset, number[i], sep = "_"), year = 2019, county = c("033","035","053","061"), state = "53"))
  b <- get(a)
  colnames(b) <- c("GEOID", "NAME", "variable", paste("est_total", names[i], sep = "_"), paste("moe_total", names[i], sep = "_"))
  b <- b[,c(-2, -3)]
  b <- b %>%
    mutate(GEOID_cty = str_sub(GEOID, 1, 5))
  assign(a, b)
  rm(a,b)
}

# Median gross rent by county and by bedrooms--------------------------------------
names <- c("cty_median_rent","cty_0_rooms", "cty_1_rooms", "cty_2_rooms",
           "cty_3_rooms", "cty_4_rooms", "cty_5_rooms")

for(i in 1:length(names)) {
  a <- paste("total", names[i], sep = "_")
  assign(a, get_acs(geography = "county", variables = paste(dataset, number[i], sep = "_"), year = 2019, county = c("033","035","053","061"), state = "53"))
  b <- get(a)
  colnames(b) <- c("GEOID_cty", "NAME", "variable", paste("est_total", names[i], sep = "_"), paste("moe_total", names[i], sep = "_"))
  b <- b[,c(-2, -3)]
  assign(a, b)
  rm(a,b)
}

# Outer join of datasets
median_rent <- `total_tract_median_rent` %>%
  full_join(`total_tract_0_rooms`,   by = "GEOID") %>%
  full_join(`total_tract_1_rooms`,   by = "GEOID") %>%
  full_join(`total_tract_2_rooms`,   by = "GEOID") %>%
  full_join(`total_tract_3_rooms`,   by = "GEOID") %>%
  full_join(`total_tract_4_rooms`,   by = "GEOID") %>%
  full_join(`total_tract_5_rooms`,   by = "GEOID") 

median_rent <- median_rent %>%
  full_join(`total_cty_median_rent`, by = "GEOID_cty") %>%
  full_join(`total_cty_0_rooms`,     by = "GEOID_cty") %>%
  full_join(`total_cty_1_rooms`,     by = "GEOID_cty") %>%
  full_join(`total_cty_2_rooms`,     by = "GEOID_cty") %>%
  full_join(`total_cty_3_rooms`,     by = "GEOID_cty") %>%
  full_join(`total_cty_4_rooms`,     by = "GEOID_cty") %>%
  full_join(`total_cty_5_rooms`,     by = "GEOID_cty") 

# Calculate ratio of median rent by bedrooms to the regional median, or each county's median
median_rent <- median_rent %>%
       mutate(ind_rent = est_total_tract_median_rent / est_total_cty_median_rent,
              ind_rent_0_rooms = est_total_tract_0_rooms / est_total_cty_0_rooms,
              ind_rent_1_rooms = est_total_tract_1_rooms / est_total_cty_1_rooms,
              ind_rent_2_rooms = est_total_tract_2_rooms / est_total_cty_2_rooms,
              ind_rent_3_rooms = est_total_tract_3_rooms / est_total_cty_3_rooms,
              ind_rent_4_rooms = est_total_tract_4_rooms / est_total_cty_4_rooms,
              ind_rent_5_rooms = est_total_tract_5_rooms / est_total_cty_5_rooms)

# Create spatial dataset 
data <- median_rent
data <- data %>%
  mutate(GEOID = factor(GEOID), 
         county = factor(str_sub(GEOID, 1, 5)))

tract <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/gis/tract2010_nowater.shp")
tract <- tract %>%
  mutate(GEOID = factor(GEOID10))

# Project tracts
tract <- st_transform(tract, 4269)

# Join datasets  ----------------------------------------------- 
tract <- tract %>%
  left_join(data, by = "GEOID")
  
# Export dataset to csv
write_csv(median_rent %>% select(-starts_with("moe_"), -starts_with("GEOID_")), file = "./data/14-Median Rent/14_MedianRent.csv")
write_rds(tract, "./data/14-Median Rent/tract_14_MedianRent.rds")
