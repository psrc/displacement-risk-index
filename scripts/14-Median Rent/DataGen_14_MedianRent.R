# Generate indicator data set

# Libraries --------------------------------------
# install.packages(tidyverse)
# install.packages(tidycensus)
library(tidyverse)
library(tidycensus)
library(psrccensus)
library(sf)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2026")

# The 2021 update (2019 ACS data) accessed the survey data using the API, which required a key (http://api.census.gov/data/key_signup.html), but since 2021, the psrccensus library was developed. The 2026 (2024 ACS data) update will access the survey data using psrccensus (https://psrc.github.io/psrccensus/articles/psrccensus.html).
acs_data_year <- "2024"

# Accessing ACS data --------------------------------------
# 5y estimates, by tract
base_acs_data_tract <- get_acs_recs(geography ='tract', 
                                    table.names = 'B25031', #Median Gross Rent by Bedrooms
                                    years = c(as.numeric(acs_data_year)),
                                    acs.type = 'acs5')
# 5y estimates, by tract
base_acs_data_county <- get_acs_recs(geography ='county', 
                                     table.names = 'B25031', #Median Gross Rent by Bedrooms
                                     years = c(as.numeric(acs_data_year)),
                                     acs.type = 'acs5')

## Transforming data ----
# Define variables of interest (and order based on 2021 update data)
variables_list <- c("B25031_001", #Estimate!!Median gross rent --!!Total
                    "B25031_002", #Estimate!!Median gross rent --!!Total:!!No bedroom
                    "B25031_003", #Estimate!!Median gross rent --!!Total:!!1 bedroom
                    "B25031_004", #Estimate!!Median gross rent --!!Total:!!2 bedrooms
                    "B25031_005", #Estimate!!Median gross rent --!!Total:!!3 bedrooms
                    "B25031_006", #Estimate!!Median gross rent --!!Total:!!4 bedrooms
                    "B25031_007") #Estimate!!Median gross rent --!!Total:!!5 or more bedrooms

# Tract-level data
acs_data_tract <- base_acs_data_tract %>%
  mutate(medrent_cat = case_when(variable=="B25031_001"~"tract_median_rent",
                                 variable=="B25031_002"~"tract_0_rooms",
                                 variable=="B25031_003"~"tract_1_rooms",
                                 variable=="B25031_004"~"tract_2_rooms",
                                 variable=="B25031_005"~"tract_3_rooms",
                                 variable=="B25031_006"~"tract_4_rooms",
                                 variable=="B25031_007"~"tract_5_rooms")) %>% 
  dplyr::select(GEOID, county, medrent_cat, estimate, moe) %>% # simplify data set
  rename(est_total=estimate) %>% # to match 2021 data
  pivot_wider(names_from = medrent_cat,
              values_from = c(est_total, moe)) %>% # pivot data to facilitate calculations
  mutate(across(where(is.numeric), # select only numeric columns
                ~ na_if(., 0))) # replace 0 with NA - 0 median rent is misleading when there's no data
  

# County-level data
acs_data_county <- base_acs_data_county %>%
  mutate(medrent_cat = case_when(variable=="B25031_001"~"cty_median_rent",
                                 variable=="B25031_002"~"cty_0_rooms",
                                 variable=="B25031_003"~"cty_1_rooms",
                                 variable=="B25031_004"~"cty_2_rooms",
                                 variable=="B25031_005"~"cty_3_rooms",
                                 variable=="B25031_006"~"cty_4_rooms",
                                 variable=="B25031_007"~"cty_5_rooms")) %>% 
  dplyr::select(GEOID, name, medrent_cat, estimate, moe) %>% # simplify data set
  rename(est_total=estimate,
         county=name,
         GEOID_cty=GEOID) %>% # to match 2021 data
  filter(county!="Region") %>% 
  pivot_wider(names_from = medrent_cat,
              values_from = c(est_total, moe)) # pivot data to facilitate calculations

# merge tract and county data
acs_data <- acs_data_tract %>% 
  left_join(acs_data_county,
            by="county")

## Calculate ratio of median rent by bedrooms to the regional median, or each county's median by census tract ----
median_rent_calc <- acs_data %>%
  mutate(ind_rent = est_total_tract_median_rent / est_total_cty_median_rent,
         ind_rent_0_rooms = est_total_tract_0_rooms / est_total_cty_0_rooms,
         ind_rent_1_rooms = est_total_tract_1_rooms / est_total_cty_1_rooms,
         ind_rent_2_rooms = est_total_tract_2_rooms / est_total_cty_2_rooms,
         ind_rent_3_rooms = est_total_tract_3_rooms / est_total_cty_3_rooms,
         ind_rent_4_rooms = est_total_tract_4_rooms / est_total_cty_4_rooms,
         ind_rent_5_rooms = est_total_tract_5_rooms / est_total_cty_5_rooms)


# Creating spatial data set --------------------------------------

## Calculating reliability measures for ACS estimates ----
# old method (2021 update): unlike the other indicators MOEs were not calculated for this indicator
# new method (2026 update): using moe_prop() from the tidycensus library (https://psrc.github.io/psrccensus/articles/calculate-reliability-moe-transformed-acs.html)

# use the moe_prop function from tidycensus: moe_prop(num, denom, moe_num, moe_denom)
median_rent_moe_values <- median_rent_calc %>%
  # rowwise() %>%
  mutate(moe_prop_medrent=moe_prop(num=est_total_tract_median_rent,
                                   denom=est_total_cty_median_rent, 
                                   moe_num=moe_tract_median_rent, 
                                   moe_denom=est_total_cty_median_rent)) %>% 
  reliability_calcs(estimate='ind_rent', 
                    moe='moe_prop_medrent') %>% 
  rename(se_medrent=se,
         cv_medrent=cv,
         reliability_medrent=reliability) %>% 
  # 0 rooms
  mutate(moe_prop_medrent_0_rooms=moe_prop(num=est_total_tract_0_rooms,
                                           denom=est_total_cty_0_rooms, 
                                           moe_num=moe_tract_0_rooms, 
                                           moe_denom=moe_cty_0_rooms)) %>% 
  reliability_calcs(estimate='ind_rent_0_rooms', 
                    moe='moe_prop_medrent_0_rooms') %>% 
  rename(se_0_rooms=se,
         cv_0_rooms=cv,
         reliability_0_rooms=reliability) %>%
  # 1 room
  mutate(moe_prop_medrent_1_rooms=moe_prop(num=est_total_tract_1_rooms,
                                           denom=est_total_cty_1_rooms, 
                                           moe_num=moe_tract_1_rooms, 
                                           moe_denom=moe_cty_1_rooms)) %>% 
  reliability_calcs(estimate='ind_rent_1_rooms', 
                    moe='moe_prop_medrent_1_rooms') %>% 
  rename(se_1_rooms=se,
         cv_1_rooms=cv,
         reliability_1_rooms=reliability) %>%
  # 2 rooms
  mutate(moe_prop_medrent_2_rooms=moe_prop(num=est_total_tract_2_rooms,
                                           denom=est_total_cty_2_rooms, 
                                           moe_num=moe_tract_2_rooms, 
                                           moe_denom=moe_cty_2_rooms)) %>% 
  reliability_calcs(estimate='ind_rent_2_rooms', 
                    moe='moe_prop_medrent_2_rooms') %>% 
  rename(se_2_rooms=se,
         cv_2_rooms=cv,
         reliability_2_rooms=reliability) %>%
  # 3 rooms
  mutate(moe_prop_medrent_3_rooms=moe_prop(num=est_total_tract_3_rooms,
                                           denom=est_total_cty_3_rooms, 
                                           moe_num=moe_tract_3_rooms, 
                                           moe_denom=moe_cty_3_rooms)) %>% 
  reliability_calcs(estimate='ind_rent_3_rooms', 
                    moe='moe_prop_medrent_3_rooms') %>% 
  rename(se_3_rooms=se,
         cv_3_rooms=cv,
         reliability_3_rooms=reliability) %>%
  # 4 rooms
  mutate(moe_prop_medrent_4_rooms=moe_prop(num=est_total_tract_4_rooms,
                                           denom=est_total_cty_4_rooms, 
                                           moe_num=moe_tract_4_rooms, 
                                           moe_denom=moe_cty_4_rooms)) %>% 
  reliability_calcs(estimate='ind_rent_4_rooms', 
                    moe='moe_prop_medrent_4_rooms') %>% 
  rename(se_4_rooms=se,
         cv_4_rooms=cv,
         reliability_4_rooms=reliability) %>%
  # 5 rooms
  mutate(moe_prop_medrent_5_rooms=moe_prop(num=est_total_tract_5_rooms,
                                           denom=est_total_cty_5_rooms, 
                                           moe_num=moe_tract_5_rooms, 
                                           moe_denom=moe_cty_5_rooms)) %>% 
  reliability_calcs(estimate='ind_rent_5_rooms', 
                    moe='moe_prop_medrent_5_rooms') %>% 
  rename(se_5_rooms=se,
         cv_5_rooms=cv,
         reliability_5_rooms=reliability)



# Connecting to ElmerGeo for census geographies through Portal, instead of saving spatial file to the project folder
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract, median_rent_moe_values,
               by.x="geoid20",
               by.y="GEOID",
               all.x=TRUE)

# tract <- tract %>% 
#   mutate(across(starts_with("ind"), ~ round(.x, 2)))


# Exporting data sets ----------------------------------------------- 

## Final data set for percentage by census tract, plus calculation components ----
median_rent <- median_rent_calc %>% select(GEOID, starts_with("est"), starts_with("ind"))
write_csv(median_rent, file = "./data/14-Median Rent/14_MedianRent.csv")

## Final data set with tract information and MOE calculations ----
write_rds(tract, "./data/14-Median Rent/tract_14_MedianRent.rds") 

# To compare with 2021 update:
# rds_2021 <- readRDS("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/14-Median Rent/tract_14_MedianRent.rds")







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
