# Generate indicator data set

# Libraries --------------------------------------
# install.packages("tidyverse")
# install.packages("tidycensus")
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
base_acs_data <- get_acs_recs(geography ='tract', 
                              table.names = 'B03002', #subject table code
                              years = c(as.numeric(acs_data_year)),
                              acs.type = 'acs5')

## Transforming data ----
# Define variables of interest (and order based on 2021 update data)
variables_list <- c("B03002_001", #Estimate!!Total
                    "B03002_003", #Estimate!!Total:!!Not Hispanic or Latino:!!White alone
                    "B03002_013", #Estimate!!Total:!!Hispanic or Latino:!!White alone 
                    "B03002_002", #Estimate!!Total:!!Not Hispanic or Latino
                    "B03002_012") #Estimate!!Total:!!Hispanic or Latino)

acs_data <- base_acs_data %>%
  filter(variable %in% variables_list) %>% 
  mutate(variable=factor(variable, levels = variables_list)) %>% # to match 2021 data
  arrange(variable) %>% # to match 2021 data
  mutate(race_cat = case_when(variable=="B03002_001"~"total_race",
                              variable=="B03002_003"~"nhispanic_white",
                              variable=="B03002_013"~"hispanic_white",
                              variable=="B03002_002"~"total_nhispanic",
                              variable=="B03002_012"~"total_hispanic")) %>% 
  dplyr::select(GEOID, race_cat, estimate, moe) %>% # simplify data set
  rename(est=estimate) %>% # to match 2021 data
  pivot_wider(names_from = race_cat, 
              values_from = c(est, moe)) # pivot data to facilitate calculations

## Calculating proportion and percent of POC by census tract ----
# old method (2021 update): prop_notwhite = (est_total_hispanic + est_total_nhispanic - est_nhispanic_white) / est_total_race
# new method (2026 update): simplified - using fewer variables with larger sample yields a better MOE 
race_calc <- acs_data %>% 
  mutate(prop_notwhite = (est_total_race - est_nhispanic_white) / est_total_race,
         per_notwhite = prop_notwhite * 100)


# Creating spatial data set --------------------------------------

## Calculating reliability measures for ACS estimates ----
# old method (2021 update): manual calculations using SE and z-score of 1.645
# new method (2026 update): using moe_prop() from the tidycensus library (https://psrc.github.io/psrccensus/articles/calculate-reliability-moe-transformed-acs.html)

# use the moe_prop function from tidycensus: moe_prop(num, denom, moe_num, moe_denom)
tract_moe_prop <- tract %>% 
  dplyr::mutate(prop_white=(est_nhispanic_white/est_total_race),
                moe_prop=tidycensus::moe_prop(num=est_nhispanic_white,
                                              denom=est_total_race,
                                              moe_num=moe_nhispanic_white, 
                                              moe_denom=moe_total_race)) %>% 
  reliability_calcs(estimate='prop_white', 
                    moe='moe_prop') %>% 
  mutate(est_POC=est_total_race-est_nhispanic_white,
         prop_POC=est_POC/est_total_race)

# use the moe_sum function from tidycensus: moe_sum(moe, estimate = NULL, na.rm = FALSE)
# use the moe_prop function from tidycensus: moe_prop(num, denom, moe_num, moe_denom)
notwhite_moe_values <- race_calc %>%
  # rowwise() %>%
  mutate(est_notwhite=(est_total_race - est_nhispanic_white)) %>% 
  mutate(moe_est_notwhite=moe_sum(estimate=c(est_total_race, est_nhispanic_white),
                                  moe=c(moe_total_race, moe_nhispanic_white))) %>%
  mutate(moe_prop_notwhite=moe_prop(num=est_notwhite,
                                    denom=est_total_race, 
                                    moe_num=moe_est_notwhite, 
                                    moe_denom=moe_total_race)) %>%
  reliability_calcs(estimate='prop_notwhite', 
                    moe='moe_prop_notwhite')


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
write_csv(race, file = "./data/01-People of Color/01_PeopleOfColor.csv")

## Final data set with tract information and MOE calculations ----
write_rds(notwhite_moe_values, "./data/01-People of Color/tract_01_PeopleOfColor.rds")

# To compare with 2021 update:
# rds_2021 <- readRDS("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/01-People of Color/tract_01_PeopleOfColor.rds")