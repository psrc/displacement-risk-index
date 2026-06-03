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
base_acs_data <- get_acs_recs(geography ='tract', 
                              table.names = 'S1701', #Poverty Status in the Past 12 Months
                              years = c(as.numeric(acs_data_year)),
                              acs.type = 'acs5')

## Transforming data ----
# Define variables of interest (and order based on 2021 update data)
variables_list <- c("S1701_C01_001", #Estimate!!Total!!Population for whom poverty status is determined
                    "S1701_C01_042") #Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!200 percent of poverty level

acs_data <- base_acs_data %>%
  filter(variable %in% variables_list) %>% 
  mutate(pov_cat = case_when(variable=="S1701_C01_001"~"pov",
                              variable=="S1701_C01_042"~"200pov")) %>% 
  dplyr::select(GEOID, pov_cat, estimate, moe) %>% # simplify data set
  rename(est=estimate) %>% # to match 2021 data
  pivot_wider(names_from = pov_cat, 
              values_from = c(est, moe)) # pivot data to facilitate calculations


## Calculate percentage of population below 200% of poverty level by census tract ----
pov_calc <- acs_data %>% 
  mutate(prop_poverty = est_200pov/est_pov,
         per_poverty = prop_poverty * 100)


# Creating spatial data set --------------------------------------

## Calculating reliability measures for ACS estimates ----
# old method (2021 update): manual calculations using SE and z-score of 1.645
# new method (2026 update): using moe_sum() from the tidycensus library (https://psrc.github.io/psrccensus/articles/calculate-reliability-moe-transformed-acs.html)

# use the moe_prop function from tidycensus: moe_prop(num, denom, moe_num, moe_denom)
pov_moe_values <- pov_calc %>%
  # rowwise() %>%
  mutate(moe_prop_poverty=moe_prop(num=est_200pov,
                                   denom=est_pov, 
                                   moe_num=moe_200pov, 
                                   moe_denom=moe_pov )) %>%
  reliability_calcs(estimate='prop_poverty', 
                    moe='moe_prop_poverty')


# Connecting to ElmerGeo for census geographies through Portal, instead of saving spatial file to the project folder
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract, pov_moe_values,
               by.x="geoid20",
               by.y="GEOID",
               all.x=TRUE)

tract$per_poverty <- round(tract$per_poverty, digits = 2)


# Exporting data sets ----------------------------------------------- 

## Final data set for percentage by census tract, plus calculation components ----
pov <- pov_calc %>% select(GEOID, starts_with("est"), per_poverty)
write_csv(pov, file = "./data/06-Household Income/06_HouseholdIncome.csv")

## Final data set with tract information and MOE calculations ----
write_rds(tract, "./data/06-Household Income/tract_06_HouseholdIncome.rds")

# To compare with 2021 update:
# rds_2021 <- readRDS("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/06-Household Income/tract_06_HouseholdIncome.rds")








# Data was downloaded from ACS because it was not available in tidycensus
# TABLE S1701
rawdata <- read_csv("./data/06-Household Income/ACS_19_5YR_S1701.csv")
rawdata <- rawdata[-1,]

# Estimate the percentage of pop below 200% of poverty level
# Column names for the variables needed
data <- rawdata %>%
  select(GEO_ID, S1701_C01_001E, S1701_C01_001M, S1701_C01_042E, S1701_C01_042M) %>%
  rename(GEOID = "GEO_ID", est_pov = "S1701_C01_001E", moe_pov = "S1701_C01_001M", 
         est_200pov = "S1701_C01_042E", moe_200pov = "S1701_C01_042M") %>%
  mutate(GEOID = str_sub(GEOID, 10))

data[,-1] <- lapply(data[,-1], as.numeric)

data <- data %>% mutate(per_poverty = est_200pov / est_pov * 100) 

# Create spatial dataset with MOE calculation
spdata <- data %>%
  mutate(GEOID = factor(GEOID), 
         county = factor(str_sub(GEOID, 1, 5)))

tract <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/gis/tract2010_nowater.shp")
tract <- tract %>%
  mutate(GEOID = factor(GEOID10))

# Project tracts
tract <- st_transform(tract, 4269)

# Join datasets  ----------------------------------------------- 
tract <- tract %>%
  left_join(spdata, by = ("GEOID")) 

# margin of error for proportion of severe poverty by tract
# ACS MOE eqns: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2020_ACS_Accuracy_Document_Worked_Examples.pdf
levels <- c("<=5%", "5%-10%", ">10%")
z <- 1.645
tract <- tract %>% mutate(se_200pov = moe_200pov/z,
                          se_pov = moe_pov/z,
                          se_prop_poverty = 1/est_pov * sqrt(se_200pov^2 - (est_200pov^2/est_pov^2) * se_pov^2),
                          se_per_poverty = se_prop_poverty * 100,
                          moe_per_poverty = se_per_poverty * z
)
tract <- tract %>% mutate(err_per_poverty = moe_per_poverty/per_poverty)
tract$err_per_poverty_grp <- factor(ifelse(tract$err_per_poverty <= 0.05, 
                                            "<=5%",
                                            ifelse(tract$err_per_poverty > 0.05 & tract$err_per_poverty <= 0.1, 
                                                   "5%-10%",
                                                   ">10%")), 
                                     levels = levels)
tract <- tract %>% select(-c(se_200pov, se_pov, se_prop_poverty, se_per_poverty))

# Export datasets -----------------------------------------------

# Final dataset for percentage by census tract, plus calculation components
write_csv(data %>% select(-starts_with("moe")), file = "./data/06-Household Income/06_HouseholdIncome.csv")

# Final dataset with tract information and MOE calculation
write_rds(tract, "./data/06-Household Income/tract_06_HouseholdIncome.rds")
