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
                              table.names = 'B19113', #Median Family Income in the Past 12 Months (in 2024 Inflation-Adjusted Dollars)
                              years = c(as.numeric(acs_data_year)),
                              acs.type = 'acs5')

# 5y estimates, by county
base_acs_data_county <- get_acs_recs(geography ='county', 
                                    table.names = 'B19113', #Median Family Income in the Past 12 Months (in 2024 Inflation-Adjusted Dollars)
                                    years = c(as.numeric(acs_data_year)),
                                    acs.type = 'acs5')

## Transforming data ----

# Tract-level data
acs_data_tract <- base_acs_data_tract %>%
  mutate(faminc_cat = case_when(variable=="B19113_001"~"total_tract_median")) %>% # to match 2021 data
  dplyr::select(GEOID, county, faminc_cat, estimate, moe) %>% # simplify data set
  rename(est=estimate) %>% # to match 2021 data
  pivot_wider(names_from = faminc_cat, 
              values_from = c(est, moe)) # pivot data to facilitate calculations

# There are 13 tracts with 0 as the median household income - this is misleading and should be converted to NA
acs_data_tract <- acs_data_tract %>% 
  mutate(across(where(is.numeric), # select only numeric columns
                ~ na_if(., 0))) # replace 0 with NA 

# County-level data
acs_data_county <- base_acs_data_county %>%
  mutate(faminc_cat = case_when(variable=="B19113_001"~"county_mfi")) %>% # to match 2021 data
  dplyr::select(GEOID, name, faminc_cat, estimate, moe) %>% # simplify data set
  rename(est=estimate,
         county=name,
         GEOID_cty=GEOID) %>% # to match 2021 data
  filter(county!="Region") %>% 
  pivot_wider(names_from = faminc_cat, 
              values_from = c(est, moe)) %>% # pivot data to facilitate calculations
  rename(county_mfi_est=est_county_mfi,
         county_mfi_moe=moe_county_mfi) # to match 2021 data

# merge tract and county data
acs_data <- acs_data_tract %>% 
  left_join(acs_data_county,
            by="county")

## Calculate median household income compared to county median by census tract ----
# assign mfi levels
mfi_calc <- acs_data %>%
  mutate(per_mfi = est_total_tract_median / county_mfi_est,
         level_mfi_80 = ifelse(per_mfi <= 0.80, 1, 0),
         level_mfi_120 = ifelse(per_mfi >= 1.20, 1, 0)) %>% 
         # level_mfi = factor(ifelse(per_mfi <= 0.80, 1,
         #                           ifelse(per_mfi >= 1.20, 2, 0)),
         #                    levels = c(0, 1, 2),
         #                    labels = c("80%-120%MFI", "<80%MFI", ">120%MFI"))) %>%
  select(-c(county, GEOID_cty))

# Creating spatial data set --------------------------------------

## Calculating reliability measures for ACS estimates ----
# old method (2021 update): unlike the other indicators MOEs were not calculated for this indicator
# new method (2026 update): using moe_prop() from the tidycensus library (https://psrc.github.io/psrccensus/articles/calculate-reliability-moe-transformed-acs.html)

# use the moe_prop function from tidycensus: moe_prop(num, denom, moe_num, moe_denom)
mfi_moe_values <- mfi_calc %>%
  # rowwise() %>%
  mutate(moe_prop_mfi=moe_prop(num=est_total_tract_median,
                               denom=county_mfi_est, 
                               moe_num=moe_total_tract_median, 
                               moe_denom=county_mfi_moe)) %>% 
  reliability_calcs(estimate='per_mfi', 
                    moe='moe_prop_mfi')


# Connecting to ElmerGeo for census geographies through Portal, instead of saving spatial file to the project folder
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract <- st_read(tracts20.url)

## Joining spatial and tabular data by tract ----
tract <- merge(tract, mfi_moe_values,
                     by.x="geoid20",
                     by.y="GEOID",
                     all.x=TRUE)

# Identify tracts with mfi <80% that are next to at least one with mfi >120%
tract <- tract %>% mutate(neighbor = 0)
tract_mfi_80 <- which(tract$level_mfi_80 == 1) # Select tracts with mfi <80%
tract_mfi_120 <- which(tract$level_mfi_120 == 1) # Select tracts with mfi >120%

# Matrix with entries the indices of tracts >120% that touch the tract <80% represented by the row
touch_mat <- st_touches(tract[tract_mfi_80,], tract[tract_mfi_120,])
# For each row, check if the touching tracts have >120% and enter 1 if so
for(i in 1:length(tract_mfi_80)){
  touching <- touch_mat[[i]]
  out <- ifelse(length(touching) > 0, 1, 0)
  tract[tract_mfi_80[i], "neighbor"] <- out
}

# Add neighbor column to mfi
mfi <- left_join(mfi_calc, 
                 as_tibble(tract) %>% 
                   select(geoid20, neighbor), 
                 by = c("GEOID" = "geoid20"))


# Exporting data sets ----------------------------------------------- 

## Final data set for percentage by census tract, plus calculation components ----
median_household_income <- mfi %>% 
  st_drop_geometry() %>% 
  select(GEOID, contains("est"), per_mfi, starts_with("level"), neighbor)
write_csv(median_household_income, file = "./data/12-Proximity to High Income Neighborhood/12_ProximityToHighIncomeNeighborhood.csv")

## Final data set with tract information and MOE calculations ----
write_rds(tract, "./data/12-Proximity to High Income Neighborhood/tract_12_ProximityToHighIncomeNeighborhood.rds") 

# To compare with 2021 update:
# rds_2021 <- readRDS("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/12-Proximity to High Income Neighborhood/tract_12_ProximityToHighIncomeNeighborhood.rds")








# ACS key to access the survey data. Key can be obtained from: http://api.census.gov/data/key_signup.html 
# Run once to add CENSUS API key to .Renviron file
# census_api_key("16995506559e358a55d32e63541106a22b34acd7",install = TRUE)
readRenviron("~/.Renviron")

# Name of ACS datasets
v15 <- load_variables(2019, "acs5", cache = TRUE)

# Households that are renters --------------------------------------

# Download data from api ACS (family income)
# Rename variables
dataset <- "B19113_001"

total_tract_median <- get_acs(geography = "tract", variables = dataset, year = 2019, county = c("033","035","053","061"), state = "53")
colnames(total_tract_median) <- c("GEOID", 
                                  "NAME", 
                                  "variable", 
                                  paste("est_total", "tract_median", sep = "_"), 
                                  paste("moe_total", "tract_median", sep = "_"))
total_tract_median <- total_tract_median[,c(-2, -3)]
  
mfi <- total_tract_median %>%
    mutate(county = factor(str_sub(GEOID, 1, 5)))
  
county_mfi <-  get_acs(geography = "county", variables = dataset, year = 2019, county = c("033","035","053","061"), state = "53")
county_mfi <- county_mfi[-c(2,3)]
colnames(county_mfi) <- c("county", "county_mfi_est", "county_mfi_moe")

mfi <- mfi %>%
  left_join(county_mfi, by = "county") %>%
  mutate(per_mfi = est_total_tract_median / county_mfi_est,
         level_mfi_80 = ifelse(per_mfi <= 0.80, 1, 0),
         level_mfi_120 = ifelse(per_mfi >= 1.20, 1, 0),
         level_mfi = factor(ifelse(per_mfi <= 0.80, 1,
                            ifelse(per_mfi >= 1.20, 2, 0)),
                            levels = c(0, 1, 2),
                            labels = c("80%-120%MFI", "<80%MFI", ">120%MFI"))) %>%
  select(GEOID, 
         est_total_tract_median, 
         county_mfi_est, 
         per_mfi, 
         level_mfi_80, 
         level_mfi_120)

# Create spatial dataset 
tract <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/gis/tract2010_nowater.shp")
tract <- tract %>%
  mutate(GEOID = factor(GEOID10))
tract <- st_transform(tract, 4269)

# Join datasets  ----------------------------------------------- 
tract <- tract %>%
  left_join(mfi, by = ("GEOID")) 

# Identify tracts with mfi <80% that are next to at least one with mfi >120%
tract <- tract %>% mutate(neighbor = 0)
tract_mfi_80 <- which(tract$level_mfi_80 == 1) # Select tracts with mfi <80%
tract_mfi_120 <- which(tract$level_mfi_120 == 1) # Select tracts with mfi >120%

# Matrix with entries the indices of tracts >120% that touch the tract <80% represented by the row
touch_mat <- st_touches(tract[tract_mfi_80,], tract[tract_mfi_120,])
# For each row, check if the touching tracts have >120% and enter 1 if so
for(i in 1:length(tract_mfi_80)){
  touching <- touch_mat[[i]]
  out <- ifelse(length(touching) > 0, 1, 0)
  tract[tract_mfi_80[i], "neighbor"] <- out
}

# Add neighbor column to mfi
mfi <- left_join(mfi, 
                 as_tibble(tract) %>% 
                   select(GEOID, neighbor), by = "GEOID"
                 )
 

# Export datasets ----------------------------------------------- 

# Final dataset for percentage by census tract, plus calculation components
write_csv(mfi, file = "./data/12-Proximity to High Income Neighborhood/12_ProximityToHighIncomeNeighborhood.csv")

# Final dataset with tract information
write_rds(tract, "./data/12-Proximity to High Income Neighborhood/tract_12_ProximityToHighIncomeNeighborhood.rds")
