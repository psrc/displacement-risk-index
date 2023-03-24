# Generate indicator dataset ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(tidycensus)
# install.packages(sf)
library(tidyverse)
library(tidycensus)
library(sf)

setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/")

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
