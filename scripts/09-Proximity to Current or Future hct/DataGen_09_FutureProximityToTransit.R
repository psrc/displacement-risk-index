# 9 Proximity to Current or Future Link Light Rail & Streetcar
# Percent of area within .5 miles to 2030 ferry, street car, commuter rail and light rail and 
# .25 miles to 2030 BRT 
# Generate indicator dataset ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(tigris)
library(tidyverse)
library(tigris)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Load current data  ----------------------------------------------- 
prox2030 <- read.csv("X:/Trans/2022 Regional Transportation Plan/Data Team/2030/displacement/hct_area_coverage_quarter_and_half_mile.csv")
prox2030 <- prox2030 %>% mutate(percent_hct = percent_hct * 100)

### Note: Tracts with value 0 are not present in prox2030 file, need to reinstate them
# Pull GEOIDs from another indicator
temp <- read.csv("Y:/Vision 2050/Data/Displacement/Displacement Index 2021/data/01-People of Color/01_PeopleOfColor.csv")
temp <- temp %>% select(GEOID)
prox2030 <- prox2030 %>% right_join(temp, by = c("geoid10" = "GEOID"))
prox2030 <- prox2030 %>% mutate(percent_hct = replace_na(percent_hct, 0))

# Export datasets ----------------------------------------------- 

# Final dataset for percentage by census tract, plus calculation components
write_csv(prox2030, file = "./data/09-Proximity to Current or Future hct/09_futureProximityToTransit.csv")
psrc_tracts_upd <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(prox2030 %>% mutate(geoid10 = as.character(geoid10)), by = c("GEOID"= "geoid10"))

# Final dataset with tract information
write_rds(psrc_tracts_upd, "./data/09-Proximity to Current or Future hct/tract_09_futureProximityToTransit.rds")
