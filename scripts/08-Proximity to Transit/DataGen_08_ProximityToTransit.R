# Generate indicator dataset ------------------------------------------------
# 8	Proximity to Transit - Percent of population within ¼ mile to frequent or high capacity transit

# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(sf)
# install.packages(tigris)
library(tidyverse)
library(sf)
library(tigris)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Load data  ----------------------------------------------- 
ptt_2018 <- read.csv("X:/Trans/2022 Regional Transportation Plan/Data Team/2018_base_year/displacement/hct_population_coverage.csv")

ptt_2018 <- ptt_2018 %>% 
  mutate(percent_pop_quarter_mile = percent_pop_quarter_mile * 100)

# Export datasets ----------------------------------------------- 
# Final dataset for percentage by census tract, plus calculation components
write_csv(ptt_2018, file = "./data/08-Proximity to Transit/08_proximityToTransit.csv")

# Final dataset with tract information
tract <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(ptt_2018 %>% mutate(geoid10 = as.character(geoid10)), by = c("GEOID"= "geoid10"))
write_rds(tract, "./data/08-Proximity to Transit/tract_08_proximityToTransit.rds")
