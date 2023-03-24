# Generate indicator dataset ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages(tidyverse)
library(tidyverse)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

jobA_18 <- read.csv("X:/Trans/2022 Regional Transportation Plan/Data Team/2018_base_year/displacement/auto_jobs_access.csv")

# Selecting tracts only
jobA_18_tracts = jobA_18 %>% filter(geography_group == 'Census2010Tract')
jobA_18_tracts$geography <- as.numeric(jobA_18_tracts$geography)
jobA_18_tracts$value <- round(jobA_18_tracts$value)

transit2018_upd <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/07-Access to Jobs/transit_jobs_access.csv")
transit2018_upd_tracts <- transit2018_upd %>% filter(geography_group == 'Census2010Tract')
transit2018_upd_tracts$value <- round(transit2018_upd_tracts$value)

# Export datasets ----------------------------------------------- 
# Final dataset, auto
write_csv(jobA_18_tracts %>% select(GEOID = geography, NumJobsAuto = value), file = "./data/07-Access to Jobs/07a_AccesstoJobs.csv")
# Final dataset, transit
write_csv(transit2018_upd_tracts %>% select(GEOID = geography, NumJobsTransit = value), file = "./data/07-Access to Jobs/07b_AccesstoJobs.csv")
