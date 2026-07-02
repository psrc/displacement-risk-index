# Generate indicator data set

# Libraries --------------------------------------
# install.packages(tidyverse)
# install.packages(tidycensus)
library(tidyverse)
library(tidycensus)
library(readxl) #read_excel()
library(psrccensus)
library(psrcelmer) # access Elmer CHAS data
library(sf)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2026")

# Load data set -------------------------------------- 
# 2023 base year
job_auto <- read.csv("./data/07-Access to Jobs/auto_jobs_access.csv")
job_transit <- read.csv("./data/07-Access to Jobs/transit_jobs_access.csv")

## Transforming data ----
# Selecting tracts only
job_auto_tracts <- job_auto %>% 
  filter(geography_group == 'Census2020Tract')%>%
  mutate(geography=as.numeric(geography),
         value=round(value)) 

job_transit_tracts <- job_transit %>%
  filter(geography_group == 'Census2020Tract')%>%
  mutate(geography=as.numeric(geography),
         value=round(value)) 

# Exporting data sets --------------------------------------
## Final dataset, auto ----
write_csv(job_auto_tracts %>% 
            select(GEOID = geography, NumJobsAuto = value), 
          file = "./data/07-Access to Jobs/07_a_AccesstoJobs.csv")
## Final dataset, transit ----
write_csv(job_transit_tracts %>% 
            select(GEOID = geography, NumJobsTransit = value), 
          file = "./data/07-Access to Jobs/07_b_AccesstoJobs.csv")
