# Generate indicator dataset ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(tidycensus)
library(tidyverse)
library(tidycensus)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# ACS key to access the survey data. Key can be obtained from: http://api.census.gov/data/key_signup.html 
# Run once to add CENSUS API key to .Renviron file
# census_api_key("16995506559e358a55d32e63541106a22b34acd7",install = TRUE)
readRenviron("~/.Renviron")

# Name of ACS datasets
v15 <- load_variables(2019, "acs5", cache = TRUE)

# Lack of a Bachelor's degree --------------------------------------

# Download data from ACS api
# Rename variables
total_pop      <- get_acs(geography = "tract", variables = "B15003_001E", year = 2019, county = c("033","035","053","061"), state = "53")
total_pop      <- rename(total_pop, est_total_pop = "estimate" )
total_pop      <- rename(total_pop, moe_total_pop = "moe" )

bachelor <- get_acs(geography = "tract", variables = "B15003_022E", year = 2019, county = c("033","035","053","061"), state = "53")
bachelor <- rename(bachelor, est_bachelor = "estimate" )
bachelor <- rename(bachelor, moe_bachelor = "moe" )

master  <- get_acs(geography = "tract", variables = "B15003_023E", year = 2019, county = c("033","035","053","061"), state = "53")
master  <- rename(master, est_master = "estimate" )
master  <- rename(master, moe_master = "moe" )

profesional <- get_acs(geography = "tract", variables = "B15003_024E", year = 2019, county = c("033","035","053","061"), state = "53")
profesional <- rename(profesional, est_profesional = "estimate" )
profesional <- rename(profesional, moe_profesional = "moe" )

phd  <- get_acs(geography = "tract", variables = "B15003_025E", year = 2019, county = c("033","035","053","061"), state = "53")
phd  <- rename(phd, est_phd = "estimate" )
phd  <- rename(phd, moe_phd = "moe" )

# Outer join of datasets
education <- total_pop %>%
  select(-NAME, -variable) %>%
  full_join(bachelor, by = "GEOID") %>% 
  select(-NAME, -variable) %>%
  full_join(master, by = "GEOID") %>%
  select(-NAME, -variable) %>%
  full_join(profesional, by = "GEOID") %>%
  select(-NAME, -variable) %>%
  full_join(phd, by = "GEOID") %>%
  select(-NAME, -variable)

# Calculate percentage of people without a bachelor's degree by census tract
education <- education %>% 
        mutate(prop_nobachelor = (est_total_pop - (est_bachelor + est_master + est_profesional + est_phd)) / est_total_pop,
               per_nobachelor = prop_nobachelor * 100)

# Create spatial dataset with MOE calculation
tract <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/gis/tract2010_nowater.shp")
tract <- tract %>%
  mutate(GEOID = factor(GEOID10))

# Project tracts
tract <- st_transform(tract, 4269)

# Join datasets  ----------------------------------------------- 
tract <- tract %>%
  left_join(education %>%
              mutate(GEOID = factor(GEOID), 
                     county = factor(str_sub(GEOID, 1, 5))), by = ("GEOID")) 

tract$per_nobachelor <- round(tract$per_nobachelor, digits = 2)

# margin of error for
# 1. count of ppl without a bachelor's degree per tract
# 2. proportion of ppl in a tract without a bachelor's degree 
# ACS MOE eqns: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2020_ACS_Accuracy_Document_Worked_Examples.pdf

# count of no bachelors
# = pop - bac - mas - pro - phd
# = est_total_pop - (est_bachelor + est_master + est_profesional + est_phd)

# SE(X +/- Y) -> eq 1 from ACS MOE eqns
# SE(X/Y) -> eq 3 from ACS MOE eqns
levels <- c("<=5%", "5%-10%", ">10%")
z <- 1.645
tract <- tract %>% mutate(count_nobachelor = est_total_pop - (est_bachelor + est_master + est_profesional + est_phd),
                        se_count_nobachelor = sqrt((moe_total_pop/z)^2 + (moe_bachelor/z)^2 + (moe_master/z)^2 + (moe_profesional/z)^2 + (moe_phd/z)^2),
                        moe_count_nobachelor = se_count_nobachelor * z,
                        se_total_pop = moe_total_pop/z,
                        se_prop_nobachelor = 1/est_total_pop * sqrt(se_count_nobachelor^2 - (count_nobachelor^2/est_total_pop^2) * se_total_pop^2),
                        se_per_nobachelor = se_prop_nobachelor * 100,
                        moe_per_nobachelor = se_per_nobachelor * z
)
tract <- tract %>% mutate(err_count_nobachelor = moe_count_nobachelor/count_nobachelor,
                        err_per_nobachelor = moe_per_nobachelor/per_nobachelor)
tract$err_count_nobachelor_grp <- factor(ifelse(tract$err_count_nobachelor <= 0.05, 
                                               "<=5%",
                                               ifelse(tract$err_count_nobachelor > 0.05 & tract$err_count_nobachelor <= 0.1, 
                                                      "5%-10%",
                                                      ">10%")), 
                                        levels = levels)
tract$err_per_nobachelor_grp <- factor(ifelse(tract$err_per_nobachelor <= 0.05, 
                                             "<=5%",
                                             ifelse(tract$err_per_nobachelor > 0.05 & tract$err_per_nobachelor <= 0.1, 
                                                    "5%-10%",
                                                    ">10%")), 
                                      levels = levels)
tract <- tract %>% select(-c(se_count_nobachelor, se_total_pop, se_prop_nobachelor, se_per_nobachelor))

# Export datasets ----------------------------------------------- 

# Final dataset for percentage by census tract, plus calculation components
education <- education %>% select(-starts_with("moe"), -prop_nobachelor)
write_csv(education, file = "./data/03-Education Attainment/03_EducationalAttainment.csv")

# Final dataset with tract information and MOE calculation
write_rds(tract, "./data/03-Education Attainment/tract_03_EducationalAttainment.rds")

