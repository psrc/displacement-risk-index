# Generate indicator dataset ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages("tidyverse")
# install.packages("tidycensus")
library(tidyverse)
library(tidycensus)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# ACS key to access the survey data. Key can be obtained from: http://api.census.gov/data/key_signup.html 
# Run once to add CENSUS API key to .Renviron file
# census_api_key("16995506559e358a55d32e63541106a22b34acd7",install = TRUE)
readRenviron("~/.Renviron")

# Name of ACS databases
v15 <- load_variables(2019, "acs5", cache = TRUE)

# Race other than non-Hispanic White --------------------------------------

# Download data from ACS api
# Rename variables
total_race      <- get_acs(geography = "tract", variables = "B03002_001E", year = 2019, county = c("033","035","053","061"), state = "53")
total_race      <- rename(total_race, est_total_race = "estimate" )
total_race      <- rename(total_race, moe_total_race = "moe" )

nhispanic_white <- get_acs(geography = "tract", variables = "B03002_003E", year = 2019, county = c("033","035","053","061"), state = "53")
nhispanic_white <- rename(nhispanic_white, est_nhispanic_white = "estimate" )
nhispanic_white <- rename(nhispanic_white, moe_nhispanic_white = "moe" )

hispanic_white  <- get_acs(geography = "tract", variables = "B03002_013E", year = 2019, county = c("033","035","053","061"), state = "53")
hispanic_white  <- rename(hispanic_white, est_hispanic_white = "estimate" )
hispanic_white  <- rename(hispanic_white, moe_hispanic_white = "moe" )

total_nhispanic <- get_acs(geography = "tract", variables = "B03002_002E", year = 2019, county = c("033","035","053","061"), state = "53")
total_nhispanic <- rename(total_nhispanic, est_total_nhispanic = "estimate" )
total_nhispanic <- rename(total_nhispanic, moe_total_nhispanic = "moe" )

total_hispanic  <- get_acs(geography = "tract", variables = "B03002_012E", year = 2019, county = c("033","035","053","061"), state = "53")
total_hispanic  <- rename(total_hispanic, est_total_hispanic = "estimate" )
total_hispanic  <- rename(total_hispanic, moe_total_hispanic = "moe" )

# Outer join of datasets
race <- total_race %>%
  select(-NAME, -variable) %>%
  full_join(nhispanic_white, by = "GEOID") %>% 
  select(-NAME, -variable) %>%
  full_join(hispanic_white, by = "GEOID") %>%
  select(-NAME, -variable) %>%
  full_join(total_nhispanic, by = "GEOID") %>%
  select(-NAME, -variable) %>%
  full_join(total_hispanic, by = "GEOID") %>%
  select(-NAME, -variable)

# Calculate percentage of non-Hispanic White by census tract
race <- race %>% 
  mutate(prop_notwhite = (est_total_hispanic + est_total_nhispanic - est_nhispanic_white) / est_total_race,
         per_notwhite = prop_notwhite * 100)

# Create spatial dataset with MOE calculation
tract <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/gis/tract2010_nowater.shp")

tract <- tract %>%
  mutate(GEOID = factor(GEOID10))
# project tracts
tract <- st_transform(tract, 4269)

# join databases  ----------------------------------------------- 
tract <- tract %>%
  left_join(race %>%
              mutate(GEOID = factor(GEOID), 
                     county = factor(str_sub(GEOID, 1, 5))), by = ("GEOID")) 

tract$per_notwhite <- round(tract$per_notwhite, digits = 2)

# margin of error for
# 1. count of POC per tract
# 2. proportion of POC by tract
# ACS MOE eqns: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2020_ACS_Accuracy_Document_Worked_Examples.pdf

# count of POC
# = hw+(h-hw)+(nh-nhw)
# = (est_total_hispanic + est_total_nhispanic - est_nhispanic_white)
# = est_total_race - est_nhispanic_white

# SE(X +/- Y) -> eq 1 from ACS MOE eqns
# SE(X/Y) -> eq 3 from ACS MOE eqns
levels <- c("<=5%", "5%-10%", ">10%")
z <- 1.645
tract <- tract %>% mutate(count_notwhite = est_total_race - est_nhispanic_white,
                          se_count_notwhite = sqrt((moe_total_race/z)^2 + 
                                                   (moe_nhispanic_white/z)^2),
                          moe_count_notwhite = se_count_notwhite * z,
                          se_total_race = moe_total_race/z,
                          se_prop_notwhite = 1/est_total_race * sqrt(se_count_notwhite^2 - (count_notwhite^2/est_total_race^2) * se_total_race^2),
                          se_per_notwhite = se_prop_notwhite * 100,
                          moe_per_notwhite = se_per_notwhite * z
)
tract <- tract %>% mutate(err_count_notwhite = moe_count_notwhite/count_notwhite,
                          err_per_notwhite = moe_per_notwhite/per_notwhite)
tract$err_count_notwhite_grp <- factor(ifelse(tract$err_count_notwhite <= 0.05, 
                                             "<=5%",
                                             ifelse(tract$err_count_notwhite > 0.05 & tract$err_count_notwhite <= 0.1, 
                                                    "5%-10%",
                                                    ">10%")), 
                                      levels = levels)
tract$err_per_notwhite_grp <- factor(ifelse(tract$err_per_notwhite <= 0.05, 
                                           "<=5%",
                                           ifelse(tract$err_per_notwhite > 0.05 & tract$err_per_notwhite <= 0.1, 
                                                  "5%-10%",
                                                  ">10%")), 
                                    levels = levels)
tract <- tract %>% select(-c(se_count_notwhite, se_total_race, se_prop_notwhite, se_per_notwhite))

# Export datasets ----------------------------------------------- 

# Final dataset for percentage by census tract, plus calculation components
race <- race %>% select(-starts_with("moe"), -prop_notwhite)
write_csv(race, file = "./data/01-People of Color/01_PeopleOfColor.csv")

# Final dataset with tract information and MOE calculation
write_rds(tract, "./data/01-People of Color/tract_01_PeopleOfColor.rds")
