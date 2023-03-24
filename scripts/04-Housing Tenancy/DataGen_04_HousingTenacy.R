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

# Households that are renters --------------------------------------

# Download data from ACS api
# Rename variables
total_housing      <- get_acs(geography = "tract", variables = "B25003_001", year = 2019, county = c("033","035","053","061"), state = "53")
total_housing      <- rename(total_housing, est_total_ho = "estimate" )
total_housing      <- rename(total_housing, moe_total_ho = "moe" )

total_rent <- get_acs(geography = "tract", variables = "B25003_003", year = 2019, county = c("033","035","053","061"), state = "53")
total_rent <- rename(total_rent, est_total_rent = "estimate" )
total_rent <- rename(total_rent, moe_total_rent = "moe" )


# Outer join of datasets
tenancy <- total_housing %>%
  select(-NAME, -variable) %>%
  full_join(total_rent, by = "GEOID") %>% 
  select(-NAME, -variable)

# Calculate percentage of renters by census tract
tenancy <- tenancy %>% 
        mutate(prop_rent = (est_total_rent / est_total_ho),
               per_rent = prop_rent * 100)

# Create spatial dataset with MOE calculation
tract <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/gis/tract2010_nowater.shp")
tract <- tract %>%
  mutate(GEOID = factor(GEOID10))

# Project tracts
tract <- st_transform(tract, 4269)

# Join datasets  ----------------------------------------------- 
tract <- tract %>%
  left_join(tenancy %>%
              mutate(GEOID = factor(GEOID), 
                     county = factor(str_sub(GEOID, 1, 5))), by = ("GEOID")) 

tract$per_rent <- round(tract$per_rent, digits = 2)

# margin of error for
# 1. count of ppl renting per tract
# 2. proportion of ppl in a tract who are renting
# ACS MOE eqns: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2020_ACS_Accuracy_Document_Worked_Examples.pdf

# SE(X/Y) -> eq 3 from ACS MOE eqns
levels <- c("<=5%", "5%-10%", ">10%")
z <- 1.645
tract <- tract %>% mutate(se_count_rent = moe_total_rent/z,
                        se_total_ho = moe_total_ho/z,
                        se_prop_rent = 1/est_total_ho * sqrt(se_count_rent^2 - (est_total_rent^2/est_total_ho^2) * se_total_ho^2),
                        se_per_rent = se_prop_rent * 100,
                        moe_per_rent = se_per_rent * z
)
tract <- tract %>% mutate(err_per_rent = moe_per_rent/per_rent)
tract$err_per_rent_grp <- factor(ifelse(tract$err_per_rent <= 0.05, 
                                       "<=5%",
                                       ifelse(tract$err_per_rent > 0.05 & tract$err_per_rent <= 0.1, 
                                              "5%-10%",
                                              ">10%")), 
                                levels = levels)
tract <- tract %>% select(-c(se_count_rent, se_total_ho, se_prop_rent, se_per_rent))

# Export datasets ----------------------------------------------- 

# Final dataset for percentage by census tract, plus calculation components
tenancy <- tenancy %>% select(-starts_with("moe"), -prop_rent)
write_csv(tenancy, file = "./data/04-Housing Tenancy/04_HousingTenancy.csv")

# Final dataset with tract information and MOE calculation
write_rds(tract, "./data/04-Housing Tenancy/tract_04_HousingTenancy.rds")

