# Generate indicator dataset ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(sf)
library(tidyverse)
library(sf)

setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

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
