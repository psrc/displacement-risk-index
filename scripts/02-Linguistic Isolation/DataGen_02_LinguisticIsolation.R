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
# census_api_key("16995506559e358a55d32e63541106a22b34acd7", install = TRUE)
readRenviron("~/.Renviron")

# Name of ACS databases
v15 <- load_variables(2019, "acs5", cache = TRUE)

# Download data from ACS api
# Rename variables
# SPEAK LESS THAN VERY WELL TABLE C16001
names <- c("total_>5","spanish_less", "cajun_less", "west_less", "slavic_less",
           "euro_less", "kor_less", "mand_less", "viet_less", "tag_less", "asi_less", 
           "arab_less", "other_less")
number <-c("001", paste(ifelse(nchar(seq(5,38,by = 3)) == 1, "00", "0"), as.character(seq(5,38,by = 3)), sep = ""))
database <- "C16001"

for(i in 1:length(names)) {
  
  a <- paste("total", names[i], sep = "_")
  assign(a, get_acs(geography = "tract", 
                    variables = as.character(paste(database, number[i], sep = "_")), 
                    year = 2019, 
                    county = c("033","035","053","061"), 
                    state = "53"))
  b <- get(a)
  colnames(b) <- c("GEOID", 
                   "NAME", 
                   "variable", 
                   paste("est_total", names[i], sep = "_"), 
                   paste("moe_total", names[i], sep = "_"))
  b <- b[,c(-2, -3)]
  assign(a, b)
  rm(a,b)
  
}

# Outer join of datasets
language <- `total_total_>5` %>%
  full_join(`total_spanish_less`, by = "GEOID") %>%
  full_join(`total_cajun_less`, by = "GEOID") %>% 
  full_join(`total_west_less`, by = "GEOID") %>% 
  full_join(`total_slavic_less`, by = "GEOID") %>% 
  full_join(`total_euro_less`, by = "GEOID") %>% 
  full_join(`total_kor_less`, by = "GEOID") %>% 
  full_join(`total_mand_less`, by = "GEOID") %>% 
  full_join(`total_viet_less`, by = "GEOID") %>% 
  full_join(`total_tag_less`, by = "GEOID") %>% 
  full_join(`total_asi_less`, by = "GEOID") %>% 
  full_join(`total_arab_less`, by = "GEOID") %>% 
  full_join(`total_other_less`, by = "GEOID") 

# Calculate percentage of population 5+ that speak English less than very well by tract
language <- language %>% 
   mutate(prop_noenglish = (`est_total_spanish_less` + `est_total_cajun_less` +
                           `est_total_west_less` + `est_total_slavic_less` +
                           `est_total_euro_less` + `est_total_kor_less` +
                           `est_total_mand_less` + `est_total_viet_less` +
                           `est_total_tag_less` + `est_total_asi_less` +
                           `est_total_arab_less` + `est_total_other_less`) 
                           / `est_total_total_>5`,
          per_noenglish = prop_noenglish * 100)

# Create spatial dataset with MOE calculation
tract <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/gis/tract2010_nowater.shp")
tract <- tract %>%
  mutate(GEOID = factor(GEOID10))

# Project tracts
tract <- st_transform(tract, 4269)

# Join datasets  ----------------------------------------------- 
tract <- tract %>%
  left_join(language %>%
              mutate(GEOID = factor(GEOID), 
                     county = factor(str_sub(GEOID, 1, 5))), by = ("GEOID")) 

tract$per_noenglish <- round(tract$per_noenglish, digits = 2)

# margin of error for
# 1. count of linguistically isolated per tract
# 2. proportion of linguistically isolated by tract
# ACS MOE eqns: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2020_ACS_Accuracy_Document_Worked_Examples.pdf

# count of linguistically isolated
# = spn + frn + ger + rus + eur + kor + chn + vnm + tag + asn + arb + oth
# = est_total_spanish_less + est_total_cajun_less +
#   est_total_west_less + est_total_slavic_less +
#   est_total_euro_less + est_total_kor_less +
#   est_total_mand_less + est_total_viet_less +
#   est_total_tag_less + est_total_asi_less +
#   est_total_arab_less + est_total_other_less

# SE(X +/- Y) -> eq 1 from ACS MOE eqns
# SE(X/Y) -> eq 3 from ACS MOE eqns
levels <- c("<=5%", "5%-10%", ">10%")
z <- 1.645
tract <- tract %>% mutate(count_noenglish = est_total_spanish_less + est_total_cajun_less +
                          est_total_west_less + est_total_slavic_less +
                          est_total_euro_less + est_total_kor_less +
                          est_total_mand_less + est_total_viet_less +
                          est_total_tag_less + est_total_asi_less +
                          est_total_arab_less + est_total_other_less,
                        se_count_noenglish = sqrt(
                          (moe_total_spanish_less/z)^2 + (moe_total_cajun_less/z)^2 +
                            (moe_total_west_less/z)^2 + (moe_total_slavic_less/z)^2 +
                            (moe_total_euro_less/z)^2 + (moe_total_kor_less/z)^2 +
                            (moe_total_mand_less/z)^2 + (moe_total_viet_less/z)^2 +
                            (moe_total_tag_less/z)^2 + (moe_total_asi_less/z)^2 +
                            (moe_total_arab_less/z)^2 + (moe_total_other_less/z)^2),
                        moe_count_noenglish = se_count_noenglish * z,
                        `se_total_total_>5` = `moe_total_total_>5`/z,
                        se_prop_noenglish = 1/`est_total_total_>5` * sqrt(se_count_noenglish^2 - (count_noenglish^2/`est_total_total_>5`^2) * `se_total_total_>5`^2),
                        se_per_noenglish = se_prop_noenglish * 100,
                        moe_per_noenglish = se_per_noenglish * z
)
tract <- tract %>% mutate(err_count_noenglish = moe_count_noenglish/count_noenglish,
                        err_per_noenglish = moe_per_noenglish/per_noenglish)
tract$err_count_noenglish_grp <- factor(ifelse(tract$err_count_noenglish <= 0.05, 
                                              "<=5%",
                                              ifelse(tract$err_count_noenglish > 0.05 & tract$err_count_noenglish <= 0.1, 
                                                     "5%-10%",
                                                     ">10%")), 
                                       levels = levels)
tract$err_per_noenglish_grp <- factor(ifelse(tract$err_per_noenglish <= 0.05, 
                                            "<=5%",
                                            ifelse(tract$err_per_noenglish > 0.05 & tract$err_per_noenglish <= 0.1, 
                                                   "5%-10%",
                                                   ">10%")), 
                                     levels = levels)
tract <- tract %>% select(-c(se_count_noenglish, `se_total_total_>5`, se_prop_noenglish, se_per_noenglish))

# Export datasets ----------------------------------------------- 

# Final dataset for percentage by census tract, plus calculation components
language <- language %>% select(-starts_with("moe"), -prop_noenglish)
write_csv(language, file = "./data/02-Linguistic Isolation/02_LinguisticIsolation.csv")

# Final dataset with tract information and MOE calculation
write_rds(tract, "./data/02-Linguistic Isolation/tract_02_LinguisticIsolation.rds")

