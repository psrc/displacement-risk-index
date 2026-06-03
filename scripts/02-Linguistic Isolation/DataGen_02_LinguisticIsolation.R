# Generate indicator data set

# Libraries --------------------------------------
# install.packages(tidyverse)
# install.packages(tidycensus)
library(tidyverse)
library(tidycensus)
library(psrccensus)
library(sf)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2026")

# The 2021 update (2019 ACS data) accessed the survey data using the API, which required a key (http://api.census.gov/data/key_signup.html), but since 2021, the psrccensus library was developed. The 2026 (2024 ACS data) update will access the survey data using psrccensus (https://psrc.github.io/psrccensus/articles/psrccensus.html).
acs_data_year <- "2024"

# Accessing ACS data --------------------------------------
# 5y estimates, by tract
base_acs_data <- get_acs_recs(geography ='tract', 
                              table.names = 'C16001', #subject table code
                              years = c(as.numeric(acs_data_year)),
                              acs.type = 'acs5')

## Transforming data ----
# Define variables of interest (and order based on 2021 update data)
variables_list <- c("C16001_001", #Estimate!!Total
                    "C16001_005", #...!!Spanish:!!Speak English less than "very well"
                    "C16001_008", #...!!French, Haitian, or Cajun:!!Speak English less than "very well"
                    "C16001_011", #...!!German or other West Germanic languages:!!Speak English less than "very well"
                    "C16001_014", #...!!Russian, Polish, or other Slavic languages:!!Speak English less than "very well"
                    "C16001_017", #...!!Other Indo-European languages:!!Speak English less than "very well"
                    "C16001_020", #...!!Korean:!!Speak English less than "very well"
                    "C16001_023", #...!!Chinese (incl. Mandarin, Cantonese):!!Speak English less than "very well"
                    "C16001_026", #...!!Vietnamese:!!Speak English less than "very well"
                    "C16001_029", #...!!Tagalog (incl. Filipino):!!Speak English less than "very well"
                    "C16001_032", #...!!Other Asian and Pacific Island languages:!!Speak English less than "very well"
                    "C16001_035", #...!!Arabic:!!Speak English less than "very well"
                    "C16001_038") #...!!Other and unspecified languages:!!Speak English less than "very well"


acs_data <- base_acs_data %>%
  filter(variable %in% variables_list) %>% 
  mutate(lang_cat = case_when(variable=="C16001_001"~"total_>5",
                              variable=="C16001_005"~"total_spanish_less",
                              variable=="C16001_008"~"total_cajun_less",
                              variable=="C16001_011"~"total_west_less",
                              variable=="C16001_014"~"total_slavic_less",
                              variable=="C16001_017"~"total_euro_less",
                              variable=="C16001_020"~"total_kor_less",
                              variable=="C16001_023"~"total_mand_less",
                              variable=="C16001_026"~"total_viet_less",
                              variable=="C16001_029"~"total_tag_less",
                              variable=="C16001_032"~"total_asi_less",
                              variable=="C16001_035"~"total_arab_less",
                              variable=="C16001_038"~"total_other_less")) %>% 
  dplyr::select(GEOID, lang_cat, estimate, moe) %>% # simplify data set
  rename(est=estimate) %>% # to match 2021 data
  pivot_wider(names_from = lang_cat, 
              values_from = c(est, moe)) # pivot data to facilitate calculations


## Calculate percentage of population 5+ that speak English less than very well by tract ----
language_calc <- acs_data %>% 
  rowwise() %>% 
  mutate(sum_noenglish = sum(c_across(matches("^est.*less$")), na.rm = TRUE), # Sum columns starting with "est" and ending with "less"
         prop_noenglish = sum_noenglish/`est_total_>5`,
         per_noenglish = prop_noenglish * 100)


# Creating spatial data set --------------------------------------

## Calculating reliability measures for ACS estimates ----
# old method (2021 update): manual calculations using SE and z-score of 1.645
# new method (2026 update): using moe_sum() and moe_prop() from the tidycensus library (https://psrc.github.io/psrccensus/articles/calculate-reliability-moe-transformed-acs.html)

# use the moe_sum function from tidycensus: moe_sum(moe, estimate = NULL, na.rm = FALSE)
# use the moe_prop function from tidycensus: moe_prop(num, denom, moe_num, moe_denom)
language_moe_values <- language_calc %>%
  mutate(
    est_total_noenglish = rowSums(across(matches("^est.*less$")), na.rm = TRUE), #same as sum_noenglish
    moe_total_noenglish = moe_sum(estimate=across(matches("^est.*less$")),
                                  moe=across(matches("^moe.*less$")))) %>% 
  mutate(
    moe_prop_noenglish = moe_prop(num=est_total_noenglish,
                                  denom=`est_total_>5`, 
                                  moe_num=moe_total_noenglish, 
                                  moe_denom=`moe_total_>5`)) %>%
  reliability_calcs(estimate='prop_noenglish', 
                    moe='moe_prop_noenglish')


# Connecting to ElmerGeo for census geographies through Portal, instead of saving spatial file to the project folder
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract, language_moe_values,
               by.x="geoid20",
               by.y="GEOID",
               all.x=TRUE)

tract$per_noenglish <- round(tract$per_noenglish, digits = 2)


# Exporting data sets ----------------------------------------------- 

## Final data set for percentage by census tract, plus calculation components ----
language <- language_calc %>% select(GEOID, starts_with("est"), per_noenglish)
write_csv(language, file = "./data/02-Linguistic Isolation/02_LinguisticIsolation.csv")

## Final data set with tract information and MOE calculations ----
write_rds(tract, "./data/02-Linguistic Isolation/tract_02_LinguisticIsolation.rds")

# To compare with 2021 update:
# rds_2021 <- readRDS("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/02-Linguistic Isolation/tract_02_LinguisticIsolation.rds")