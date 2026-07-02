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

# The 2021 update accessed Consolidated Planning/CHAS Data from HUD - 2014-2018 was the most recent vintage and it was available for download as a csv in a zipped file at the tract level (https://www.huduser.gov/portal/datasets/cp/2014thru2018-140-csv.zip). For the 2026 update, the most recent vintage available is 2018-2022 (updated 2025/12/23). Data can be downloaded manually through the HUD Portal (https://www.huduser.gov/portal/datasets/cp.html#data_2006-2022), through a URL file path (fileUrl <- "https://www.huduser.gov/portal/datasets/cp/2018thru2022-140-csv.zip"), or through Elmer - more information available on the Data Wiki (http://aws-linux/mediawiki/index.php/Elmer_CHAS_Data). 

# Accessing the Consolidated Planning/CHAS Data --------------------------------------
# 2018 - 2022 (most recent vintage for 2026 displacement risk update); Census Tracts
chas_table7_22_tract <- "select * 
  from chas.get_table_7(2022, 'tract')"

chas_table7 <- get_query(sql=chas_table7_22_tract, db_name = "Elmer")

str(chas_table7)

# data dictionary is not included in zip folder in the 2018-2022 vintage (it was included in the 2014-18 data) - it can be manually downloaded here: https://www.huduser.gov/portal/datasets/cp/CHAS/data_doc_chas.html
# datadictioary_Url <- "https://www.huduser.gov/portal/datasets/cp/CHAS-data-dictionary-18-22.xlsx"
# download.file(datadictioary_Url, destfile = file_name, mode="wb")
file_name <- "./data/05-Cost Burdened Households/CHAS/CHAS-data-dictionary-18-22.xlsx"
sheet_names <- excel_sheets(file_name)

# Read sheet names from data dictionary file - one of the sheet names is 'Table List'
table_list <- read_excel(file_name,
                         sheet = "Table List")

# Interested in cost burden, which is included in Table 7 =  "Tenure (2) by Household Income (5) by Household Type (5) by Housing Cost Burden (3)"
dictionary_table7 <- read_excel(file_name,
                                sheet = "Table 7")

# explore data dictionary
dictionary_table7$`Household income` <- factor(dictionary_table7$`Household income`)
dictionary_table7$`Cost burden` <- factor(dictionary_table7$`Cost burden`)
dictionary_table7$`Column Name` <- factor(dictionary_table7$`Column Name`)

# Get names of necessary columns 
levels(dictionary_table7$`Cost burden`)
levels(dictionary_table7$`Household income`)


## Transforming data ----
# a. housing cost-burdened households
dic_cost_burd_hh <- dictionary_table7 %>%
  filter(`Cost burden` == "housing cost burden is greater than 30% but less than or equal to 50%" | 
           `Cost burden` == "housing cost burden is greater than 50%", 
         `Household income` == "household income is less than or equal to 30% of HAMFI" | 
           `Household income` == "household income is greater than 30% but less than or equal to 50% of HAMFI" |
           `Household income` == "household income is greater than 50% but less than or equal to 80% of HAMFI") 

lev_cost_burd_hh <- as.character(dic_cost_burd_hh$`Column Name`)

# b. severely housing cost-burdened households
dic_sev_cost_burd_hh <- dictionary_table7 %>%
  filter(`Cost burden` == "housing cost burden is greater than 50%", 
         `Household income` == "household income is less than or equal to 30% of HAMFI" | 
           `Household income` == "household income is greater than 30% but less than or equal to 50% of HAMFI" |
           `Household income` == "household income is greater than 50% but less than or equal to 80% of HAMFI")

lev_sev_cost_burd_hh <- as.character(dic_sev_cost_burd_hh$`Column Name`)

# total number of households per tract
total_hh_tract <- chas_table7 %>% 
  filter(variable_name =="T7_est1") %>% # Tenure=="Total: Occupied housing units"
  select(tract_geoid, estimate, moe) %>% 
  rename(total_hh_estimate=estimate,
         total_hh_moe=moe)


## Calculating proportion and percent of cost burden by census tract ----
# a. Housing Cost-burdened Households -------------------------
# a1. calculate total housing cost-burdened hh and MOE per tract
cost_burdened_hh <- chas_table7 %>%
  filter(variable_name %in% lev_cost_burd_hh) %>%
  group_by(tract_geoid, chas_year) %>%
  summarise(
    total_cost_burdened_estimate = sum(estimate),
    total_cost_burdened_moe = moe_sum(moe, estimate),  # sum MOE correctly
    .groups = "drop"
  )


# a2. join and calculate proportion cost-burdened hh and MOE per tract 
prop_cost_burdened_hh <- cost_burdened_hh %>% 
  left_join(total_hh_tract, by = "tract_geoid") %>% 
  mutate(prop_cost_burdened = ((total_cost_burdened_estimate/total_hh_estimate)*100),
         moe_prop_cost_burdened = moe_prop(total_cost_burdened_estimate, 
                                           total_cost_burdened_moe,
                                           total_hh_estimate, 
                                           total_hh_moe)) %>% 
  reliability_calcs(estimate='prop_cost_burdened', 
                    moe='moe_prop_cost_burdened') %>% 
  rename(reliability_cost_burden=reliability) %>% 
  select(-c(se, cv)) %>% 
  rename(GEOID=tract_geoid,
         hh_burden=total_cost_burdened_estimate,
         per_burden=prop_cost_burdened) # make fields consistent with 2021 data 


# b. Housing Cost-burdened Households -------------------------
# b1. calculate total housing cost-burdened hh and MOE per tract
sev_cost_burdened_hh <- chas_table7 %>%
  filter(variable_name %in% lev_sev_cost_burd_hh) %>%
  group_by(tract_geoid, chas_year) %>%
  summarise(
    total_sev_cost_burdened_estimate = sum(estimate),
    total_sev_cost_burdened_moe = moe_sum(moe, estimate),  # sum MOE correctly
    .groups = "drop"
  )

# b2. join and calculate proportion cost-burdened hh and MOE per tract 
prop_sev_cost_burdened_hh <- sev_cost_burdened_hh %>% 
  left_join(total_hh_tract, by = "tract_geoid") %>% 
  mutate(prop_sev_cost_burdened = ((total_sev_cost_burdened_estimate/total_hh_estimate)*100),
         moe_prop_sev_cost_burdened = moe_prop(total_sev_cost_burdened_estimate,
                                               total_sev_cost_burdened_moe,
                                               total_hh_estimate, 
                                               total_hh_moe)) %>% 
  reliability_calcs(estimate='prop_sev_cost_burdened', 
                    moe='moe_prop_sev_cost_burdened') %>% 
  rename(reliability_sev_cost_burden=reliability) %>% 
  select(-c(se, cv)) %>% 
  rename(GEOID=tract_geoid,
         hh_sev_burden=total_sev_cost_burdened_estimate,
         per_sev_burden=prop_sev_cost_burdened) # make fields consistent with 2021 data 


# Combine cost-burdened and severely cost-burdened data sets
all_cost_burden <- merge(prop_cost_burdened_hh, prop_sev_cost_burdened_hh,
                         by = c("GEOID", "chas_year", 
                                "total_hh_estimate", "total_hh_moe"))


# Creating spatial data set --------------------------------------
# Connecting to ElmerGeo for census geographies through Portal, instead of saving spatial file to the project folder
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract, all_cost_burden,
               by.x="geoid20",
               by.y="GEOID",
               all.x=TRUE)

tract <- tract %>% 
  rename(GEOID=geoid20) %>% 
  mutate(per_burden=round(per_burden, digits = 2),
         per_sev_burden=round(per_sev_burden, digits = 2))


# Exporting data sets ----------------------------------------------- 

## Final data set for percentage by census tract, plus calculation components ----
prop_cost_burdened_hh_csv <- prop_cost_burdened_hh %>% 
  select(GEOID, total_hh_estimate, hh_burden, per_burden) %>% 
  rename(total=total_hh_estimate)

prop_sev_cost_burdened_hh_csv <- prop_sev_cost_burdened_hh %>% 
  select(GEOID, total_hh_estimate, hh_sev_burden, per_sev_burden) %>% 
  rename(total=total_hh_estimate)

write_csv(prop_cost_burdened_hh_csv, 
          file = "./data/05-Cost Burdened Households/05_a_CostBurdenHousehold.csv")
write_csv(prop_sev_cost_burdened_hh_csv, 
          file = "./data/05-Cost Burdened Households/05_b_SevereCostBurdenHousehold.csv")

## Final data set with tract information and MOE calculations ----
write_rds(tract, "./data/05-Cost Burdened Households/tract_05_CostBurden.rds")

# To compare with 2021 update:
# rds_2021 <- readRDS("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/05-Cost Burdened Households/tract_05_CostBurden.rds")


# if trying to download from online portal ----

# fileUrl <- "https://www.huduser.gov/portal/datasets/cp/2018thru2022-140-csv.zip"
# 
# # The file size is 215.7 MB (which is considered a large file - 50MB or more) - this is because its data for all census tracts in the country. As a result, we need to increase timeout to 1000 seconds (700 wasn't long enough). The alternative would be to manually download the zip file from the HUD portal.
# # options(timeout = max(1000, getOption("timeout")))
# # download.file(fileUrl, destfile = "./data/05-Cost Burdened Households/CHAS_2018_22.zip") # can be commented out 
# 
# # list the contents of the zip file without extracting
# # unzip("./data/05-Cost Burdened Households/CHAS/CHAS_2018_22.zip", list = TRUE)
#
# # extract all of the tables in the zip folder, but unzip() takes a long time, so could focus on Table 7
# # unzip("./data/05-Cost Burdened Households/CHAS/CHAS_2018_22.zip", 
# #       exdir = "./data/05-Cost Burdened Households/CHAS")
# unzip(zipfile = "./data/05-Cost Burdened Households/CHAS/CHAS_2018_22.zip", 
#       files = "140/Table7.csv", 
#       exdir = "./data/05-Cost Burdened Households/CHAS")
# 
# # Organize folder and files
# list.files("./data/05-Cost Burdened Households/CHAS")
# # file.rename(from = "./data/05-Cost Burdened Households/CHAS/140", 
# #             to = "./data/05-Cost Burdened Households/CHAS/CHAS_2018_22")
# 
# # Read table with cost-burden data
# table7 <- read_csv("./data/05-Cost Burdened Households/CHAS/CHAS_2018_22/Table7.csv")
