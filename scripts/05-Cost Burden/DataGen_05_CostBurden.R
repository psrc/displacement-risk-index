# Generate indicator dataset ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(sf)
# install.packages(readxl)
library(tidyverse)
library(sf)
library(readxl)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Download Consolidated Planning/CHAS Data -------------------------
# 2014 - 2018 (most recent vintage for 2021 displacement risk update); Census Tracts; csv

fileUrl <- "https://www.huduser.gov/portal/datasets/cp/2014thru2018-140-csv.zip"
download.file(fileUrl, destfile = "./data/05-Cost Burdened Households/CHAS_2014_18.zip")

# unzip("./data/05-Cost Burdened Households/CHAS_2014_18.zip", exdir = "./data/05-Cost Burdened Households/")
# unlink("./data/05-Cost Burdened Households/CHAS_2014_18.zip")

# list.files("./data/05-Cost Burdened Households/")
# file.rename(from = "./data/05-Cost Burdened Households/140" , to = "./data/05-Cost Burdened Households/CHAS")

# Import dictionary and table with Income and Cost Burden -------------------------
dictionary_table7 <- read_excel("./data/05-Cost Burdened Households/CHAS/CHAS data dictionary 14-18.xlsx", 
                                sheet = "Table 7")
table7 <- read_csv("./data/05-Cost Burdened Households/CHAS/Table7.csv")

# Clean datasets -------------------------
dictionary_table7$`Household income` <- factor(dictionary_table7$`Household income`)
dictionary_table7$`Cost burden` <- factor(dictionary_table7$`Cost burden`)
dictionary_table7$`Column Name` <- factor(dictionary_table7$`Column Name`)

table7$st <- factor(table7$st)
table7$cnty <- factor(table7$cnty)
table7$tract <- factor(table7$tract)

table7 <- table7 %>%
  mutate(GEOID = str_sub(geoid, 8, 18))

# Get names of necessary columns -------------------------
levels(dictionary_table7$`Cost burden`)
levels(dictionary_table7$`Household income`)

# a. housing cost-burdened households, column names
dic_cost_burd_hh <- dictionary_table7 %>%
  filter(`Cost burden` == "housing cost burden is greater than 30% but less than or equal to 50%" | 
         `Cost burden` == "housing cost burden is greater than 50%", 
         `Household income` == "household income is less than or equal to 30% of HAMFI" | 
         `Household income` == "household income is greater than 30% but less than or equal to 50% of HAMFI" |
         `Household income` == "household income is greater than 50% but less than or equal to 80% of HAMFI")

lev_cost_burd_hh <- as.character(dic_cost_burd_hh$`Column Name`)

# b. severely housing cost-burdened households, column names
dic_sev_cost_burd_hh <- dictionary_table7 %>%
  filter(`Cost burden` == "housing cost burden is greater than 50%", 
         `Household income` == "household income is less than or equal to 30% of HAMFI" | 
         `Household income` == "household income is greater than 30% but less than or equal to 50% of HAMFI" |
         `Household income` == "household income is greater than 50% but less than or equal to 80% of HAMFI")

lev_sev_cost_burd_hh <- as.character(dic_sev_cost_burd_hh$`Column Name`)

# Column name for total number of households per tract
lev_total <- c("T7_est1")

# a. Housing Cost-burdened Households in Puget Sound -------------------------
counties <- factor(c("033", "035", "053", "061"))
ws <- factor(c("53"))

cost_burd_hh <- table7 %>%
  filter(st %in% ws, 
         cnty %in% counties) %>%
  select(GEOID,
         all_of(lev_cost_burd_hh), #one_of helps selecting columns with those names in the vector
         all_of(lev_total)) %>%
  mutate(hh_burden = Reduce("+", .[2:61]),
         per_burden = hh_burden / T7_est1 * 100)

# b. Severe Housing Cost-burdened Households in Puget Sound -------------------------
sev_cost_burd_hh <- table7 %>%
  filter(st %in% ws, 
         cnty %in% counties) %>%
  select(GEOID,
         one_of(lev_sev_cost_burd_hh),
         one_of(lev_total)) %>%
  mutate(hh_sev_burden = Reduce("+", .[2:31]),
         per_sev_burden = hh_sev_burden / T7_est1 * 100)

# Subset columns
cost_burd_hh <- cost_burd_hh[,c(1,62:64)]
sev_cost_burd_hh <- sev_cost_burd_hh[,c(1,32:34)]

colnames(cost_burd_hh)[2] <- "total"
colnames(sev_cost_burd_hh)[2] <- "total"

# Create spatial dataset
cb <- cost_burd_hh %>% 
  select(GEOID, hh_burden, per_burden)

s_cb <- sev_cost_burd_hh %>% 
  select(GEOID, hh_sev_burden, per_sev_burden)

data <- cb %>%
  left_join(s_cb, by = ("GEOID"))

data <- data %>%
  mutate(GEOID = factor(GEOID), 
         county = factor(str_sub(GEOID, 1, 5)))

tract <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/gis/tract2010_nowater.shp")
tract <- tract %>%
  mutate(GEOID = factor(GEOID10))

#   project tracts
tract <- st_transform(tract, 4269)

# Join datasets  ----------------------------------------------- 
tract <- tract %>%
  left_join(data, by = ("GEOID")) 

tract$per_burden <- round(tract$per_burden , digits = 2)
tract$per_sev_burden <- round(tract$per_sev_burden , digits = 2)

# Export datasets -----------------------------------------------

# Final dataset for cost-burdened percentage by census tract, plus calculation components
write_csv(cost_burd_hh, file = "./data/05-Cost Burdened Households/05_a_cost-burdenHousehold.csv")

# Final dataset for severely cost-burdened percentage by census tract, plus calculation components
write_csv(sev_cost_burd_hh, file = "./data/05-Cost Burdened Households/05_b_Severecost-burdenHousehold.csv")

# Final dataset with tract information 
write_rds(tract, "./data/05-Cost Burdened Households/tract_05_cost-burden.rds")
