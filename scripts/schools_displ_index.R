# This script compares two datasets with schools in the PSRC region
library(sf)
library(tidyverse)
library(readxl)
library(tidycensus)
library(gdata)
#load the data

schools_2020 = read.csv('J:/Projects/Surveys/HHTravel/Survey2019/Data/schools/Updated_tables/upd_schools.csv')


geodatabase_server <- "AWS-PROD-SQL\\Sockeye"
geodatabase_name <- "ElmerGeo"
gdb_nm <- paste0("MSSQL:server=",geodatabase_server,";database=",geodatabase_name,";trusted_connection=yes")
tbl_nm <- "dbo.k12_schools"

gis_schools_geo <- st_read(gdb_nm, tbl_nm)

gis_schools = st_set_geometry(gis_schools_geo, NULL)

#types of school in 2018 data
gis_schools %>%  group_by(type) %>% tally() %>%  arrange(desc(n))

#types of school in 2020 data
schools_2020 %>% group_by(category) %>% tally() %>%  arrange(desc(n))

#delete daycare (D), universities (U), O and I from 2020 schools
schools_2020 = schools_2020 %>% 
  filter(!category %in% c("D","U","I","0"))


#name_gis_schools = list(gis_schools$school)
#name_schools_2020 = list(schools_2020$sname)

name_gis_schools = gis_schools$school
name_schools_2020 = schools_2020$sname
sum(name_gis_schools %in% name_schools_2020)
sum(name_schools_2020 %in% name_gis_schools)

#There are a lot of differences between GIS school data and 020 urbansim data.
#For this analysis we decided to use public k12 schools only
# the k12 public data is accessible on the Wa state public data portal
#visual comparison of PSRC internal school data and OFM data showed that there are some discrepancies
# the following code explores the differences between two datasets 

geodatabase_server <- "AWS-PROD-SQL\\Sockeye"
geodatabase_name <- "ElmerGeo"
gdb_nm <- paste0("MSSQL:server=",geodatabase_server,";database=",geodatabase_name,";trusted_connection=yes")
tbl_nm <- "dbo.schools_k_12"

psrc_schools_geo <- st_read(gdb_nm, tbl_nm)
psrc_schools_geo = psrc_schools_geo %>% st_transform(crs=4326)

psrc_schools = st_set_geometry(psrc_schools_geo, NULL)

ofm_k12_schools_geo = read_sf(dsn = "C:/Users/pbutrina/Downloads/Washington_State_Public_Schools/Washington_State_Public_Schools.shp", layer = "Washington_State_Public_Schools")
ofm_k12_schools = st_set_geometry(ofm_k12_schools_geo, NULL)

ofm_k12_schools_psrc = ofm_k12_schools %>% filter( County %in% c("King", "Pierce", "Snohomish", "Kitsap"))


name_gis_schools = psrc_schools$school
name_schools_ofm = ofm_k12_schools_psrc$SchoolName
sum(name_gis_schools %in% name_schools_ofm)
sum(name_schools_ofm %in% name_gis_schools)


