# libraries -----------------------------------------------
library(tidyverse)
library(readxl)
library(RMySQL)
library(sf)

# working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/15-Voter Turnout/")

### Get 2020 presidential election votes by precinct ---------------------
# From: https://www.sos.wa.gov/elections/research/data-and-statistics.aspx
election = read_excel("./2020_precinct_results.xlsx",
                      sheet = "Sheet1")

voters <- election %>% filter(RaceName == "Turnout",
                              Candidate %in% c("Registered Voters", "Ballots Cast"),
                              County %in% c("King","Kitsap","Snohomish","Pierce")) %>% 
  select(Candidate,PrecinctCode,County,Votes) %>% 
  pivot_wider( names_from = Candidate, values_from = Votes) %>% 
  rename(registered_voters = 'Registered Voters', ballots_cast = 'Ballots Cast' )

voters <- voters %>%
  mutate(Precinct = factor(PrecinctCode), 
         County = factor(County),
         CPrecinct = str_c(County, Precinct, sep = "_"))

rm(election)

# write_csv(voters, "./components/PSRC_voters.csv")

### Get population and number of residential units by parcel ---------------------
mydb = dbConnect(MySQL(), user='psrcurbansim', password='psrc_urbansim', dbname='2018_parcel_baseyear_rtp', host='aws-modelmysql')

#dbListTables(mydb)

# Total population for each parcel - used to calculate percent of people in precinct who voted
# Join households and buildings to get population per parcel
building_pop = dbGetQuery(mydb, "select building_id, sum(persons) as total_ppl 
                                from households
                                group by building_id")

parcel_data = dbGetQuery(mydb, "select * 
                                from buildings")

pop_by_parcel = parcel_data %>% 
  select(building_id, parcel_id) %>% 
  left_join(building_pop,  by = "building_id") %>% 
  group_by(parcel_id) %>% 
  summarise(pop_sum = sum(total_ppl, na.rm = TRUE))

# Total residential units per parcel - used to calculate weight of clipped precinct in tract
resunits_by_parcel = parcel_data %>%
  group_by(parcel_id) %>%
  summarise(resunit_sum = sum(residential_units, na.rm = TRUE))

# Get parcel centroids (X,Y coordinates)
parcel_centroid = dbGetQuery(mydb, "select * 
                                from parcels")

# Select only relevant columns and set projection to NAD83 Washington State Plane North (CRS 2285)
parcel_centroid <- parcel_centroid %>%
  select(parcel_id, census_tract_id, x_coord_sp, y_coord_sp) %>% 
  st_as_sf(coords = c("x_coord_sp", "y_coord_sp")) %>% 
  st_set_crs(2285) %>% 
  st_transform(2285)

# Attach parcel population/residential unit information to parcel centroids
parcel_centroid <- parcel_centroid %>%
  left_join(pop_by_parcel, by = "parcel_id") %>% 
  left_join(resunits_by_parcel, by = "parcel_id") %>%
  # remove NAs 
  filter(!is.na(resunit_sum))

# write_rds(parcel_centroid, "./components/parcel_centroid_pop_resunits.rds")

rm(mydb, building_pop, parcel_data, pop_by_parcel, resunits_by_parcel)

### Get precinct shapefile for intersections ---------------------
# From: https://www.sos.wa.gov/elections/research/precinct-shapefiles.aspx
precinct <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/15-Voter Turnout/statewide_precincts_2020general/Statewide_Precincts_2020General.shp")

# Retain only PSRC counties, select only relevant columns, set CRS 2285 projection
precinct <- precinct %>%
  filter(CountyName %in% c("King", "Kitsap", "Pierce", "Snohomish")) %>%
  mutate(County = factor(County),
         Precinct = factor(PrecName),
         PrecinctCode = factor(PrecCode),
         CPrecinct = str_c(County, Precinct, sep = "_")) %>%
  select(County, PrecinctCode, Precinct, CPrecinct, St_Code, geometry) %>%
  st_transform(2285)

# Attach election results to precinct spatial features
precinct_elec <- precinct %>%
  left_join(voters %>% select(PrecinctCode, registered_voters, ballots_cast), 
            by = c("St_Code" = "PrecinctCode")) %>% 
  mutate(ballots_cast = replace_na(ballots_cast, 0))

rm(precinct, voters)

### Get tract shapefile for intersections ---------------------
tract <- read_sf("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/gis/tract2010_nowater.shp")
# Set correct projection
tract <- tract %>%
  mutate(GEOID = factor(GEOID10)) %>%
  st_transform(2285)

### Spatial intersections

# PRECINCT/PARCEL: Intersect parcel centroids with precinct spatial feature - yields number of people and residential units in each precinct
precinct_parcel = st_join(precinct_elec, parcel_centroid)
# 25 precincts have no parcels or residential units, so the parcel columns are NA. Make these zero to indicate absence of population/residential units
precinct_parcel$pop_sum <- replace_na(precinct_parcel$pop_sum, 0)
precinct_parcel$resunit_sum <- replace_na(precinct_parcel$resunit_sum, 0)

# Sum up population and total residential by precinct
precinct_pop = precinct_parcel %>%
  # Do not need spatial features to calculate population/total res. units by parcel, and runs faster making geometry NULL
  st_set_geometry(NULL) %>% 
  group_by(St_Code) %>% 
  summarise(precinct_pop = sum(pop_sum),
            precinct_resunit = sum(resunit_sum))

rm(precinct_parcel)

# Attach population/resunit totals to precinct spatial features/election info
# Then can calculate "turnout", or number of votes out of all people living in the precinct (not just eligible voters)
# Note adding 1 to precinct_pop to avoid zero divide
precinct_pop <- precinct_elec %>% 
  left_join(precinct_pop, by = "St_Code" ) %>% 
  mutate(turnout = ballots_cast/(precinct_pop + 1),
         # 64 precincts have more ballots cast than population. Cap turnout at 1 (100%)
         turnout = ifelse(turnout > 1, 1, turnout)) 

rm(precinct_elec)
# write_rds(precinct_pop, "./components/precinct_turnout_cap100.rds")

# PRECINCT/TRACT: Intersect precinct and tract spatial features to yield clipped precincts
precinct_tract <- st_intersection(precinct_pop, tract) %>% 
  # Add ids to clipped precincts to group on later
  mutate(id = row_number())

rm(precinct_pop)

# PARCELS/CLIPPED PRECINCTS: Intersect parcel centroids with clipped precincts - yields number of people and residential units in each clipped precinct
precinct_tract_parcel = st_join(precinct_tract, parcel_centroid)

rm(parcel_centroid)

# 4402 clipped precincts have no parcels, so the parcel columns are NA. Change total population and residential units to 0
precinct_tract_parcel$pop_sum <- precinct_tract_parcel$pop_sum %>% replace_na(0)
precinct_tract_parcel$resunit_sum <- precinct_tract_parcel$resunit_sum %>% replace_na(0)

# Count all residential units in clipped precinct
precinct_tract_population = precinct_tract_parcel %>% 
  st_set_geometry(NULL) %>% 
  group_by(id) %>% 
  summarise(precinct_resunit_int = sum(resunit_sum)) 

rm(precinct_tract_parcel)

# Attach total residential units to clipped precinct spatial features and data
precinct_tract <- precinct_tract %>% 
  left_join(precinct_tract_population, by = "id" )

rm(precinct_tract_population)

# WEIGHTS: Calculate total residential units in each tract by grouping clipped precincts by tract
tractresunit <- precinct_tract %>% st_set_geometry(NULL) %>% group_by(GEOID) %>% summarise(tractresunit = sum(precinct_resunit_int))

# Attach total residential units by tract to clipped precints, calculate weights as (# resunits clipped precinct)/(# resunits in tract + 1)
# One tract has no residential units, so add 1 to tractresunit
precinct_tract <- precinct_tract %>%
  left_join(tractresunit, by = "GEOID") %>%
  mutate(weight = precinct_resunit_int/(tractresunit + 1),
         weighted_vote = weight * turnout)

# FINAL VALUES: Calculate turnout score by tract as weighted sum of turnout in clipped precincts
votes_tract <- precinct_tract %>%
  st_set_geometry(NULL) %>%
  group_by(GEOID) %>%
  summarise(votes = sum(weighted_vote, na.rm = TRUE) * 100)

votes_tract_viz <- precinct_tract %>%
  st_set_geometry(NULL) %>%
  group_by(GEOID) %>%
  summarise(votes = sum(weighted_vote, na.rm = TRUE),
            precincts = toString(St_Code),
            clipped_resunits = toString(precinct_resunit_int),
            turnouts = toString(round(turnout,2)) * 100)

rm(precinct_tract, tractresunit)

# Join to tract
tract <- tract %>%
  left_join(votes_tract, by = ("GEOID"))

# Export datasets ----------------------------------------------- 
write_csv(votes_tract, "./15_CivicEngagement.csv")
write_csv(votes_tract_viz, "./components/votes_tract_precinctinfo.csv")
write_rds(tract, "./tract_15_CivicEngagement.rds")
