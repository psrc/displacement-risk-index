### Descriptive and spatial analysis of the data ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages('tidyverse')
# install.packages("tidycensus")
# install.packages("sf")
# install.packages("leaflet") 
# install.packages("wesanderson") 
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(wesanderson)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2026")

# Current data  ----------------------------------------------- 
vote <- read.csv("./data/15-Voter Turnout/15_CivicEngagement.csv")

# 2016 data
vote_2016 <- read_excel("../displacement-risk-data.xlsx", sheet = 2, skip = 1)
vote_2016 <- vote_2016[,c(1,47)]
names(vote_2016) <- c("GEOID", "votes")

# 2020 data
vote_2020 <- read.csv("../Displacement Index 2021/data/15-Voter Turnout/15_CivicEngagement.csv")

# Calculate quantiles
temp = as.data.frame(quantile(vote$votes, probs = seq(0, 1, 0.2),na.rm = TRUE))
colnames(temp) <- "new"

# Variable distributions
vote %>% ggplot(aes(votes)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of voter turnout") +
  geom_vline(aes(xintercept = quantile(votes, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(votes, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(votes, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))


# Compare 2016, 2020, and 2024 distributions
mean_vote_2016 = mean(vote_2016$votes,na.rm = TRUE)
mean_vote_2020 = mean(vote_2020$votes,na.rm = TRUE)
mean_vote_2024 = mean(vote$votes,na.rm = TRUE)

# Bind data sets
vote_2016 <- vote_2016 %>% 
  as.data.frame() %>% 
  mutate(votes= as.numeric(votes)*100,
         year=as.factor(2016))

vote_2020$year = as.factor(2019)

vote$year = as.factor(2024)

vote_all <- rbind(vote_2016, vote_2020, vote)

vote_all %>% ggplot(aes(votes,fill = year))+
  geom_density(alpha=.2)

vote_all %>% ggplot(aes(votes, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_vote_2016),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_vote_2020),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_vote_2024),
             color="royalblue", linetype="dashed", linewidth=1)






# Density plots
## This plot is using the previous displacement risk data from the original product (2016 election year) and the 2021 update (2020 election year). The methodology calculating voter participation shifted for the 2026 update - creating geography splits between precincts and tracts

# 2016
old_2016 <- read_excel("Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx", sheet = 2, skip = 1)
old_2016 <- old_2016[,c(1,47)]
names(old_2016) <- c("GEOID", "votes")
old_2016$votes <- as.numeric(old_2016$votes)*100
old_2016$year <- "2016"

# 2020
old_2020 <- read_excel("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/FinalIndicators.xlsx", 
                  sheet = 1, skip = 1)
old_2020 <- old_2020[,c(1,27)]
names(old_2020) <- c("GEOID", "votes")
old_2020$votes <- as.numeric(old_2020$votes)
old_2020$year <- "2020"

# 2024
vote$year <- "2024"

all <- rbind(vote, old_2020, old_2016)

all %>% ggplot(aes(x = votes, fill = year)) + geom_density(alpha=.2) + 
  coord_cartesian(xlim = c(0, 100)) + 
  coord_cartesian(ylim = c(0, 0.035))

## This plot is using the 2012-2024 data generated with the geography splits between precincts and tracts. The 2024 voter participation data will be the same as above, but the methodology to calculate the voter participation values for the previous elections varies from the data sets visualized above 
all_election = read.csv("Y:/Equity Indicators/tracker-webpage-content/h-public-services/h02-voter-participation/raw-data/voter_participation.csv")

str(all_election)

# make consistent with previous combined data set
election_simp <- all_election %>% 
  select(planning_geog, pop18_turnout, election_year) %>% 
  rename(GEOID=planning_geog,
         votes=pop18_turnout,
         year=election_year) %>% 
  mutate(votes=round(votes*100, digits=2),
         year=as.character(year)) %>% 
  filter(year!="2012") #to more easily compare with previous density plot

election_simp %>% ggplot(aes(x = votes, fill = year)) + geom_density(alpha=.2) + 
  coord_cartesian(xlim = c(0, 100)) + 
  coord_cartesian(ylim = c(0, 0.035))



# Summary stats and map - comparing 2020 data ------------

# Previous 2020 data set (calculated for 2021 update, using Ava's parcel-based methodology)
# Troubleshooting
# # Precincts with more ballots cast than ppl in them (parcel-based)
# precinct_pop <- readRDS("RegionPrecinct_votes_pop_2020.rds")
# precinct_pop <- precinct_pop %>% mutate(over100 = factor(ifelse(turnout > 1, 1, 0),
#                                                          labels = c("Ballots cast <= pop",
#                                                                     "Ballots cast > pop")))
# precinct_pop_wgs84 <- st_transform(precinct_pop, 4326)
# 
# # Map of over100
# pal <- colorFactor(c("white", "red"), domain = precinct_pop_wgs84$over100)
# 
# leaflet(precinct_pop_wgs84) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = precinct_pop_wgs84,
#               stroke = T,
#               opacity = 1,
#               weight = 1,
#               fillColor = ~pal(precinct_pop_wgs84$over100),
#               fillOpacity = 0.7,
#               popup = paste(
#                 "Ballots cast: ", precinct_pop_wgs84$ballots_cast, "<br>",
#                 "Population (parcel-based): ", precinct_pop_wgs84$precinct_pop, "<br>",
#                 "Precinct: ", precinct_pop_wgs84$St_Code
#               )) %>%
#   addLegend(pal = pal, values = precinct_pop_wgs84$over100, opacity = 0.7, title = "Ballots vs. parcel-based population, Precincts",
#             position = "bottomright")


# Map of tracts with vote percentage > 100%
str(old_2020)
summary(old_2020$votes) #max is 86.04, so mapping is unnecessary

# join tract voter turn out data set to tract spatial file (choosing 2019 year because 2010 fields are not consistent)
tracts_2010 <- tracts("WA", county = c(033, 035, 053, 061), cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>%
  select(GEOID, TRACTCE, geometry)
tracts_2010$GEOID <- as.numeric(tracts_2010$GEOID)

tracts_old_method <- tracts_2010 %>% left_join(old_2020, by = "GEOID")
tracts_old_method <- tracts_old_method %>%
  mutate(over100 = factor(
    ifelse(votes > 100, 1, 0),
    levels = c(0, 1),  # match the output of ifelse
    labels = c("<=100% voted", ">100% voted")
  ))
pal <- colorFactor(c("white", "red"), domain = tracts_old_method$over100)

leaflet(tracts_old_method) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = tracts_old_method,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(tracts_old_method$over100),
              fillOpacity = 0.7,
              popup = paste(
                "Vote percentage: ", round(tracts_old_method$votes, 2)#, "<br>",
                # "Intersecting precincts: ", tracts$precincts, "<br>",
                # "Component resunits: ", tracts$clipped_resunits, "<br>",
                # "Component turnouts: ", tracts$turnouts
              )) %>% 
  addLegend(pal = pal, values = tracts_old_method$over100, opacity = 0.7, title = "Turnout > 100%, Tracts",
            position = "bottomright")

# # Precinct turnout continuous
# # bins <- c(0, quantile(precinct_pop$turnout100, probs = seq(0, 1, 0.1), na.rm = TRUE))
# bins <- quantile(precinct_pop_wgs84$turnout, seq(0,1,0.1), na.rm = TRUE)
# # bins <- c(0, 11, 22, 33, 44, 55, 66, 77, 88, 100)
# pal <- colorBin("YlOrRd", domain = precinct_pop_wgs84$turnout, bins = bins)
# 
# leaflet(precinct_pop_wgs84)%>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data=precinct_pop_wgs84,
#               stroke = T,
#               opacity = 1,
#               weight = 1,
#               fillColor = ~pal(precinct_pop_wgs84$turnout),
#               fillOpacity = 0.7,
#               popup = paste(
#                 "Ballots cast: ", precinct_pop_wgs84$ballots_cast, "<br>",
#                 "Population (parcel-based): ", precinct_pop_wgs84$precinct_pop, "<br>",
#                 "Turnout: ", round(precinct_pop_wgs84$turnout, 2), "<br>",
#                 "Precinct: ", precinct_pop_wgs84$St_Code
#               )) %>% 
#   addLegend(pal = pal, values = precinct_pop_wgs84$turnout, opacity = 0.7, title = "Turnout (ballots/pop), Precincts",
#             position = "bottomright")

# Tract turnout continuous
# bins <- quantile(tracts_old_method$votes, probs = seq(0, 1, 0.1), na.rm = TRUE)
# bins <- c(0, 45, 50, 55, 60, 75, 70, 80, 90, 100, 102)
bins <- c(0, 40, 50, 60, 70, 80, 90, 100, 102)
pal <- colorBin("YlOrRd", domain = tracts_old_method$votes, bins = bins)

leaflet(tracts_old_method) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = tracts_old_method,
              stroke = T,
              opacity = 1,
              weight = 0.4,
              color = "gray",
              fillColor = ~pal(tracts_old_method$votes),
              fillOpacity = 0.7,
              popup = paste(
                "Vote percentage: ", round(tracts_old_method$votes, 2)#, "<br>",
                # "Intersecting precincts: ", tracts$precincts, "<br>",
                # "Component resunits: ", tracts$clipped_resunits, "<br>",
                # "Component turnouts: ", tracts$turnouts
              )) %>% 
  addLegend(pal = pal, values = tracts_old_method$votes, opacity = 0.7, title = "Turnout, Tracts",
            position = "bottomright")


# Current 2020 data set (calculated for 2026 update/equity tracker, using precinct-tract geography splits)
str(election_simp)

election_simp_2020 <- election_simp %>% 
  filter(year=="2020")

summary(election_simp_2020$votes)

# join tract voter turn out data set to tract spatial file
tracts_2020 <- tracts("WA", county = c(033, 035, 053, 061), cb = TRUE, year = 2020) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>%
  select(GEOID, TRACTCE, geometry)
tracts_2020$GEOID <- as.numeric(tracts_2020$GEOID)

tracts_new_method <- tracts_2020 %>% left_join(election_simp_2020, by = "GEOID")

# Tract turnout continuous
# bins <- quantile(tracts_new_method$votes, probs = seq(0, 1, 0.1), na.rm = TRUE)
bins <- c(0, 40, 50, 60, 70, 80, 90, 100, 102)
pal <- colorBin("YlOrRd", domain = tracts_new_method$votes, bins = bins)

leaflet(tracts) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = tracts_new_method,
              stroke = T,
              opacity = 1,
              weight = 0.4,
              color = "gray",
              fillColor = ~pal(tracts_new_method$votes),
              fillOpacity = 0.7,
              popup = paste(
                "Vote percentage: ", round(tracts_new_method$votes, 2)#, "<br>",
                # "Intersecting precincts: ", tracts$precincts, "<br>",
                # "Component resunits: ", tracts_new_method$clipped_resunits, "<br>",
                # "Component turnouts: ", tracts_new_method$turnouts
              )) %>% 
  addLegend(pal = pal, values = tracts_new_method$votes, opacity = 0.7, title = "Turnout, Tracts",
            position = "bottomright")
