### Descriptive and spatial analysis of the data ------------------------------------------------

# Libraries
# install.packages('tidyverse')
# install.packages("sf")
# install.packages("leaflet") 
# install.packages("tigris")
# install.packages("readxl")
library(tidyverse)
library(leaflet)
library(tigris)
library(sf)
library(readxl)

setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/15-Voter Turnout/components/")

vote <- read.csv("../15_CivicEngagement.csv")
vote <- filter(vote, GEOID != 53061990002)
# vote <- vote %>% select(GEOID, prop = per_voted) %>% mutate(year = "2016")

# Calculate quantiles
temp = as.data.frame(quantile(vote$votes, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$ind_rent_0_rooms, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
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


# old <- read_excel("Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx", sheet = 2, skip = 1)
# old <- old %>% select(GEOID, prop = `%pop. >18yr who voted`) %>% 
#   mutate(prop = as.numeric(prop), 
#          year = "2020")
# 
# all <- rbind(vote, old)
# 
# all %>% ggplot(aes(prop, fill = year)) +
#   geom_density(alpha=.2)


# makeCatMap <- function(dataset, title, colnumber, colors) {
#   data <- left_join(tracts, dataset, by = "GEOID")
#   data[[colnumber]] <- as.factor(data[[colnumber]])
#   
#   pal <- colorFactor(colors, domain = data[[3]])
#   
#   m <- leaflet(data) %>%
#     addProviderTiles(providers$CartoDB.Positron) %>%
#     addPolygons(data = data,
#                 stroke = T,
#                 opacity = 1,
#                 weight = 1,
#                 fillColor = ~pal(data[[3]]),
#                 fillOpacity = 0.7,
#                 popup = paste(
#                   indicator,", ", type, ": ", data[[3]], "%<br>",
#                   "tract", data$TRACTCE
#                 )) %>% 
#     addLegend(pal = pal, values = data[[3]], opacity = 0.7, title = paste(indicator, ",", type),
#               position = "bottomright")
#   saveWidget(as_widget(m), file.path(getwd(), paste(indicator, ".html", sep = "")))
# }

# Troubleshooting
# Precincts with more ballots cast than ppl in them (parcel-based)
precinct_pop <- readRDS("RegionPrecinct_votes_pop_2020.rds")
precinct_pop <- precinct_pop %>% mutate(over100 = factor(ifelse(turnout > 1, 1, 0),
                                                         labels = c("Ballots cast <= pop",
                                                                    "Ballots cast > pop")))
precinct_pop_wgs84 <- st_transform(precinct_pop, 4326)

# Map of over100
pal <- colorFactor(c("white", "red"), domain = precinct_pop_wgs84$over100)

leaflet(precinct_pop_wgs84) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = precinct_pop_wgs84,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(precinct_pop_wgs84$over100),
              fillOpacity = 0.7,
              popup = paste(
                "Ballots cast: ", precinct_pop_wgs84$ballots_cast, "<br>",
                "Population (parcel-based): ", precinct_pop_wgs84$precinct_pop, "<br>",
                "Precinct: ", precinct_pop_wgs84$St_Code
              )) %>% 
  addLegend(pal = pal, values = precinct_pop_wgs84$over100, opacity = 0.7, title = "Ballots vs. parcel-based population, Precincts",
            position = "bottomright")

# Map of tracts with vote percentage > 100%
vote_viz <- read_csv("../components/votes_tract_precinctinfo.csv")

tracts <- tracts("WA", county = c(033, 035, 053, 061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>%
  select(GEOID, TRACTCE, geometry)
tracts$GEOID <- as.numeric(tracts$GEOID)

tracts <- tracts %>% left_join(vote_viz, by = "GEOID")
tracts <- tracts %>% mutate(over100 = factor(ifelse(votes > 1, 1, 0),
                                             labels = c("<=100% voted",
                                                        ">100% voted")))
pal <- colorFactor(c("white", "red"), domain = tracts$over100)

leaflet(tracts) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = tracts,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(tracts$over100),
              fillOpacity = 0.7,
              popup = paste(
                "Vote percentage: ", round(tracts$votes, 2), "<br>",
                "Intersecting precincts: ", tracts$precincts, "<br>",
                "Component resunits: ", tracts$clipped_resunits, "<br>",
                "Component turnouts: ", tracts$turnouts
              )) %>% 
  addLegend(pal = pal, values = tracts$over100, opacity = 0.7, title = "Turnout > 100%, Tracts",
            position = "bottomright")

# Precinct turnout continuous
# bins <- c(0, quantile(precinct_pop$turnout100, probs = seq(0, 1, 0.1), na.rm = TRUE))
bins <- quantile(precinct_pop_wgs84$turnout, seq(0,1,0.1), na.rm = TRUE)
# bins <- c(0, 11, 22, 33, 44, 55, 66, 77, 88, 100)
pal <- colorBin("YlOrRd", domain = precinct_pop_wgs84$turnout, bins = bins)

leaflet(precinct_pop_wgs84)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=precinct_pop_wgs84,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(precinct_pop_wgs84$turnout),
              fillOpacity = 0.7,
              popup = paste(
                "Ballots cast: ", precinct_pop_wgs84$ballots_cast, "<br>",
                "Population (parcel-based): ", precinct_pop_wgs84$precinct_pop, "<br>",
                "Turnout: ", round(precinct_pop_wgs84$turnout, 2), "<br>",
                "Precinct: ", precinct_pop_wgs84$St_Code
              )) %>% 
  addLegend(pal = pal, values = precinct_pop_wgs84$turnout, opacity = 0.7, title = "Turnout (ballots/pop), Precincts",
            position = "bottomright")

# Tract turnout continuous
bins <- quantile(tracts$votes, probs = seq(0, 1, 0.1), na.rm = TRUE)
# bins <- c(0, 11, 22, 33, 44, 55, 66, 77, 88, 100)
pal <- colorBin("YlOrRd", domain = tracts$votes, bins = bins)

leaflet(tracts) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = tracts,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(tracts$votes),
              fillOpacity = 0.7,
              popup = paste(
                "Vote percentage: ", round(tracts$votes, 2), "<br>",
                # "Intersecting precincts: ", tracts$precincts, "<br>",
                "Component resunits: ", tracts$clipped_resunits, "<br>",
                "Component turnouts: ", tracts$turnouts
              )) %>% 
  addLegend(pal = pal, values = tracts$votes, opacity = 0.7, title = "Turnout, Tracts",
            position = "bottomright")

# Density plots
old <- read_excel("Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx", sheet = 2, skip = 1)
old <- old[,c(1,47)]
names(old) <- c("GEOID", "votes")
old$votes <- as.numeric(old$votes)
old$year <- "2016"

vote$year <- "2020"

all <- rbind(vote, old)

all %>% ggplot(aes(x = votes, fill = year)) + geom_density(alpha=.2)
