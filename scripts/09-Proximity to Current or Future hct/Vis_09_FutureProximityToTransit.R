# 9 Proximity to Current or Future Link Light Rail & Streetcar
# Percent of area within .5 miles to 2030 ferry, street car, commuter rail and light rail and 
# .25 miles to 2030 BRT 
# Descriptive and spatial analysis of the data ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(sf)
# install.packages(tigris)
# install.packages(leaflet)
library(tidyverse)
library(sf)
library(tigris)
library(leaflet)

# Compare 2025 to 2030

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Load current data  ----------------------------------------------- 
prox_2030 <- read.csv("./data/09-Proximity to Current or Future hct/09_futureProximityToTransit.csv") %>% mutate(prop_transit = percent_hct/100)

# Load previous data (from data folder)
prox_2025 <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/009_AreaNear2025Transit.csv")

#### Note: 
# Discrepancy in 2025 data between file,
# Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/009_AreaNear2025Transit.csv
# and final values in spreadsheet,
# Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx

# Examine discrepancy
# Load 2025 data (from finalized displacement-risk-data.xlsx file)
old <- read_excel("Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx", sheet = 2, skip = 1)
old <- old[,c(1,22)]
names(old) <- c("GEOID", "prop_transit")

# Call values in spreadsheet "final"
old$type <- "OldFinal"

# Call values in folder "draft"
old_draft <- prox_2025 %>% select(GEOID = geoid10, prop_transit = percent_area)
old_draft$type <- "OldDraft"

current <- prox_2030 %>% select(GEOID = geoid10, prop_transit) %>% mutate(type = "Current")

compare <- rbind(current, old_draft, old)
compare %>% ggplot(aes(prop_transit, fill = type)) + geom_density(alpha = 0.2) 

# Population within 1/4 mi of HCT data  ----------------------------------------------- 
data <- read_csv("./data/09-Proximity to Current or Future hct/09_futureProximityToTransit.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$percent_hct, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$percent_hct, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(percent_hct)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of area within 1/4 mile of 2030 bus rapid transit \n and 1/2 mile of other 2030 HCT stops/stations") +
  geom_vline(aes(xintercept = quantile(percent_hct, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(percent_hct, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(percent_hct, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

### Very right-skewed - make visualisations without zeros
# Calculate quantiles - no zeros
temp = as.data.frame(quantile(data %>% 
                                filter(percent_hct != 0) %>% 
                                select(percent_hct), probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$percent_hct, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions - exclude zeros
data %>% filter(percent_hct != 0) %>% ggplot(aes(percent_hct)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of area within 1/4 mile of 2030 bus rapid transit \n and 1/2 mile of other 2030 HCT stops/stations") +
  geom_vline(aes(xintercept = quantile(percent_hct[percent_hct != 0], 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(percent_hct, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(percent_hct, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

### Exploring issues with UrbanSim output by examining values alongside transit stops
# Transit stops
hct_2030 = read_sf('T:/2021December/Stefan/Polina/2030/hct_stops_2030.shp')
brt_2030 = read_sf('T:/2021December/Stefan/Polina/2030/brt_stops_2030.shp')

hct_2030_upd = hct_2030 %>% 
                    st_as_sf(coords = c("geometry")) %>% 
                    st_transform(2285) %>% 
                    st_transform(4326)

brt_2030_upd = brt_2030 %>% 
  st_as_sf(coords = c("geometry")) %>% 
  st_transform(2285) %>% 
  st_transform(4326)

prox_both = prox_2030 %>% 
  select(geoid10, percent_hct) %>%
  rename(area_2030 = percent_hct) %>%
  full_join(prox_2025, by = "geoid10") %>% 
  rename(area_2025 = percent_area) %>%
  mutate(area_2030_upd = replace_na(area_2030, 0), 
         area_2025_upd = replace_na(area_2025, 0),
         diff_draft = area_2030_upd - area_2025_upd) %>%
  full_join(old %>% select(-type), by = c("geoid10" = "GEOID")) %>%
  mutate(area_2025_final = replace_na(prop_transit, 0), 
         diff_final = area_2030_upd - area_2025_final,
         geoid10 = as.character(geoid10))

prox_both %>% 
  summary()

# Get tract shapefiles

psrc_tracts <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(prox_both, by = c("GEOID"= "geoid10"))

# Map difference
bins <- c(min(psrc_tracts$diff_draft),-0.35,-0.18, 0, 0.15, 0.32, 0.5, 0.66, 0.83, max(psrc_tracts$diff_final))
pal <- colorBin("PRGn", domain = psrc_tracts$diff_final, bins = bins)

m <- leaflet(psrc_tracts)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = psrc_tracts,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(psrc_tracts$diff_final),
              fillOpacity = 0.7,
              popup = paste("new - old (final) difference: ", psrc_tracts$diff_final,"<br>",
                            "% of area within 0.5 miles to high capcity transit in 2025: ", psrc_tracts$area_2025_upd,"<br>",
                            "% of area within 0.5 miles to high capcity transit in 2030: ", psrc_tracts$area_2030_upd,"<br>",
                            "tract", psrc_tracts$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = psrc_tracts$diff_final, opacity = 0.7, title = "Difference between 2030 and 2025 (final) of % of area 0.25 miles of future BRT and 0.5 mile of future rail and other hct stops/stations",
            position = "bottomright")
print(m)

# Map 2030
bins <- c(min(psrc_tracts$area_2030_upd,na.rm = TRUE),0.1,0.2,0.3, 0.4,0.5,0.6,0.7,0.8,0.9, max(psrc_tracts$area_2030_upd,na.rm = TRUE))
pal <- colorBin("BuGn", domain = psrc_tracts$area_2030_upd, bins = bins)

m <- leaflet(psrc_tracts) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = psrc_tracts,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(psrc_tracts$area_2030_upd),
              fillOpacity = 0.7,
              popup = paste("% of area within 0.5 miles to high capcity transit in 2025: ", psrc_tracts$area_2025_upd,"<br>",
                            "% of area within 0.5 miles to high capcity transit in 2030: ", psrc_tracts$area_2030_upd,"<br>",
                            "tract", psrc_tracts$TRACTCE
              )) %>% 
  # addCircleMarkers(data = hct_2030_upd,
  #                  radius = 2,
  #                  color = "darkred",
  #                  fillOpacity = 0.9) %>% 
  # addCircleMarkers(data = brt_2030_upd,
  #                  radius = 2,
  #                  color = "green4",
  #                  fillOpacity = 0.9) %>%
  addLegend(pal = pal, values = psrc_tracts$area_2030_upd, opacity = 0.7, title = "2030: % of area within 0.25 miles of future BRT and 0.5 mile of future rail and other hct stops/stations",
            position = "bottomright") # %>% 
#   addLegend(colors = "darkred", labels = "High capacity transit stops", opacity = 0.7,
#             position = "bottomright") %>% 
# addLegend(colors = "black", labels = "BRT stops", opacity = 0.7,
#           position = "bottomright")
print(m)

# Map 2025
bins <- c(min(psrc_tracts$area_2025_final,na.rm = TRUE),0.1,0.2,0.3, 0.4,0.5,0.6,0.7,0.8,0.9, max(psrc_tracts$area_2025_final,na.rm = TRUE))
pal <- colorBin("BuGn", domain = psrc_tracts$area_2025_final, bins = bins)

m <- leaflet(psrc_tracts)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=psrc_tracts,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(psrc_tracts$area_2025_final),
              fillOpacity = 0.7,
              popup = paste(
                "% of area within 0.5 miles to high capcity transit in 2025: ", psrc_tracts$area_2025_final,"<br>",
                "% of area within 0.5 miles to high capcity transit in 2030: ", psrc_tracts$area_2030_final,"<br>",
                "tract", psrc_tracts$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = psrc_tracts$area_2025_final, opacity = 0.7, title = "2025 (final): % of area within 0.5 mile of future high capacity transit stops/stations",
            position = "bottomright")
print(m)