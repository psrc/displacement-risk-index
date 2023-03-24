# 8	Proximity to Transit - Percent of population within ¼ mile to frequent or high capacity transit
### Descriptive and spatial analysis of the data ------------------------------------------------

# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(sf)
# install.packages(tigris)
# install.packages(leaflet)
# install.packages(readxl)
library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(readxl)

# Comparing 2018 base year and 2014 base year 

# Load current data  ----------------------------------------------- 
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")
ptt_2018 <- read.csv("./data/08-Proximity to Transit/08_proximityToTransit.csv") %>% mutate(percent_pop_quarter_mile = percent_pop_quarter_mile/100)

# Load 2014BY data (from data folder)
ptt_2014 <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/008_PopulationNear2017Transit.csv")

#### Note: 
# Discrepancy in 2014BY data between file,
# Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/008_PopulationNear2017Transit.csv
# and final values in spreadsheet,
# Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx

# Examine discrepancy
# Load 2014BY data (from finalized displacement-risk-data.xlsx file)
old <- read_excel("Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx", sheet = 2, skip = 1)
old <- old[,c(1,20)]
names(old) <- c("GEOID", "prop_pop_transit")

# Call values in spreadsheet "final"
old$DataVersion <- "OldFinal"

# Call values in folder "draft"
old_draft <- ptt_2014 %>% select(GEOID = geoid10, prop_pop_transit = transit_prop)
old_draft$DataVersion <- "OldDraft"

current <- ptt_2018 %>% select(GEOID = geoid10, prop_pop_transit = percent_pop_quarter_mile) %>% mutate(DataVersion = "Current")

compare <- rbind(current, old_draft, old)
compare %>% ggplot(aes(prop_pop_transit, fill = DataVersion)) + geom_density(alpha = 0.2) + ggtitle("08 - Percent of population w/in 1/4 mi. of transit")

ptt_both <- ptt_2018 %>%  
  select(geoid10, percent_pop_quarter_mile) %>%
  rename(perc_2018 = percent_pop_quarter_mile) %>%
  full_join(ptt_2014, by = "geoid10") %>% 
  rename(perc_2014 = transit_prop) %>%
  mutate(perc_2018 = perc_2018 * 100,
         perc_2014 = perc_2014 * 100,
         diff_draft = round(perc_2018 - perc_2014, 2),
         absdifference = abs(diff_draft)) %>%
  full_join(old %>% select(-DataVersion), by = c("geoid10" = "GEOID")) %>%
  mutate(perc_2014_final = replace_na(prop_pop_transit, 0), 
         diff_final = round(perc_2018 - perc_2014_final),
         geoid10 = as.character(geoid10))

ptt_both$perc_2018 <- round(ptt_both$perc_2018, 2)
ptt_both$perc_2014 <- round(ptt_both$perc_2014, 2)

ptt_both %>% 
  summary()

# Population within 1/4 mi of HCT data  ----------------------------------------------- 
data <- read_csv("./data/08-Proximity to Transit/08_proximityToTransit.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$percent_pop_quarter_mile, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$percent_pop_quarter_mile, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(percent_pop_quarter_mile)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of pop. within 1/4 mile to frequent or high capacity transit") +
  geom_vline(aes(xintercept = quantile(percent_pop_quarter_mile, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(percent_pop_quarter_mile, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(percent_pop_quarter_mile, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

### Very right-skewed - make visualisations without zeros
# Calculate quantiles - no zeros
temp = as.data.frame(quantile(data %>% 
                                filter(percent_pop_quarter_mile != 0) %>% 
                                select(percent_pop_quarter_mile), probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$percent_pop_quarter_mile, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions - exclude zeros
data %>% filter(percent_pop_quarter_mile != 0) %>% ggplot(aes(percent_pop_quarter_mile)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of pop. within 1/4 mile to frequent or high capacity transit") +
  geom_vline(aes(xintercept = quantile(percent_pop_quarter_mile[percent_pop_quarter_mile != 0], 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(percent_pop_quarter_mile, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(percent_pop_quarter_mile, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Get tract shapefiles
tract <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(ptt_both, by = c("GEOID"= "geoid10"))

# Map 2018 absolute difference between updated data and old data
bins <- seq(min(tract$absdifference, na.rm = TRUE), max(tract$absdifference, na.rm = TRUE), length = 10)
pal <- colorBin("BrBG", domain = tract$absdifference, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(tract$absdifference),
              fillOpacity = 0.7,
              popup = paste("new to old abs difference: ", tract$absdifference,"<br>",
                            "percent of population within 0.25 miles to transit in 2014by: ", tract$perc_2014,"%<br>",
                            "percent of population within 0.25 miles to transit in 2018by: ", tract$perc_2018,"%<br>",
                            "tract: ", tract$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = tract$absdifference, opacity = 0.7, title = "Absolute Difference between 2018by and 2014by percents of pop within .25 mi to freq transit (%)",
            position = "bottomright")
print(m)

# Map 2018 difference between updated data and old data
bins <- seq(min(tract$difference, na.rm = TRUE), max(tract$difference, na.rm = TRUE), length = 10)
pal <- colorBin("BrBG", domain = tract$difference, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(tract$difference),
              fillOpacity = 0.7,
              popup = paste("new to old abs difference: ", tract$difference,"<br>",
                            "percent of population within 0.25 miles to transit in 2014by: ", tract$perc_2014,"%<br>",
                            "percent of population within 0.25 miles to transit in 2018by: ", tract$perc_2018,"%<br>",
                            "tract: ", tract$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = tract$difference, opacity = 0.7, title = "Difference (2018by - 2014by) in percents of pop within .25 mi to freq transit (%)",
            position = "bottomright")
print(m)

# Map 2018 updated data
# bins <- seq(min(tract$perc_2018, na.rm = TRUE), max(tract$perc_2018, na.rm = TRUE), length = 10)
bins <- c(0, 11, 22, 33, 44, 55, 66, 77, 88, 100)
pal <- colorBin("YlGnBu", domain = tract$perc_2018, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(tract$perc_2018),
              fillOpacity = 0.7,
              popup = paste("new to old abs difference: ", tract$absdifference,"<br>",
                            "percent of population within 0.25 miles to transit in 2014by used: ", tract$perc_2014,"%<br>",
                            "percent of population within 0.25 miles to transit in 2018by new: ", tract$perc_2018,"%<br>",
                            "tract: ", tract$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = tract$perc_2018, opacity = 0.7, title = "Percent of pop within .25 mi to freq transit, 2018by (%)",
            position = "bottomright")
print(m)

# Map showing 2014by data
# bins <- seq(min(tract$perc_2014, na.rm = TRUE), max(tract$perc_2014, na.rm = TRUE), length = 10)
bins <- c(0, 11, 22, 33, 44, 55, 66, 77, 88, 100)
pal <- colorBin("YlGnBu", domain = tract$perc_2014, bins = bins)


m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(tract$perc_2014),
              fillOpacity = 0.7,
              popup = paste("new to old abs difference: ", tract$absdifference,"<br>",
                            "percent of population within 0.25 miles to transit in 2014by used: ", tract$perc_2014,"<br>",
                            "percent of population within 0.25 miles to transit in 2018by new: ", tract$perc_2018,"<br>",
                            "tract: ", tract$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = tract$perc_2014, opacity = 0.7, title = "percent of population within .25 mi to freq transit, 2014by (%)",
            position = "bottomright")
print(m)


