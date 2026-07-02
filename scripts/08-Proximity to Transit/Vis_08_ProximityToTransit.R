### Descriptive and spatial analysis of the data ------------------------------------------------

# Libraries
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

# Current data (2023 BY data)
ptt <- read.csv("./data/08-Proximity to Transit/08_proximityToTransit.csv")

ptt <- ptt %>%
  rename(geoid=geoid20) %>%
  mutate(percent_pop_quarter_mile=percent_pop_quarter_mile*100)

# 2014 BY data
ptt_2014 <- read.csv("../Displacement_Risk_Script/data/008_PopulationNear2017Transit.csv")
# make 2014 fields consistent with current data
ptt_2014 <- ptt_2014 %>% 
  rename(geoid = geoid10, 
         population = HH_P_TOT,
         population_quarter_mile = HH_P_transit,
         percent_pop_quarter_mile = transit_prop) %>%
  mutate(percent_pop_quarter_mile=percent_pop_quarter_mile*100)

# 2018 BY data
ptt_2018 <- read.csv("../Displacement Index 2021/data/08-Proximity to Transit/08_proximityToTransit.csv")
# make 2018 fields consistent with current data
ptt_2018 <- ptt_2018 %>% 
  rename(geoid = geoid10) %>% 
  select(-tractce10)


#### Note: 
# Discrepancy in 2014BY data between file,
# Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/008_PopulationNear2017Transit.csv
# and final values in spreadsheet,
# Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx

# Examine discrepancy ----
# Load 2014BY data (from finalized displacement-risk-data.xlsx file)
old_14 <- read_excel("Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx", sheet = 2, skip = 1)
old_14 <- old_14[,c(1,20)]
names(old_14) <- c("geoid", "percent_pop_quarter_mile")

# Call values in combined spreadsheet "final"
old_14$DataVersion <- "OldFinal_2014"

# Call values in indicator folder "draft"
old_14_draft <- ptt_2014 %>% 
  select(geoid, percent_pop_quarter_mile) %>% 
  mutate(DataVersion = "OldDraft_2014")

old_18 <- ptt_2018 %>% 
  select(geoid, percent_pop_quarter_mile) %>% 
  mutate(DataVersion = "Old_2018")

current <- ptt %>% 
  select(geoid, percent_pop_quarter_mile) %>% 
  mutate(DataVersion = "Current")

compare <- rbind(current, old_18, old_14_draft, old_14)
compare %>% ggplot(aes(percent_pop_quarter_mile, fill = DataVersion)) + geom_density(alpha = 0.2) + ggtitle("Percent of population w/in 1/4 mi. of transit")

# compare differences between two 2014 data sets and 2018 (2010 geographies)
# updated data in 2020 geographies
ptt_both <- old_18 %>%  
  select(geoid, percent_pop_quarter_mile) %>%
  rename(perc_2018 = percent_pop_quarter_mile) %>%
  #join to 2014 indicator csv data
  full_join(old_14_draft %>% select(geoid, percent_pop_quarter_mile), by = "geoid") %>%
  rename(perc_2014_draft = percent_pop_quarter_mile) %>%
  mutate(perc_2018 = round(perc_2018,2),
         perc_2014_draft = round(perc_2014_draft,2),
         diff_draft = perc_2018 - perc_2014_draft,
         absdiff_draft = abs(diff_draft)) %>%
  #join to 2014 combined index excel workbook data
  full_join(old_14 %>% select(-DataVersion), by = "geoid") %>%
  rename(perc_2014_final = percent_pop_quarter_mile) %>%
  mutate(perc_2014_final = round(perc_2014_final, 2),
         diff_final = perc_2018 - perc_2014_final,
         absdiff_final= abs(diff_final),
         diff_14 = perc_2014_final-perc_2014_draft,
         absdiff_14 = abs(diff_14))

ptt_both %>% 
  summary()

# no further investigation into discrepancy needed moving forward (as of 2026 update)


# Population within 1/4 mi of HCT data  -----------------------------------------------
# Calculate quantiles
temp = as.data.frame(quantile(ptt$percent_pop_quarter_mile, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$percent_pop_quarter_mile, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
ptt %>% ggplot(aes(percent_pop_quarter_mile)) +
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
temp = as.data.frame(quantile(ptt %>% 
                                filter(percent_pop_quarter_mile != 0) %>% 
                                select(percent_pop_quarter_mile), probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$percent_pop_quarter_mile, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions - exclude zeros
ptt %>% filter(percent_pop_quarter_mile != 0) %>% ggplot(aes(percent_pop_quarter_mile)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of pop. within 1/4 mile to frequent or high capacity transit") +
  geom_vline(aes(xintercept = quantile(percent_pop_quarter_mile[percent_pop_quarter_mile != 0], 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(percent_pop_quarter_mile, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(percent_pop_quarter_mile, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))




# Compare 2014, 2018, and 2023 distributions
mean_ptt_2014 = mean(ptt_2014$percent_pop_quarter_mile,na.rm = TRUE)
mean_ptt_2018 = mean(ptt_2018$percent_pop_quarter_mile,na.rm = TRUE)
mean_ptt_2023 = mean(ptt_2023$percent_pop_quarter_mile,na.rm = TRUE)

# Bind data sets
ptt_2014$year = as.factor(2014)
ptt_2018$year = as.factor(2018)
ptt$year = as.factor(2023)

ptt_all <- rbind(ptt_2014, ptt_2018, ptt)

ptt_all %>% ggplot(aes(percent_pop_quarter_mile, fill = year))+
  geom_density(alpha=.2)

ptt_all %>% ggplot(aes(percent_pop_quarter_mile, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_ptt_2014),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_ptt_2018),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_ptt_2023),
             color="royalblue", linetype="dashed", linewidth=1)






# Mapping -----------------------------------------------
# Get tract shapefiles ----
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts10.url <- file.path(arc_service, "Census_Tracts_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract_10 <- st_read(tracts10.url)
tract_20 <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract_10, ptt_both,
               by.x="geoid10",
               by.y="geoid",
               all.x=TRUE)

tract_ptt <- merge(tract_20, ptt,
                  by.x="geoid20",
                  by.y="geoid",
                  all.x=TRUE)

## Separate years ----

# Map 2023by data
# bins <- seq(min(tract_ptt$percent_pop_quarter_mile, na.rm = TRUE), max(tract_ptt$percent_pop_quarter_mile, na.rm = TRUE), length = 10)
bins <- c(0, 11, 22, 33, 44, 55, 66, 77, 88, 100)
pal <- colorBin("YlGnBu", domain = tract_ptt$percent_pop_quarter_mile, bins = bins)

m <- leaflet(tract_ptt)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract_ptt,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract_ptt$percent_pop_quarter_mile),
              fillOpacity = 0.7,
              popup = paste("percent of population within 0.25 miles to transit in 2023BY: ", tract_ptt$percent_pop_quarter_mile,"%<br>",
                            "tract: ", tract_ptt$geoid20
              )) %>% 
  addLegend(pal = pal, values = tract_ptt$percent_pop_quarter_mile, opacity = 0.7, title = "Percent of pop within .25 mi to freq transit, 2023BY (%)",
            position = "bottomright")
print(m)


# Map 2018by data
# bins <- seq(min(tract$perc_2018, na.rm = TRUE), max(tract$perc_2018, na.rm = TRUE), length = 10)
bins <- c(0, 11, 22, 33, 44, 55, 66, 77, 88, 100)
pal <- colorBin("YlGnBu", domain = tract$perc_2018, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$perc_2018),
              fillOpacity = 0.7,
              popup = paste("percent of population within 0.25 miles to transit in 2018BY: ", tract$perc_2018,"%<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$perc_2018, opacity = 0.7, title = "Percent of pop within .25 mi to freq transit, 2018BY (%)",
            position = "bottomright")
print(m)

# Map 2014by data
# bins <- seq(min(tract$perc_2014_draft, na.rm = TRUE), max(tract$perc_2014_draft, na.rm = TRUE), length = 10)
bins <- c(0, 11, 22, 33, 44, 55, 66, 77, 88, 100)
pal <- colorBin("YlGnBu", domain = tract$perc_2014_draft, bins = bins)


m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$perc_2014_draft),
              fillOpacity = 0.7,
              popup = paste("percent of population within 0.25 miles to transit in 2014BY: ", tract$perc_2014_draft,"%<br>",
                            "tract: ", tract$geoid
              )) %>% 
  addLegend(pal = pal, values = tract$perc_2014_draft, opacity = 0.7, title = "Percent of pop within .25 mi to freq transit, 2014BY (%)",
            position = "bottomright")
print(m)


## Comparing differences ----

# Map 2018-2014 (final xlsx) absolute difference
bins <- seq(min(tract$absdiff_final, na.rm = TRUE), max(tract$absdiff_final, na.rm = TRUE), length = 10)
pal <- colorBin("YlOrRd", domain = tract$absdifference, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$absdiff_final),
              fillOpacity = 0.7,
              popup = paste("2018 to 2014 (final xlsx) abs difference: ", tract$absdiff_final,"<br>",
                            "percent of population within 0.25 miles to transit in 2014by (draft csv): ", tract$perc_2014_draft,"%<br>",
                            "percent of population within 0.25 miles to transit in 2014by (final xlsx): ", tract$perc_2014_final,"%<br>",
                            "percent of population within 0.25 miles to transit in 2018by: ", tract$perc_2018,"%<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$absdiff_final, opacity = 0.7, title = "Absolute difference between 2018by and 2014by (final xlsx) percents of pop within .25 mi to freq transit (%)",
            position = "bottomright")
print(m)

# Map 2018-2014 (draft csv) absolute difference
bins <- seq(min(tract$absdiff_draft, na.rm = TRUE), max(tract$absdiff_draft, na.rm = TRUE), length = 10)
pal <- colorBin("YlOrRd", domain = tract$absdifference, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$absdiff_draft),
              fillOpacity = 0.7,
              popup = paste("2018 to 2014 (draft csv) abs difference: ", tract$absdiff_draft,"<br>",
                            "percent of population within 0.25 miles to transit in 2014by (draft csv): ", tract$perc_2014_draft,"%<br>",
                            "percent of population within 0.25 miles to transit in 2014by (final xlsx): ", tract$perc_2014_final,"%<br>",
                            "percent of population within 0.25 miles to transit in 2018by: ", tract$perc_2018,"%<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$absdiff_draft, opacity = 0.7, title = "Absolute difference between 2018by and 2014by (draft csv) percents of pop within .25 mi to freq transit (%)",
            position = "bottomright")
print(m)

# Map 2014 (final xlsx) - 2014 (draft csv) absolute difference
bins <- seq(min(tract$absdiff_14, na.rm = TRUE), max(tract$absdiff_14, na.rm = TRUE), length = 10)
pal <- colorBin("YlOrRd", domain = tract$absdiff_14, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$absdiff_14),
              fillOpacity = 0.7,
              popup = paste("2014 (final xlsx) to 2014 (draft csv) abs difference: ", tract$absdiff_14,"<br>",
                            "percent of population within 0.25 miles to transit in 2014by (draft csv): ", tract$perc_2014_draft,"%<br>",
                            "percent of population within 0.25 miles to transit in 2014by (final xlsx): ", tract$perc_2014_final,"%<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$absdiff_14, opacity = 0.7, title = "Absolute difference between 2014by (final xlsx) and 2014by (draft csv) percents of pop within .25 mi to freq transit (%)",
            position = "bottomright")
print(m)
