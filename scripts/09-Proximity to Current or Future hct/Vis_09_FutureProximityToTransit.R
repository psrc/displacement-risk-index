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
future_ptt <- read.csv("./data/09-Proximity to Current or Future hct/09_futureProximityToTransit.csv")

future_ptt <- future_ptt %>% 
  rename(geoid=geoid20) %>% 
  select(geoid, percent_hct)

# 2014 BY data
future_ptt_2014 <- read.csv("../Displacement_Risk_Script/data/009_AreaNear2025Transit.csv")
future_ptt_2014 <- future_ptt_2014 %>% 
  rename(geoid=geoid10) %>% 
  select(geoid, percent_hct=percent_area)

# 2018 BY data
future_ptt_2018 <- read.csv("../Displacement Index 2021/data/09-Proximity to Current or Future hct/09_futureProximityToTransit_old.csv") 

future_ptt_2018 <- future_ptt_2018 %>% 
  rename(geoid=geoid10) %>% 
  select(geoid, percent_hct) %>% 
  mutate(percent_hct=percent_hct/100)

#### Note: 
# Discrepancy in 2014 BY data between file,
# Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/009_AreaNear2025Transit.csv
# and final values in spreadsheet,
# Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx

# Examine discrepancy ----
# Load 2014BY data (from finalized displacement-risk-data.xlsx file)
old_14 <- read_excel("Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx", sheet = 2, skip = 1)
old_14 <- old_14[,c(1,22)]
names(old_14) <- c("geoid", "percent_hct")

# Call values in combined spreadsheet "final"
old_14$DataVersion <- "OldFinal_2014"
old_14 <- as.data.frame(old_14)

# Call values in folder "draft"
old_14_draft <- future_ptt_2014 %>% 
  mutate(DataVersion="OldDraft")

old_18 <- future_ptt_2018 %>% 
  mutate(DataVersion = "Old_2018")

current <- future_ptt %>% 
  mutate(DataVersion = "Current")

compare <- rbind(current, old_18, old_14_draft, old_14)
compare %>% ggplot(aes(percent_hct, fill = DataVersion)) + geom_density(alpha = 0.2) + ggtitle("Percent of tract area w/in 1/4 or 1/2 mi. of future transit") + xlim(0,1)

# compare differences between two 2014 data sets and 2018 (2010 geographies)
# updated data in 2020 geographies
future_ptt_both <- old_18 %>%  
  select(geoid, percent_hct) %>%
  rename(perc_2018 = percent_hct) %>%
  #join to 2014 indicator csv data
  full_join(old_14_draft %>% select(geoid, percent_hct), by = "geoid") %>%
  rename(perc_2014_draft = percent_hct) %>%
  mutate(perc_2018 = round(perc_2018,2),
         perc_2014_draft = round(perc_2014_draft,2),
         diff_draft = perc_2018 - perc_2014_draft,
         absdiff_draft = abs(diff_draft)) %>%
  #join to 2014 combined index excel workbook data
  full_join(old_14 %>% select(-DataVersion), by = "geoid") %>%
  rename(perc_2014_final = percent_hct) %>%
  mutate(perc_2014_final = round(perc_2014_final, 2),
         diff_final = perc_2018 - perc_2014_final,
         absdiff_final= abs(diff_final),
         diff_14 = perc_2014_final-perc_2014_draft,
         absdiff_14 = abs(diff_14))

future_ptt_both %>% 
  summary()

# no further investigation into discrepancy needed moving forward (as of 2026 update)

# Tract area within 1/4 mi of future HCT data  -----------------------------------------------
# Calculate quantiles
temp = as.data.frame(quantile(future_ptt$percent_hct, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(future_ptt$percent_hct, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
future_ptt %>% ggplot(aes(percent_hct)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of area within 1/4 mile of 2035 bus rapid transit \n and 1/2 mile of other 2035 HCT stops/stations") +
  geom_vline(aes(xintercept = quantile(percent_hct, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(percent_hct, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(percent_hct, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

### Very right-skewed - make visualisations without zeros
# Calculate quantiles - no zeros
temp = as.data.frame(quantile(future_ptt %>% 
                                filter(percent_hct != 0) %>% 
                                select(percent_hct), probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(future_ptt$percent_hct, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions - exclude zeros
future_ptt %>% filter(percent_hct != 0) %>% ggplot(aes(percent_hct)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of area within 1/4 mile of 2035 bus rapid transit \n and 1/2 mile of other 2035 HCT stops/stations") +
  geom_vline(aes(xintercept = quantile(percent_hct[percent_hct != 0], 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(percent_hct, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(percent_hct, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))


# Compare 2014, 2018, and 2023 distributions
mean_future_ptt_2014 = mean(future_ptt_2014$percent_hct,na.rm = TRUE)
mean_future_ptt_2018 = mean(future_ptt_2018$percent_hct,na.rm = TRUE)
mean_future_ptt_2023 = mean(future_ptt$percent_hct,na.rm = TRUE)

# Bind data sets
future_ptt_2014$year = as.factor(2014)
future_ptt_2018$year = as.factor(2018)
future_ptt$year = as.factor(2023)

future_ptt_all <- rbind(future_ptt_2014, future_ptt_2018, future_ptt)

future_ptt_all %>% ggplot(aes(percent_hct, fill = year))+
  geom_density(alpha=.2)

future_ptt_all %>% ggplot(aes(percent_hct, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_future_ptt_2014),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_future_ptt_2018),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_future_ptt_2023),
             color="royalblue", linetype="dashed", linewidth=1)



# Mapping -----------------------------------------------
# Get tract shapefiles ----
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts10.url <- file.path(arc_service, "Census_Tracts_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract_10 <- st_read(tracts10.url)
tract_20 <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract_10, future_ptt_both,
               by.x="geoid10",
               by.y="geoid",
               all.x=TRUE)

tract_ptt <- merge(tract_20, future_ptt,
                   by.x="geoid20",
                   by.y="geoid",
                   all.x=TRUE)

## Separate years ----

# Map 2023by data
bins <- seq(min(tract_ptt$percent_hct, na.rm = TRUE), max(tract_ptt$percent_hct, na.rm = TRUE), length = 5)
pal <- colorBin("YlGnBu", domain = tract_ptt$percent_hct, bins = bins)

m <- leaflet(tract_ptt)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract_ptt,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract_ptt$percent_hct),
              fillOpacity = 0.7,
              popup = paste("percent of tract area within 0.25 or 0.5 miles to 2035 transit in 2023BY: ", tract_ptt$percent_hct,"%<br>",
                            "tract: ", tract_ptt$geoid20
              )) %>% 
  addLegend(pal = pal, values = tract_ptt$percent_hct, opacity = 0.7, title = "Percent of tract area within 0.25 or 0.5 miles to 2035 transit, 2023BY (%)",
            position = "bottomright")
print(m)


# Map 2018by data
bins <- seq(min(tract$perc_2018, na.rm = TRUE), max(tract$perc_2018, na.rm = TRUE), length = 5)
pal <- colorBin("YlGnBu", domain = tract$perc_2018, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$perc_2018),
              fillOpacity = 0.7,
              popup = paste("percent of tract area within 0.25 or 0.5 miles to 2030 transit in 2018BY: ", tract$perc_2018,"%<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$perc_2018, opacity = 0.7, title = "Percent of tract area within 0.25 or 0.5 miles to 2030 transit, 2018BY (%)",
            position = "bottomright")
print(m)

# Map 2014by data
bins <- seq(min(tract$perc_2014_draft, na.rm = TRUE), max(tract$perc_2014_draft, na.rm = TRUE), length = 5)
pal <- colorBin("YlGnBu", domain = tract$perc_2014_draft, bins = bins)


m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$perc_2014_draft),
              fillOpacity = 0.7,
              popup = paste("percent of tract area within 0.25 or 0.5 miles to 2025 transit in 2014BY: ", tract$perc_2014_draft,"%<br>",
                            "tract: ", tract$geoid
              )) %>% 
  addLegend(pal = pal, values = tract$perc_2014_draft, opacity = 0.7, title = "Percent of tract area within 0.25 or 0.5 miles to 2025 transit, 2014BY (%)",
            position = "bottomright")
print(m)


## Comparing differences ----

# Map 2018-2014 (final xlsx) absolute difference
bins <- seq(min(tract$absdiff_final, na.rm = TRUE), max(tract$absdiff_final, na.rm = TRUE), length = 7)
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
                            "percent of tract area within 0.25 or 0.5 miles to 2025 transit in 2014by (draft csv): ", tract$perc_2014_draft,"%<br>",
                            "percent of tract area within 0.25 or 0.5 miles to 2025 transit in 2014by (final xlsx): ", tract$perc_2014_final,"%<br>",
                            "percent of tract area within 0.25 or 0.5 miles to 2025 transit in 2018by: ", tract$perc_2018,"%<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$absdiff_final, opacity = 0.7, title = "Absolute difference between 2018by and 2014by (final xlsx) percents of tract area within 0.25 or 0.5 miles to 2025 transit (%)",
            position = "bottomright")
print(m)

# Map 2018-2014 (draft csv) absolute difference
bins <- seq(min(tract$absdiff_draft, na.rm = TRUE), max(tract$absdiff_draft, na.rm = TRUE), length = 7)
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
                            "percent of tract area within 0.25 or 0.5 miles to 2025 transit in 2014by (draft csv): ", tract$perc_2014_draft,"%<br>",
                            "percent of tract area within 0.25 or 0.5 miles to 2025 transit in 2014by (final xlsx): ", tract$perc_2014_final,"%<br>",
                            "percent of tract area within 0.25 or 0.5 miles to 2025 transit in 2018by: ", tract$perc_2018,"%<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$absdiff_draft, opacity = 0.7, title = "Absolute difference between 2018by and 2014by (draft csv) percent of tract area within 0.25 or 0.5 miles to 2025 transit (%)",
            position = "bottomright")
print(m)

# Map 2014 (final xlsx) - 2014 (draft csv) absolute difference
bins <- seq(min(tract$absdiff_14, na.rm = TRUE), max(tract$absdiff_14, na.rm = TRUE), length = 7)
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
                            "percent of tract area within 0.25 or 0.5 miles to 2025 transit in 2014by (draft csv): ", tract$perc_2014_draft,"%<br>",
                            "percent of tract area within 0.25 or 0.5 miles to 2025 transit in 2014by (final xlsx): ", tract$perc_2014_final,"%<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$absdiff_14, opacity = 0.7, title = "Absolute difference between 2014by (final xlsx) and 2014by (draft csv) percent of tract area within 0.25 or 0.5 miles to 2025 transit (%)",
            position = "bottomright")
print(m)
