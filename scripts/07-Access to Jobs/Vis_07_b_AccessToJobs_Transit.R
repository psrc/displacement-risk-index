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

# Access to jobs by transit data  -----------------------------------------------
# Current data
jobs_transit <- read.csv("./data/07-Access to Jobs/07b_AccesstoJobs.csv")

# 2014 BY data
jobs_transit_2014 <- read.csv("../Displacement_Risk_Script/data/007_2_JobsBy45minTransit.csv")
jobs_transit_2014 <- jobs_transit_2014 %>%
  mutate(NumJobsTransit=as.numeric(gsub(",", "", HHaveraged_EMPTOT_P_45mins_transit))) %>%
  rename(GEOID=census_tract) %>%
  select(GEOID, NumJobsTransit)

# 2018 BY data
jobs_transit_2018 <- read.csv("../Displacement Index 2021/data/07-Access to Jobs/07_b_AccesstoJobs.csv")

# Calculate quantiles
temp = as.data.frame(quantile(jobs_transit$NumJobsTransit, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(jobs_transit$NumJobsTransit, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
jobs_transit %>% ggplot(aes(NumJobsTransit)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of jobs within 45min transit travel time") +
  geom_vline(aes(xintercept = quantile(NumJobsTransit, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(NumJobsTransit, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(NumJobsTransit, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2014, 2018, and 2023 distributions
mean_transit_2014 = mean(jobs_transit_2014$NumJobsTransit,na.rm = TRUE)
mean_transit_2018 = mean(jobs_transit_2018$NumJobsTransit,na.rm = TRUE)
mean_transit_2023 = mean(jobs_transit$NumJobsTransit,na.rm = TRUE)

# Bind data sets
jobs_transit_2014$year = as.factor(2014)
jobs_transit_2018$year = as.factor(2018)
jobs_transit$year = as.factor(2023)

jobs_transit_all <- rbind(jobs_transit_2014, jobs_transit_2018, jobs_transit)

jobs_transit_all %>% ggplot(aes(NumJobsTransit,fill = year))+
  geom_density(alpha=.2)

jobs_transit_all %>% ggplot(aes(NumJobsTransit, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_transit_2014),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_transit_2018),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_transit_2023),
             color="royalblue", linetype="dashed", linewidth=1)


# Compare 2018 transit data with 2014 ----------------------------------------------- 
# Can't compare most recent data with previous because of differing census geographies
# Re-shaping the data, joining both data and computing the difference
jobs_transit_14 <- jobs_transit_2014 %>%
  rename(NumJobsTransit_14 = NumJobsTransit) %>% 
  select(GEOID, NumJobsTransit_14)
jobs_transit_18 <- jobs_transit_2018 %>%
  rename(NumJobsTransit_18 = NumJobsTransit) %>% 
  select(GEOID, NumJobsTransit_18)

job_transit_both <- jobs_transit_14 %>%
  full_join(jobs_transit_18, by = "GEOID") %>% 
  mutate(difference = NumJobsTransit_18 - NumJobsTransit_14, 
         perc_dif = (NumJobsTransit_18 - NumJobsTransit_14)/NumJobsTransit_14*100)

job_transit_both %>% 
  summary()

# Correlation
cor(jobs_transit_18$NumJobsTransit_18, jobs_transit_14$NumJobsTransit_14, 
    method = c("pearson", "kendall", "spearman"), use = "complete.obs")


# Mapping -----------------------------------------------
# Get tract shapefiles ----
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts10.url <- file.path(arc_service, "Census_Tracts_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract_10 <- st_read(tracts10.url)
tract_20 <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract_10, job_transit_both,
               by.x="geoid10",
               by.y="GEOID",
               all.x=TRUE)

tract_transit <- merge(tract_20, jobs_transit,
                       by.x="geoid20",
                       by.y="GEOID",
                       all.x=TRUE)


## Separate years ----

# Map 2023by data
bins <- seq(min(tract_transit$NumJobsTransit, na.rm = TRUE), max(tract_transit$NumJobsTransit, na.rm = TRUE), length = 7)
pal <- colorBin("YlGnBu", domain = tract_transit$NumJobsTransit, bins = bins)

m <- leaflet(tract_transit)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract_transit,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract_transit$NumJobsTransit),
              fillOpacity = 0.7,
              popup = paste("jobs access by transit within 45 min in 2023BY: ", tract_transit$NumJobsTransit,"<br>",
                            "tract: ", tract_transit$geoid20
              )) %>% 
  addLegend(pal = pal, values = tract_transit$NumJobsTransit, opacity = 0.7, title = "Access to jobs by transit within 45min, 2023BY",
            position = "bottomright")
print(m)

# Map 2018by data
bins <- seq(min(tract$NumJobsTransit_18, na.rm = TRUE), max(tract$NumJobsTransit_18, na.rm = TRUE), length = 7)
pal <- colorBin("YlGnBu", domain = tract$NumJobsTransit_18, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$NumJobsTransit_18),
              fillOpacity = 0.7,
              popup = paste("jobs access by transit within 45 min in 2018BY: ", tract$NumJobsTransit_18,"<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$NumJobsTransit_18, opacity = 0.7, title = "Access to jobs by transit within 45min, 2018BY",
            position = "bottomright")
print(m)

# Map 2014by data
bins <- seq(min(tract$NumJobsTransit_14, na.rm = TRUE), max(tract$NumJobsTransit_14, na.rm = TRUE), length = 7)
pal <- colorBin("YlGnBu", domain = tract$NumJobsTransit_14, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$NumJobsTransit_14),
              fillOpacity = 0.7,
              popup = paste("jobs access by transit within 45 min in 2014BY: ", tract$NumJobsTransit_14,"<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$NumJobsTransit_14, opacity = 0.7, title = "Access to jobs by transit within 45min, 2014BY",
            position = "bottomright")
print(m)


## Comparing differences ----
bins <- quantile(tract$difference, probs = seq(0, 1, 0.2), na.rm = TRUE)
pal <- colorBin("YlOrRd", domain = tract$difference, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$difference),
              fillOpacity = 0.7,
              popup = paste("new to old difference: ", tract$difference,"<br>",
                            "new (18BY) jobs: ", tract$NumJobsTransit_18,"<br>",
                            "old (14BY) jobs: ", tract$NumJobsTransit_14,"<br>",
                            "% dif: ", tract$perc_dif)) %>% 
  addLegend(pal = pal, values = tract$difference, opacity = 0.7, title = "Difference between 2018 and 2014 ('18-'14) jobs access by transit within 45 min",
            position = "bottomright")
print(m)
