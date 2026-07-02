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

# Access to jobs by auto data  -----------------------------------------------
# Current data
jobs_auto <- read.csv("./data/07-Access to Jobs/07a_AccesstoJobs.csv")

# 2014 BY data
jobs_auto_2014 <- read.csv("../Displacement_Risk_Script/data/007_1_JobsBy30minAuto.csv")
jobs_auto_2014 <- jobs_auto_2014 %>%
  mutate(NumJobsAuto=as.numeric(gsub(",", "", HHaveraged_EMPTOT_P_30mins_auto))) %>%
  rename(GEOID=census_tract) %>%
  select(GEOID, NumJobsAuto)

# 2018 BY data
jobs_auto_2018 <- read.csv("../Displacement Index 2021/data/07-Access to Jobs/07_a_AccesstoJobs.csv")

# Calculate quantiles
temp = as.data.frame(quantile(jobs_auto$NumJobsAuto, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(jobs_auto$NumJobsAuto, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
jobs_auto %>% ggplot(aes(NumJobsAuto)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of jobs within 30min auto travel time") +
  geom_vline(aes(xintercept = quantile(NumJobsAuto, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(NumJobsAuto, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(NumJobsAuto, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2014, 2018, and 2023 distributions
mean_auto_2014 = mean(jobs_auto_2014$NumJobsAuto,na.rm = TRUE)
mean_auto_2018 = mean(jobs_auto_2018$NumJobsAuto,na.rm = TRUE)
mean_auto_2023 = mean(jobs_auto$NumJobsAuto,na.rm = TRUE)

# Bind data sets
jobs_auto_2014$year = as.factor(2014)
jobs_auto_2018$year = as.factor(2018)
jobs_auto$year = as.factor(2023)

jobs_auto_all <- rbind(jobs_auto_2014, jobs_auto_2018, jobs_auto)

jobs_auto_all %>% ggplot(aes(NumJobsAuto,fill = year))+
  geom_density(alpha=.2)

jobs_auto_all %>% ggplot(aes(NumJobsAuto, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_auto_2014),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_auto_2018),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_auto_2023),
             color="royalblue", linetype="dashed", linewidth=1)


# Compare 2018 auto data with 2014 ----------------------------------------------- 
# Can't compare most recent data with previous because of differing census geographies
# Re-shaping the data, joining both data and computing the difference
jobs_auto_14 <- jobs_auto_2014 %>%
  rename(NumJobsAuto_14 = NumJobsAuto) %>% 
  select(GEOID, NumJobsAuto_14)
jobs_auto_18 <- jobs_auto_2018 %>%
  rename(NumJobsAuto_18 = NumJobsAuto) %>% 
  select(GEOID, NumJobsAuto_18)

job_auto_both <- jobs_auto_14 %>%
  full_join(jobs_auto_18, by = "GEOID") %>% 
  mutate(difference = NumJobsAuto_18 - NumJobsAuto_14, 
         perc_dif = (NumJobsAuto_18 - NumJobsAuto_14)/NumJobsAuto_14*100)
                            
job_auto_both %>% 
  summary()

# Correlation
cor(jobs_auto_18$NumJobsAuto_18, jobs_auto_14$NumJobsAuto_14, 
    method = c("pearson", "kendall", "spearman"), use = "complete.obs")


# Mapping -----------------------------------------------
# Get tract shapefiles ----
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts10.url <- file.path(arc_service, "Census_Tracts_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract_10 <- st_read(tracts10.url)
tract_20 <- st_read(tracts20.url)


## Joining spatial and tabular data by tract ----
tract <- merge(tract_10, job_auto_both,
               by.x="geoid10",
               by.y="GEOID",
               all.x=TRUE)

tract_auto <- merge(tract_20, jobs_auto,
                   by.x="geoid20",
                   by.y="GEOID",
                   all.x=TRUE)


## Separate years ----

# Map 2023by data
bins <- seq(min(tract_auto$NumJobsAuto, na.rm = TRUE), max(tract_auto$NumJobsAuto, na.rm = TRUE), length = 7)
pal <- colorBin("YlGnBu", domain = tract_auto$NumJobsAuto, bins = bins)

m <- leaflet(tract_auto)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract_auto,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract_auto$NumJobsAuto),
              fillOpacity = 0.7,
              popup = paste("jobs access by car within 30 min in 2023BY: ", tract_auto$NumJobsAuto,"<br>",
                            "tract: ", tract_auto$geoid20
              )) %>% 
  addLegend(pal = pal, values = tract_auto$NumJobsAuto, opacity = 0.7, title = "Access to jobs by auto within 30min, 2023BY",
            position = "bottomright")
print(m)

# Map 2018by data
bins <- seq(min(tract$NumJobsAuto_18, na.rm = TRUE), max(tract$NumJobsAuto_18, na.rm = TRUE), length = 7)
pal <- colorBin("YlGnBu", domain = tract$NumJobsAuto_18, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$NumJobsAuto_18),
              fillOpacity = 0.7,
              popup = paste("jobs access by car within 30 min in 2018BY: ", tract$NumJobsAuto_18,"<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$NumJobsAuto_18, opacity = 0.7, title = "Access to jobs by auto within 30min, 2018BY",
            position = "bottomright")
print(m)

# Map 2014by data
bins <- seq(min(tract$NumJobsAuto_14, na.rm = TRUE), max(tract$NumJobsAuto_14, na.rm = TRUE), length = 7)
pal <- colorBin("YlGnBu", domain = tract$NumJobsAuto_14, bins = bins)

m <- leaflet(tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=tract,
              stroke = T,
              opacity = 1,
              weight = 0.2,
              fillColor = ~pal(tract$NumJobsAuto_14),
              fillOpacity = 0.7,
              popup = paste("jobs access by car within 30 min in 2014BY: ", tract$NumJobsAuto_14,"<br>",
                            "tract: ", tract$geoid10
              )) %>% 
  addLegend(pal = pal, values = tract$NumJobsAuto_14, opacity = 0.7, title = "Access to jobs by auto within 30min, 20184Y",
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
                            "new (18BY) jobs: ", tract$NumJobsAuto_18,"<br>",
                            "old (14BY) jobs: ", tract$NumJobsAuto_14,"<br>",
                            "% dif: ", tract$perc_dif)) %>% 
  addLegend(pal = pal, values = tract$difference, opacity = 0.7, title = "Difference between 2018 and 2014 ('18-'14) jobs access by car within 30 min",
            position = "bottomright")
print(m)
