# 07 - access to number of jobs by 45 min transit ride ------------------------------------------------
# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(sf)
# install.packages(tigris)
# install.packages(leaflet)
library(tidyverse)
library(sf)
library(tigris)
library(leaflet)

# Compare 2018 transit data with 2014 ----------------------------------------------- 
# Load current and old data  ----------------------------------------------- 
transit2014 <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/007_2_JobsBy45minTransit.csv")

transit2014$HHaveraged_EMPTOT_P_45mins_transit = as.numeric(gsub(",", "", transit2014$HHaveraged_EMPTOT_P_45mins_transit))

transit2014 = transit2014 %>% 
                  mutate(census_tract = as.character(census_tract))

transit2018 <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/07-Access to Jobs/transit_jobs_access.csv")
transit2018$value <- round(transit2018$value)

# Transforming current data and joining it with old data
all_transit = transit2018 %>% 
  filter(geography_group == 'Census2010Tract') %>% 
  select(geography, value) %>%
  mutate(geography = substr(geography,1,nchar(geography)-2)) %>% 
  rename(jobs = value) %>% 
  full_join(transit2014, by = c("geography"= "census_tract")) %>% 
  mutate(difference = jobs-HHaveraged_EMPTOT_P_45mins_transit, perc_dif = (jobs-HHaveraged_EMPTOT_P_45mins_transit)/(HHaveraged_EMPTOT_P_45mins_transit+1)*100  ) %>% 
  mutate(GEOID = geography)

all_transit%>% 
  summary()

# Comparison of distributions
transit2014_dens = transit2014 %>% 
  rename(jobs = HHaveraged_EMPTOT_P_45mins_transit, geography = census_tract) %>% 
  select(geography, jobs) %>% 
  mutate(year = as.factor("2014"))

transit2018_dens = transit2018 %>% 
  rename(jobs = value) %>%
  select(geography, jobs) %>% 
  mutate(year = as.factor("2018"))

transit_all_density <- rbind(transit2014_dens, transit2018_dens)

require(scales)
transit_all_density %>% ggplot(aes(jobs, fill = year))+
  geom_density(alpha=.2)+ 
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma) +
  ggtitle("Density: Number of jobs within 45 min of transit travel")

psrc_tracts <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(all_transit, by = "GEOID")

# Map difference
bins <- quantile(psrc_tracts$difference, probs = seq(0, 1, 0.2), na.rm = TRUE)
pal <- colorBin("PRGn", domain = psrc_tracts$difference, bins = bins)

m <- leaflet(psrc_tracts)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=psrc_tracts,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(psrc_tracts$difference),
              fillOpacity = 0.7,
              popup = paste("new to old difference: ", psrc_tracts$difference,"<br>",
                            "new (18BY) jobs: ", psrc_tracts$jobs,"<br>",
                            "old (14BY) jobs: ", psrc_tracts$HHaveraged_EMPTOT_P_45mins_transit,"<br>",
                            "% dif: ", psrc_tracts$perc_dif)) %>% 
  addLegend(pal = pal, values = psrc_tracts$difference, opacity = 0.7, title = "Difference between 2018 and 2014 ('18-'14) jobs access by transit within 45 min",
            position = "bottomright")
print(m)

# Jobs by transit data  ----------------------------------------------- 
data <- read_csv("./data/07-Access to Jobs/07_b_AccesstoJobs.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$NumJobsTransit, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$NumJobsTransit, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(NumJobsTransit)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of jobs within 45 min Transit travel time") +
  geom_vline(aes(xintercept = quantile(NumJobsTransit, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(NumJobsTransit, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(NumJobsTransit, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))
