# 07 - access to number of jobs by 30 min car ride ------------------------------------------------
# Libraries -----------------------------------------------
# install.packages(tidyverse)
# install.packages(sf)
# install.packages(tigris)
# install.packages(leaflet)
library(tidyverse)
library(sf)
library(tigris)
library(leaflet)

# Compare 2018 auto data with 2014 ----------------------------------------------- 
# Load current and old data  ----------------------------------------------- 
jobA_18 <- read.csv("X:/Trans/2022 Regional Transportation Plan/Data Team/2018_base_year/displacement/auto_jobs_access.csv")

# Selecting tracts only
jobA_18_tracts = jobA_18 %>% filter(geography_group == 'Census2010Tract')
jobA_18_tracts$geography <- as.numeric(jobA_18_tracts$geography)
jobA_18_tracts$value <- round(jobA_18_tracts$value)

jobA_14 <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/007_1_JobsBy30minAuto.csv")
jobA_14$HHaveraged_EMPTOT_P_30mins_auto <- as.numeric(gsub(",", "", jobA_14$HHaveraged_EMPTOT_P_30mins_auto))
colnames(jobA_14) <- c("geography", "HH_P", "value")

# Re-shaping the data, joining both data and computing the difference
jobA_both <- jobA_14 %>% select(geography, value) %>%
                        rename(jobs_14 = value) %>%
                        full_join(jobA_18_tracts, by = "geography") %>% 
                        rename(jobs_18 = value) %>%
                        mutate(difference = jobs_18 - jobs_14, perc_dif = (jobs_18 - jobs_14)/jobs_14*100)
                            
jobA_both$GEOID <- as.character(jobA_both$geography)

jobA_both %>% 
  summary()

# Correlation
cor(jobA_18_tracts$value, jobA_14$value, method = c("pearson", "kendall", "spearman"), use = "complete.obs")

# Comparison of distributions
jobA_14_dens = jobA_14 %>% 
  select(geography, value) %>% 
  mutate(year = as.factor("2014_old"))

jobA_18_tracts_dens = jobA_18_tracts %>% 
  select(geography, value) %>% 
  mutate(year = as.factor("2018_upd"))

auto_all_density <- rbind(jobA_14_dens, jobA_18_tracts_dens)

require(scales)
auto_all_density %>% ggplot(aes(value, fill = year))+
  geom_density(alpha=.2)+ 
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma) +
  ggtitle("Density: Number of jobs within 30 min of auto travel")

# Map difference
psrc_tracts <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(jobA_both, by = "GEOID")

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
                            "new (18BY) jobs: ", psrc_tracts$jobs_18,"<br>",
                            "old (14BY) jobs: ", psrc_tracts$jobs_14,"<br>",
                            "% dif: ", psrc_tracts$perc_dif)) %>% 
  addLegend(pal = pal, values = psrc_tracts$difference, opacity = 0.7, title = "Difference between 2018 and 2014 ('18-'14) jobs access by car within 30 min",
            position = "bottomright")
print(m)

# Plot distribution, quantiles, mean, median (for manual)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Jobs by auto data  ----------------------------------------------- 
data <- read_csv("./data/07-Access to Jobs/07_a_AccesstoJobs.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$NumJobsAuto, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$NumJobsAuto, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(NumJobsAuto)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of jobs within 30min auto travel time") +
  geom_vline(aes(xintercept = quantile(NumJobsAuto, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(NumJobsAuto, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(NumJobsAuto, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

