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

# Proximity to school data  -----------------------------------------------
# Current data
school <- read.csv("./data/11-Proximity to Civic Infrastructure/11_a_ProximityCivicInfraSchool.csv")

# 2014 BY data
school_2014 <- read.csv("../Displacement_Risk_Script/data/011_2_ProximityCivicInfraSchool.csv")

# 2018 BY data
school_2018 <- read.csv("../Displacement Index 2021/data/11-Proximity to Civic Infrastructure/11_a_ProximityCivicInfraSchool.csv") 


# Calculate quantiles
temp = as.data.frame(quantile(school$school, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(school$school, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
school %>% ggplot(aes(school)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of weighted average distance to school (miles)") +
  geom_vline(aes(xintercept = quantile(school, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(school), color = "Mean")) +
  geom_vline(aes(xintercept = median(school, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2016, 2019, and 2024 distributions
mean_sch_2014 = mean(school_2014$school,na.rm = TRUE)
mean_sch_2018 = mean(school_2018$school,na.rm = TRUE)
mean_sch_2023 = mean(school$school,na.rm = TRUE)

# Bind data sets
school_2014 <- school_2014 %>% 
  rename(GEOID=GEOID10)
school_2014$year = as.factor(2014)
school_2018$year = as.factor(2018)
school <- school %>% 
  rename(GEOID=GEOID20)
school$year = as.factor(2023)

school_all <- rbind(school_2014, school_2018, school)

school_all %>% ggplot(aes(school,fill = year))+
  geom_density(alpha=.2)

school_all %>% ggplot(aes(school, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_sch_2014),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_sch_2018),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_sch_2023),
             color="royalblue", linetype="dashed", linewidth=1)


# Proximity to park data  -----------------------------------------------
# Current data
park <- read.csv("./data/11-Proximity to Civic Infrastructure/11_b_ProximityCivicInfraPark.csv")

# 2014 BY data
park_2014 <- read.csv("../Displacement_Risk_Script/data/011_1_ProximityCivicInfraPark.csv")

# 2018 BY data
park_2018 <- read.csv("../Displacement Index 2021/data/11-Proximity to Civic Infrastructure/11_b_ProximityCivicInfraPark.csv") 


# Calculate quantiles
temp = as.data.frame(quantile(park$parks, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(park$parks, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
park %>% ggplot(aes(parks)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of weighted average distance to parks (miles)") +
  geom_vline(aes(xintercept = quantile(parks, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(parks), color = "Mean")) +
  geom_vline(aes(xintercept = median(parks, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2016, 2019, and 2024 distributions
mean_park_2014 = mean(park_2014$parks,na.rm = TRUE)
mean_park_2018 = mean(park_2018$parks,na.rm = TRUE)
mean_park_2023 = mean(park$parks,na.rm = TRUE)

# Bind data sets
park_2014 <- park_2014 %>% 
  rename(GEOID=GEOID10)
park_2014$year = as.factor(2014)
park_2018 <- park_2018 %>% 
  rename(GEOID=GEOID10)
park_2018$year = as.factor(2018)
park <- park %>% 
  rename(GEOID=GEOID20)
park$year = as.factor(2023)

park_all <- rbind(park_2014, park_2018, park)

park_all %>% ggplot(aes(parks,fill = year))+
  geom_density(alpha=.2)

park_all %>% ggplot(aes(parks, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_sch_2014),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_sch_2018),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_sch_2023),
             color="royalblue", linetype="dashed", linewidth=1)
