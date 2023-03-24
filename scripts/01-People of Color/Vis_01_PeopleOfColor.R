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
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Current data
race <- read.csv("./data/01-People of Color/01_peopleOfColor.csv")

# 2016 data
race_2016 <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/001_peopleOfColor.csv")
race_2016 <- race_2016 %>% rename(prop_notwhite = "per_notwhite")
race_2016$per_notwhite <- race_2016$prop_notwhite * 100

# Calculate quantiles
temp = as.data.frame(quantile(race$per_notwhite, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(race$per_notwhite, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
race %>% ggplot(aes(per_notwhite)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of pop. that is race other than non-hispanic white") +
  geom_vline(aes(xintercept = quantile(per_notwhite, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_notwhite, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_notwhite, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2016 and 2019 distributions
mean_race_2016 = mean(race_2016$per_notwhite,na.rm = TRUE)
mean_race_2019 = mean(race$per_notwhite,na.rm = TRUE)

# Bind datasets
race_2016 <- race_2016 %>% select(GEOID, est_total_race, est_nhispanic_white, per_notwhite)
race$year = as.factor(2019)
race_2016$year = as.factor(2016)
race_all <- rbind(race_2016, race)

race_all %>% ggplot(aes(per_notwhite,fill = year))+
  geom_density(alpha=.2)

race_all %>% ggplot(aes(per_notwhite, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_race_2016),
             color="salmon", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean_race_2019),
             color="cadetblue", linetype="dashed", size=1)


