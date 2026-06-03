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

# Current data 
medrent <- read_csv("./data/14-Median Rent/14_MedianRent.csv")

# 2019 data
medrent_2019 <- read_csv("../Displacement Index 2021/data/14-Median Rent/14_MedianRent.csv")

# Re-run 6 times changing the number of rooms for each plot
# Calculate quantiles
temp = as.data.frame(quantile(medrent$ind_rent_0_rooms, probs = seq(0, 1, 0.2),na.rm = TRUE))
colnames(temp) <- "new"

# Variable distributions
medrent %>% ggplot(aes(ind_rent_0_rooms)) +
  geom_histogram(fill="royalblue3") +
  xlab("Ratio") + 
  ggtitle("Distribution of gross median rent ratio (0 bedrooms)") +
  geom_vline(aes(xintercept = quantile(ind_rent_0_rooms, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(ind_rent_0_rooms, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(ind_rent_0_rooms, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2019 and 2024 distributions
mean_medrent_2019 = mean(medrent_2019$ind_rent_0_rooms,na.rm = TRUE)
mean_medrent_2024 = mean(medrent$ind_rent_0_rooms,na.rm = TRUE)

# Bind data sets
medrent_2019$year = as.factor(2019)
medrent$year = as.factor(2024)

medrent_all <- rbind(medrent_2019, medrent)

medrent_all %>% ggplot(aes(ind_rent_0_rooms,fill = year))+
  geom_density(alpha=.2)

medrent_all %>% ggplot(aes(ind_rent_0_rooms, fill = year))+
  geom_density(alpha=.2)+
  # geom_vline(aes(xintercept=mean_tenant_2016),
  #            color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_medrent_2019),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_medrent_2024),
             color="cadetblue", linetype="dashed", linewidth=1)