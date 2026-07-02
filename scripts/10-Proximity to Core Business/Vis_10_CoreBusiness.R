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
supermarket_data <- read_csv("./data/10-Proximity to Core Business/10_a_ProximityCoreBusinessSupermarket.csv")
pharmacy_data <- read_csv("./data/10-Proximity to Core Business/10_b_ProximityCoreBusinessPharmacy.csv")
restaurant_data <- read_csv("./data/10-Proximity to Core Business/10_c_ProximityCoreBusinessRestaurant.csv")

# 2018 data
all_data_2018 <- read.csv("../Displacement_Risk_Script/data/010_ProximityCoreBusiness.csv")

supermarket_data_2018<- all_data_2018 %>% 
  select(GEOID10, supermarket)
pharmacy_data_2018 <- all_data_2018 %>% 
  select(GEOID10, pharmacy)
restaurant_data_2018 <- all_data_2018 %>% 
  select(GEOID10, restaurant)

# 2021 data
supermarket_data_2021 <- read_csv("../Displacement Index 2021/data/10-Proximity to Core Business/10_a_ProximityCoreBusinessSupermarket.csv")
pharmacy_data_2021 <- read_csv("../Displacement Index 2021/data/10-Proximity to Core Business/10_b_ProximityCoreBusinessPharmacy.csv")
restaurant_data_2021 <- read_csv("../Displacement Index 2021/data/10-Proximity to Core Business/10_c_ProximityCoreBusinessRestaurant.csv")



# Proximity to supermarket -----------------------------------------------

# Calculate quantiles
temp = as.data.frame(quantile(supermarket_data$supermarket, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$supermarket, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
supermarket_data %>% ggplot(aes(supermarket)) +
  geom_histogram(fill="royalblue3") +
  xlab("Distance (miles)") + 
  ggtitle("Distribution of weighted average distance to supermarket/grocery (miles)") +
  geom_vline(aes(xintercept = quantile(supermarket, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(supermarket, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(supermarket, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2016, 2021, and 2024 distributions
mean_supermarket_2018 = mean(supermarket_data_2018$supermarket,na.rm = TRUE)
mean_supermarket_2021 = mean(supermarket_data_2021$supermarket,na.rm = TRUE)
mean_supermarket_2024 = mean(supermarket_data$supermarket,na.rm = TRUE)

# Bind data sets
supermarket_data_2018 <- supermarket_data_2018 %>% 
  rename(GEOID=GEOID10)
supermarket_data_2018$year = as.factor(2018)
supermarket_data_2021 <- supermarket_data_2021 %>% 
  rename(GEOID=GEOID10)
supermarket_data_2021$year = as.factor(2021)
supermarket_data$year = as.factor(2024)

supermarket_all <- rbind(supermarket_data_2018, supermarket_data_2021, supermarket_data)

supermarket_all %>% ggplot(aes(supermarket, fill = year))+
  geom_density(alpha=.2)

supermarket_all %>% ggplot(aes(supermarket, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_supermarket_2018),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_supermarket_2021),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_supermarket_2024),
             color="royalblue", linetype="dashed", linewidth=1)


# Proximity to pharmacy -----------------------------------------------

# Calculate quantiles
temp = as.data.frame(quantile(pharmacy_data$pharmacy, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$pharmacy, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
pharmacy_data %>% ggplot(aes(pharmacy)) +
  geom_histogram(fill="royalblue3") +
  xlab("Distance (miles)") + 
  ggtitle("Distribution of weighted average distance to pharmacy (miles)") +
  geom_vline(aes(xintercept = quantile(pharmacy, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(pharmacy, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(pharmacy, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2021 and 2024 distributions
mean_pharmacy_2018 = mean(pharmacy_data_2018$pharmacy,na.rm = TRUE)
mean_pharmacy_2021 = mean(pharmacy_data_2021$pharmacy,na.rm = TRUE)
mean_pharmacy_2024 = mean(pharmacy_data$pharmacy,na.rm = TRUE)

# Bind data sets
pharmacy_data_2018 <- pharmacy_data_2018 %>% 
  rename(GEOID=GEOID10)
pharmacy_data_2018$year = as.factor(2018)
pharmacy_data_2021 <- pharmacy_data_2021 %>% 
  rename(GEOID=GEOID10)
pharmacy_data_2021$year = as.factor(2021)
pharmacy_data$year = as.factor(2024)

pharmacy_all <- rbind(pharmacy_data_2018, pharmacy_data_2021, pharmacy_data)

pharmacy_all %>% ggplot(aes(pharmacy, fill = year))+
  geom_density(alpha=.2)

pharmacy_all %>% ggplot(aes(pharmacy, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_pharmacy_2018),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_pharmacy_2021),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_pharmacy_2024),
             color="royalblue", linetype="dashed", linewidth=1)


# Proximity to restaurant -----------------------------------------------

# Calculate quantiles
temp = as.data.frame(quantile(restaurant_data$restaurant, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$restaurant, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
restaurant_data %>% ggplot(aes(restaurant)) +
  geom_histogram(fill="royalblue3") +
  xlab("Distance (miles)") + 
  ggtitle("Distribution of weighted average distance to restaurant (miles)") +
  geom_vline(aes(xintercept = quantile(restaurant, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(restaurant, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(restaurant, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2021 and 2024 distributions
mean_restaurant_2018 = mean(restaurant_data_2018$restaurant,na.rm = TRUE)
mean_restaurant_2021 = mean(restaurant_data_2021$restaurant,na.rm = TRUE)
mean_restaurant_2024 = mean(restaurant_data$restaurant,na.rm = TRUE)

# Bind data sets
restaurant_data_2018 <- restaurant_data_2018 %>% 
  rename(GEOID=GEOID10)
restaurant_data_2018$year = as.factor(2018)
restaurant_data_2021 <- restaurant_data_2021 %>% 
  rename(GEOID=GEOID10)
restaurant_data_2021$year = as.factor(2021)
restaurant_data$year = as.factor(2024)

restaurant_all <- rbind(restaurant_data_2018, restaurant_data_2021, restaurant_data)

restaurant_all %>% ggplot(aes(restaurant, fill = year))+
  geom_density(alpha=.2)

restaurant_all %>% ggplot(aes(restaurant, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_restaurant_2018),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_restaurant_2021),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_restaurant_2024),
             color="royalblue", linetype="dashed", linewidth=1)
