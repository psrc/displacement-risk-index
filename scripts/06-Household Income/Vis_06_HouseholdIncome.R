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
poverty <- read_csv("./data/06-Household Income/06_HouseholdIncome.csv")

# 2016 data
poverty_2016 <- read.csv("../Displacement_Risk_Script/data/006_HouseholdIncome.csv")
poverty_2016$per_poverty <- poverty_2016$per_poverty * 100

# 2019 data
poverty_2019 <- read.csv("../Displacement Index 2021/data/06-Household Income/06_HouseholdIncome.csv")


# Calculate quantiles
temp = as.data.frame(quantile(poverty$per_poverty, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(poverty$per_poverty, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
poverty %>% ggplot(aes(per_poverty)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of pop. whose income is below 200% of poverty level") +
  geom_vline(aes(xintercept = quantile(per_poverty, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_poverty, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_poverty, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))


# Compare 2016, 2019, and 2024 distributions
mean_poverty_2016 = mean(poverty_2016$per_poverty,na.rm = TRUE)
mean_poverty_2019 = mean(poverty_2019$per_poverty,na.rm = TRUE)
mean_poverty_2024 = mean(poverty$per_poverty,na.rm = TRUE)


# Bind data sets
poverty_2016 <- poverty_2016 %>% 
  select(-starts_with("moe")) %>% 
  rename(est_pov=est_pop, #match more recent data
         est_200pov=est_pop_200pov) #match more recent data
poverty_2016$year = as.factor(2016)
poverty_2019$year = as.factor(2019)
poverty$year = as.factor(2024)

tenant_all <- rbind(poverty_2016, poverty_2019, poverty)

tenant_all %>% ggplot(aes(per_poverty,fill = year))+
  geom_density(alpha=.2)

tenant_all %>% ggplot(aes(per_poverty, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_poverty_2016),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_poverty_2019),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_poverty_2024),
             color="royalblue", linetype="dashed", linewidth=1)
