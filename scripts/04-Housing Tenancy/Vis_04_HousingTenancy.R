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
tenant <- read_csv("./data/04-Housing Tenancy/04_HousingTenancy.csv")

# 2016 data
tenant_2016 <- read.csv("../Displacement_Risk_Script/data/004_HousingTenancy.csv")
tenant_2016 <- tenant_2016 %>% rename(prop_rent = "per_rent")
tenant_2016$per_rent <- tenant_2016$prop_rent * 100

# 2019 data
tenant_2019 <- read.csv("../Displacement Index 2021/data/04-Housing Tenancy/04_HousingTenancy.csv")

# Calculate quantiles
temp = as.data.frame(quantile(tenant$per_rent, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(tenant$per_rent, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
tenant %>% ggplot(aes(per_rent)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of households renting") +
  geom_vline(aes(xintercept = quantile(per_rent, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_rent, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_rent, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))


# Compare 2016, 2019, and 2024 distributions
mean_tenant_2016 = mean(tenant_2016$per_rent,na.rm = TRUE)
mean_tenant_2019 = mean(tenant_2019$per_rent,na.rm = TRUE)
mean_tenant_2024 = mean(tenant$per_rent,na.rm = TRUE)


# Bind data sets
tenant_2016 <- tenant_2016 %>% select(-starts_with("moe"), -prop_rent)
tenant_2016$year = as.factor(2016)
tenant_2019$year = as.factor(2019)
tenant$year = as.factor(2024)

tenant_all <- rbind(tenant_2016, tenant_2019, tenant)

tenant_all %>% ggplot(aes(per_rent,fill = year))+
  geom_density(alpha=.2)

tenant_all %>% ggplot(aes(per_rent, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_tenant_2016),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_tenant_2019),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_tenant_2024),
             color="royalblue", linetype="dashed", linewidth=1)


