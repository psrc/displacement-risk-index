### Descriptive and spatial analysis of the data ------------------------------------------------

# Libraries -----------------------------------------------
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

# Current data  ----------------------------------------------- 
data <- read_csv("./data/04-Housing Tenancy/04_HousingTenancy.csv")

# 2016 data
tenant_2016 <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/004_HousingTenancy.csv")
tenant_2016 <- tenant_2016 %>% rename(prop_rent = "per_rent")
tenant_2016$per_rent <- tenant_2016$prop_rent * 100

# Calculate quantiles
temp = as.data.frame(quantile(data$per_rent, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$per_rent, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(per_rent)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of households renting") +
  geom_vline(aes(xintercept = quantile(per_rent, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_rent, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_rent, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))


# Compare 2016 and 2019 distributions
mean_tenant_2016 = mean(tenant_2016$per_rent,na.rm = TRUE)
mean_tenant_2019 = mean(data$per_rent,na.rm = TRUE)

tenant_2016 <- tenant_2016 %>% select(-starts_with("moe"), -prop_rent)
data$year = as.factor(2019)
tenant_2016$year = as.factor(2016)

tenant_all <- rbind(tenant_2016, data)

tenant_all %>% ggplot(aes(per_rent,fill = year))+
  geom_density(alpha=.2)

tenant_all %>% ggplot(aes(per_rent, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_tenant_2016),
             color="salmon", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean_tenant_2019),
             color="cadetblue", linetype="dashed", size=1)


