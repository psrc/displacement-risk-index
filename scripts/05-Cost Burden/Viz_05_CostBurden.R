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

# Cost-burdened data  ----------------------------------------------- 
data <- read_csv("./data/05-Cost Burdened Households/05_a_CostBurdenHousehold.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$per_burden, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$per_burden, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(per_burden)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of households that are cost-burdened") +
  geom_vline(aes(xintercept = quantile(per_burden, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_burden, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_burden, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Severely cost-burdened data  ----------------------------------------------- 
data <- read_csv("./data/05-Cost Burdened Households/05_b_SevereCostBurdenHousehold.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$per_sev_burden, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$per_sev_burden, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(per_sev_burden)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of households that are severely cost-burdened") +
  geom_vline(aes(xintercept = quantile(per_sev_burden, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_sev_burden, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_sev_burden, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))
