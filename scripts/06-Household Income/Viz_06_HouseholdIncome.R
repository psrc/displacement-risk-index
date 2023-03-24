# install.packages("wesanderson") 
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(wesanderson)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Poverty data  ----------------------------------------------- 
data <- read_csv("./data/06-Household Income/06_HouseholdIncome.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$per_poverty, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$per_poverty, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(per_poverty)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of pop. whose income is below 200% of poverty level") +
  geom_vline(aes(xintercept = quantile(per_poverty, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_poverty, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_poverty, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))
