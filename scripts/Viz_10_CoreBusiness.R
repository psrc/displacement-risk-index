library(tidyverse)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Proximity to supermarket data  ----------------------------------------------- 
data <- read_csv("./data/10-Proximity to Core Business/10_a_ProximityCoreBusinessSupermarket.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$supermarket, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$supermarket, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(supermarket)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of weighted average distance to supermarket/grocery (miles)") +
  geom_vline(aes(xintercept = quantile(supermarket, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(supermarket, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(supermarket, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Proximity to  pharmacy data  ----------------------------------------------- 
data <- read_csv("./data/10-Proximity to Core Business/10_b_ProximityCoreBusinessPharmacy.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$pharmacy, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$pharmacy, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(pharmacy)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of weighted average distance to pharmacy (miles)") +
  geom_vline(aes(xintercept = quantile(pharmacy, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(pharmacy, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(pharmacy, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Proximity to restaurant data  ----------------------------------------------- 
data <- read_csv("./data/10-Proximity to Core Business/10_c_ProximityCoreBusinessRestaurant.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$restaurant, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$restaurant, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(restaurant)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of weighted average distance to restaurant (miles)") +
  geom_vline(aes(xintercept = quantile(restaurant, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(restaurant, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(restaurant, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))
