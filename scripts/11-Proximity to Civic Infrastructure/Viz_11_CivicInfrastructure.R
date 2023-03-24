library(tidyverse)

# Proximity to school data  ----------------------------------------------- 
data <- read_csv("./data/11-Proximity to Civic Infrastructure/11_a_ProximityCivicInfraSchool.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$school, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$school, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(school)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of weighted average distance to school (miles)") +
  geom_vline(aes(xintercept = quantile(school, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(school, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(school, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Proximity to parks data  ----------------------------------------------- 
data <- read_csv("./data/11-Proximity to Civic Infrastructure/11_b_ProximityCivicInfraPark.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$parks, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$parks, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(parks)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of weighted average distance to parks (miles)") +
  geom_vline(aes(xintercept = quantile(parks, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(parks, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(parks, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))
