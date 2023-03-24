library(tidyverse)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Cost-burdened data  ----------------------------------------------- 
data <- read_csv("./data/14-Median Rent/14_MedianRent.csv")

# Re-run 6 times changing the number of rooms for each plot
# Calculate quantiles
temp = as.data.frame(quantile(data$ind_rent_5_rooms, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$ind_rent_0_rooms, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(ind_rent_5_rooms)) +
  geom_histogram(fill="royalblue3") +
  xlab("Ratio") + 
  ggtitle("Distribution of gross median rent ratio (5 bedrooms)") +
  geom_vline(aes(xintercept = quantile(ind_rent_5_rooms, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(ind_rent_5_rooms, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(ind_rent_5_rooms, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))
