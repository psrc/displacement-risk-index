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
proximity <- read_csv("./data/12-Proximity to High Income Neighborhood/12_ProximityToHighIncomeNeighborhood.csv")

# 2019 data
proximity_2019 <- read_csv("../Displacement Index 2021/data/12-Proximity to High Income Neighborhood/12_ProximityToHighIncomeNeighborhood.csv")

# Calculate quantiles
temp = as.data.frame(quantile(proximity$per_mfi, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(proximity$per_mfi, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

proximity %>% ggplot(aes(per_mfi)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of tract median household income to county median") +
  geom_vline(aes(xintercept = quantile(per_mfi, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_mfi, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_mfi, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))


# Compare 2019, and 2024 distributions
mean_proximity_2019 = mean(proximity_2019$per_mfi,na.rm = TRUE)
mean_proximity_2024 = mean(proximity$per_mfi,na.rm = TRUE)

# Bind data sets
proximity_2019$year = as.factor(2019)
proximity$year = as.factor(2024)

proximity_all <- rbind(proximity_2019, proximity)
  
proximity_all %>% ggplot(aes(per_mfi, fill = year))+
  geom_density(alpha=.2)

proximity_all %>% ggplot(aes(per_mfi, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_proximity_2019),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_proximity_2024),
             color="cadetblue", linetype="dashed", linewidth=1)


# Visualize distribution (yes/no)
proximity_all$neighbor <- factor(proximity_all$neighbor) %>% 
  recode_factor(`0` = "No", `1` = "Yes")

table(proximity_all$neighbor, proximity_all$year)

proximity_all %>% data.frame() %>% 
  group_by(year, neighbor) %>% 
  mutate(count = n()) %>% 
  ggplot(aes(x=neighbor, y=count, fill=year, label=count)) +
  geom_col(position="dodge") +
  geom_text(position=position_dodge(width=0.9), vjust = -0.3) +
  labs(title = "Proximity to high income neighborhood",
       subtitle = "low income (tract median household income <80% of AMI)\nhigh income (tract median household income >120% of AMI)") +
  theme(plot.subtitle=element_text(size=8, face='italic', color='gray'))
  xlab("Low income tract abutting high income tract") +
  ylab("Count of census tracts")
