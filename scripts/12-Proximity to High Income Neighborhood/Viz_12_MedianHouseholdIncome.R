library(tidyverse)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Cost-burdened data  ----------------------------------------------- 
data <- read_csv("./data/12-Proximity to High Income Neighborhood/12_ProximityToHighIncomeNeighborhood.csv")

data$neighbor <- factor(data$neighbor) %>% recode_factor(`0` = "No", `1` = "Yes")
data %>% ggplot(aes(x = neighbor)) +
  geom_bar(fill = "royalblue3") +
  xlab("Low income tract abutting high income tract") +
  ggtitle("Distribution of low income tracts (median household income <80% of AMI)\n
          that abut high income tracts (median household is >120% of AMI)") +
  ylab("Count")
