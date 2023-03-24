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
data <- read_csv("./data/03-Education Attainment/03_EducationalAttainment.csv")

# 2016 data
ed_2016 <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/003_EducationalAttainment.csv")
ed_2016 <- ed_2016 %>% rename(prop_nobachelor = "per_nobachelor")
ed_2016$per_nobachelor <- ed_2016$prop_nobachelor * 100

# Calculate quantiles
temp = as.data.frame(quantile(data$per_nobachelor, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$per_nobachelor, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(per_nobachelor)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of pop. >25yr without a bachelor's degree") +
  geom_vline(aes(xintercept = quantile(per_nobachelor, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_nobachelor, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_nobachelor, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))


# Compare 2016 and 2019 distributions
mean_ed_2016 = mean(ed_2016$per_nobachelor,na.rm = TRUE)
mean_ed_2019 = mean(data$per_nobachelor,na.rm = TRUE)

ed_2016 <- ed_2016 %>% select(-starts_with("moe"), -prop_nobachelor)
data$year = as.factor(2019)
ed_2016$year = as.factor(2016)

ed_all <- rbind(ed_2016, data)

ed_all %>% ggplot(aes(per_nobachelor,fill = year))+
  geom_density(alpha=.2)

ed_all %>% ggplot(aes(per_nobachelor, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_ed_2016),
             color="salmon", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean_ed_2019),
             color="cadetblue", linetype="dashed", size=1)

