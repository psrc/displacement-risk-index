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
data <- read_csv("./data/02-Linguistic Isolation/02_LinguisticIsolation.csv")

# 2016 data
ling_2016 <- read.csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/002_LinguisticIsolation.csv")
ling_2016 <- ling_2016 %>% rename(`est_total_total_>5` = "est_total_total_.5", 
                                  `moe_total_total_>5` = "moe_total_total_.5",
                                  prop_noenglish = "per_noenglish")
ling_2016$per_noenglish <- ling_2016$prop_noenglish * 100

# Calculate quantiles
temp = as.data.frame(quantile(data$per_noenglish, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$per_noenglish, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(per_noenglish)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of pop. >5yr that speaks English less than very well") +
  geom_vline(aes(xintercept = quantile(per_noenglish, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_noenglish, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_noenglish, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2016 and 2019 distributions
mean_ling_2016 = mean(ling_2016$per_noenglish,na.rm = TRUE)
mean_ling_2019 = mean(data$per_noenglish,na.rm = TRUE)

data$year = as.factor(2019)
ling_2016$year = as.factor(2016)
ling_2016 <- ling_2016 %>% select(-starts_with("moe"), -prop_noenglish)

ling_all <- rbind(ling_2016, data)

ling_all %>% ggplot(aes(per_noenglish,fill = year))+
  geom_density(alpha=.2)

ling_all %>% ggplot(aes(per_noenglish, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_ling_2016),
             color="salmon", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean_ling_2019),
             color="cadetblue", linetype="dashed", size=1)


