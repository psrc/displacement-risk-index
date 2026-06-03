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
ed <- read_csv("./data/03-Education Attainment/03_EducationalAttainment.csv")

# 2016 data
ed_2016 <- read.csv("../Displacement_Risk_Script/data/003_EducationalAttainment.csv")
ed_2016 <- ed_2016 %>% rename(prop_nobachelor = "per_nobachelor")
ed_2016$per_nobachelor <- ed_2016$prop_nobachelor * 100

# 2019 data
ed_2019 <- read.csv("../Displacement Index 2021/data/03-Education Attainment/03_EducationalAttainment.csv")

# Calculate quantiles
temp = as.data.frame(quantile(ed$per_nobachelor, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(ed$per_nobachelor, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
ed %>% ggplot(aes(per_nobachelor)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of pop. >25yr without a bachelor's degree") +
  geom_vline(aes(xintercept = quantile(per_nobachelor, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_nobachelor, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_nobachelor, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))


# Compare 2016, 2019, and 2024 distributions
mean_ed_2016 = mean(ed_2016$per_nobachelor,na.rm = TRUE)
mean_ed_2019 = mean(ed_2019$per_nobachelor,na.rm = TRUE)
mean_ed_2024 = mean(ed$per_nobachelor,na.rm = TRUE)

# Bind data sets
ed_2016 <- ed_2016 %>% select(-starts_with("moe"), -prop_nobachelor)
ed_2016 <- ed_2016 %>% 
  mutate(est_bachelor_or_higher=sum(est_bachelor, est_master,
                                    est_profesional, est_phd)) %>% 
  select(-c(est_bachelor, est_master,
            est_profesional, est_phd))
ed_2016$year = as.factor(2016)
ed_2019 <- ed_2019 %>% 
  mutate(est_bachelor_or_higher=sum(est_bachelor, est_master,
                                    est_profesional, est_phd)) %>% 
  select(-c(est_bachelor, est_master,
            est_profesional, est_phd))
ed_2019$year = as.factor(2019)
ed$year = as.factor(2024)

ed_all <- rbind(ed_2016, ed_2019, ed)

ed_all %>% ggplot(aes(per_nobachelor,fill = year))+
  geom_density(alpha=.2)

ed_all %>% ggplot(aes(per_nobachelor, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_ed_2016),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_ed_2019),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_ed_2024),
             color="royalblue", linetype="dashed", linewidth=1)

