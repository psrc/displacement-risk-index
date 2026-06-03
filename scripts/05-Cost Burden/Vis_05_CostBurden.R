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

# Cost-burdened data  -----------------------------------------------
# Current data
cost_burden <- read.csv("./data/05-Cost Burdened Households/05_a_CostBurdenHousehold.csv")

# 2016 data
cost_burden_2016 <- read.csv("../Displacement_Risk_Script/data/005_1_CostBurdenHousehold.csv")
cost_burden_2016$per_burden <- cost_burden_2016$per_burden * 100

# 2019 data
cost_burden_2019 <- read.csv("../Displacement Index 2021/data/05-Cost Burdened Households/05_a_CostBurdenHousehold.csv") 


# Calculate quantiles
temp = as.data.frame(quantile(cost_burden$per_burden, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$per_burden, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
cost_burden %>% ggplot(aes(per_burden)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of households that are cost-burdened") +
  geom_vline(aes(xintercept = quantile(per_burden, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_burden, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_burden, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2016, 2019, and 2024 distributions
mean_cb_2016 = mean(cost_burden_2016$per_burden,na.rm = TRUE)
mean_cb_2019 = mean(cost_burden_2019$per_burden,na.rm = TRUE)
mean_cb_2024 = mean(cost_burden$per_burden,na.rm = TRUE)


# Bind data sets
cost_burden_2016 <- cost_burden_2016 %>% 
  rename(total=T7_est1) %>%
  select(-starts_with("T7_est"))
cost_burden_2016$year = as.factor(2016)
cost_burden_2019$year = as.factor(2019)
cost_burden$year = as.factor(2024)

cost_burden_all <- rbind(cost_burden_2016, cost_burden_2019, cost_burden)

cost_burden_all %>% ggplot(aes(per_burden,fill = year))+
  geom_density(alpha=.2)

cost_burden_all %>% ggplot(aes(per_burden, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_cb_2016),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_cb_2019),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_cb_2024),
             color="royalblue", linetype="dashed", linewidth=1)



# Severely cost-burdened data  ----------------------------------------------- 
sev_cost_burden <- read_csv("./data/05-Cost Burdened Households/05_b_SevereCostBurdenHousehold.csv")

# 2016 data
sev_cost_burden_2016 <- read.csv("../Displacement_Risk_Script/data/005_2_SevereCostBurdenHousehold.csv")
sev_cost_burden_2016$per_sev_burden <- sev_cost_burden_2016$per_sev_burden * 100

# 2019 data
sev_cost_burden_2019 <- read.csv("../Displacement Index 2021/data/05-Cost Burdened Households/05_b_SevereCostBurdenHousehold.csv") 

# Calculate quantiles
temp = as.data.frame(quantile(sev_cost_burden$per_sev_burden, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$per_sev_burden, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
sev_cost_burden %>% ggplot(aes(per_sev_burden)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of % of households that are severely cost-burdened") +
  geom_vline(aes(xintercept = quantile(per_sev_burden, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_sev_burden, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_sev_burden, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Compare 2016, 2019, and 2024 distributions
mean_scb_2016 = mean(sev_cost_burden_2016$per_sev_burden,na.rm = TRUE)
mean_scb_2019 = mean(sev_cost_burden_2019$per_sev_burden,na.rm = TRUE)
mean_scb_2024 = mean(sev_cost_burden$per_sev_burden,na.rm = TRUE)


# Bind data sets
sev_cost_burden_2016 <- sev_cost_burden_2016 %>% 
  rename(total=T7_est1) %>%
  select(-starts_with("T7_est"))
sev_cost_burden_2016$year = as.factor(2016)
sev_cost_burden_2019$year = as.factor(2019)
sev_cost_burden$year = as.factor(2024)

cost_burden_all <- rbind(sev_cost_burden_2016, sev_cost_burden_2019, sev_cost_burden)

cost_burden_all %>% ggplot(aes(per_sev_burden,fill = year))+
  geom_density(alpha=.2)

cost_burden_all %>% ggplot(aes(per_sev_burden, fill = year))+
  geom_density(alpha=.2)+
  geom_vline(aes(xintercept=mean_scb_2016),
             color="salmon", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_scb_2019),
             color="cadetblue", linetype="dashed", linewidth=1)+
  geom_vline(aes(xintercept=mean_scb_2024),
             color="royalblue", linetype="dashed", linewidth=1)
