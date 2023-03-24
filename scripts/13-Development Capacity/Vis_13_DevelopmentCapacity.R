# 13 - Development Capacity

# Descriptive and spatial analysis of the data ------------------------------------------------
# 2014 base year vs 2018 base year

# Libraries -----------------------------------------------
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("wesanderson")
# install.packages("tigris")
library(tidyverse)
library(sf)
library(leaflet)
library(wesanderson)
library(tigris)

# Working directory
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021")

# Development capacity data  ----------------------------------------------- 
data <- read_csv("./data/13-Development Capacity/13_DevelopmentCapacity.csv")

# Calculate quantiles
temp = as.data.frame(quantile(data$per_at_risk_2018by, probs = seq(0, 1, 0.2),na.rm = TRUE))
# temp$new = unlist(temp$`quantile(data$per_at_risk_2018by, probs = seq(0, 1, 0.2), na.rm = TRUE)`)
colnames(temp) <- "new"

# Variable distributions
data %>% ggplot(aes(per_at_risk_2018by)) +
  geom_histogram(fill="royalblue3") +
  xlab("Percent (%)") + 
  ggtitle("Distribution of residential development potential") +
  geom_vline(aes(xintercept = quantile(per_at_risk_2018by, 0.2, na.rm = TRUE), color = "Quintiles")) +
  geom_vline(xintercept = temp$new, colour="black") +
  geom_vline(aes(xintercept = mean(per_at_risk_2018by, na.rm = TRUE), color = "Mean")) +
  geom_vline(aes(xintercept = median(per_at_risk_2018by, na.rm = TRUE), color = "Median")) +
  scale_color_manual(name = "Statistics", 
                     values = c("Quintiles" = "black", "Mean" = "red", "Median" = "orange"))

# Load data  ----------------------------------------------- 
disp_risk_2014by <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/013_DevelopmentCapacity.csv")

disp_risk_2018by <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/13-Development Capacity/2018by_upd_meth/hh_at_displacement_risk-2021-11-29/hh_at_displacement_risk-2021-11-29.csv")

disp_risk_2014by_upd_meth <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/13-Development Capacity/2014by_upd_meth/hh_at_displacement_risk-BY2014-2021-12-13.csv")

# transform 2018by data to match 2014by data

disp_risk_2018by_upd = disp_risk_2018by %>%
                      select(census_tract_id,hh_at_risk,hh_total) %>%
                      group_by(census_tract_id) %>% 
                      summarise(hh_at_risk_2018by = sum(hh_at_risk), hh_total_2018by = sum(hh_total),
                                per_at_risk_2018by = as.double(hh_at_risk_2018by/hh_total_2018by)*100) 

# 2018 updated methodology summaries
disp_risk_2018by_region = disp_risk_2018by %>%
  select(county_id,hh_at_risk,hh_total) %>%
  group_by(county_id) %>% 
  summarise(hh_at_risk_2018by = sum(hh_at_risk), hh_total_2018by = sum(hh_total),
            per_at_risk_2018by = as.double(hh_at_risk_2018by/hh_total_2018by)*100) 



# transform 2014by(updated methodology) data to match 2014by data

disp_risk_2014by_upd_meth = disp_risk_2014by_upd_meth %>%
  select(census_tract_id,hh_at_risk,hh_total) %>%
  group_by(census_tract_id) %>% 
  summarise(hh_at_risk_2014by_upd_meth = sum(hh_at_risk), hh_total_2014by_upd_meth = sum(hh_total),
            per_at_risk_2014by_upd_meth = as.double(hh_at_risk_2014by_upd_meth/hh_total_2014by_upd_meth)*100) 

disp_risk_2014by_upd_meth_region = disp_risk_2014by_upd_meth %>%
  select(county_id,hh_at_risk,hh_total) %>%
  group_by(county_id) %>% 
  summarise(hh_at_risk_2014by_upd = sum(hh_at_risk), hh_total_2014by_upd = sum(hh_total),
            per_at_risk_2014by_upd = as.double(hh_at_risk_2014by_upd/hh_total_2014by_upd)*100)

# transform 2014by data
disp_risk_2014by_upd = disp_risk_2014by %>% 
  mutate(at_risk_2014by = baseyear_du - persists_du, 
         per_atrisk_2014by = at_risk_2014by / baseyear_du*100)

disp_risk_2014by_old_meth_region = disp_risk_2014by %>% 
                                  mutate(county = substr(geoid10, 3, 5)) %>% 
                                  group_by(county) %>% 
                                  summarise(baseyear_du = sum(baseyear_du),persists_du = sum(persists_du) ) %>% 
  mutate(at_risk_2014by = baseyear_du - persists_du, 
         per_atrisk_2014by = at_risk_2014by / baseyear_du*100, county_id = as.numeric(county)) %>% 
  select(county_id, baseyear_du, at_risk_2014by, per_atrisk_2014by)


# region summary 2014by old, 2014by new, 2018 by

all_region_summary = disp_risk_2014by_old_meth_region %>% 
                      left_join(disp_risk_2014by_upd_meth_region, by = ("county_id")) %>% 
                      left_join(disp_risk_2018by_region, by =("county_id") ) %>% 
                      mutate(county = case_when(county_id == 33 ~ "King County",
                                                county_id == 35 ~ "Kitsap County",
                                                county_id == 53 ~ "Pierce County",
                                                county_id == 61 ~ "Snohomish County"),
                             hh_total_2014by_old = baseyear_du) %>% 
  
  select(county,hh_total_2014by_old,at_risk_2014by, 
         hh_at_risk_2014by_upd,hh_total_2014by_upd,
         hh_at_risk_2018by, hh_total_2018by,
         per_atrisk_2014by,per_at_risk_2014by_upd,per_at_risk_2018by) %>% 
  add_row(county = "Region", 
          at_risk_2014by =sum(disp_risk_2014by_old_meth_region$at_risk_2014by),
          hh_total_2014by_old = sum(disp_risk_2014by_old_meth_region$baseyear_du), 
          hh_at_risk_2014by_upd = sum(disp_risk_2014by_upd_meth_region$hh_at_risk_2014by_upd) ,
          hh_total_2014by_upd = sum(disp_risk_2014by_upd_meth_region$hh_total_2014by_upd),
          hh_at_risk_2018by = sum(disp_risk_2018by_region$hh_at_risk_2018by), 
          hh_total_2018by = sum(disp_risk_2018by_region$hh_total_2018by),
          per_atrisk_2014by=sum(disp_risk_2014by_old_meth_region$at_risk_2014by)/sum(disp_risk_2014by_old_meth_region$baseyear_du)*100,
          per_at_risk_2014by_upd = sum(disp_risk_2014by_upd_meth_region$hh_at_risk_2014by_upd)/sum(disp_risk_2014by_upd_meth_region$hh_total_2014by_upd)*100,
          per_at_risk_2018by = sum(disp_risk_2018by_region$hh_at_risk_2018by)/sum(disp_risk_2018by_region$hh_total_2018by)*100)


# 2014by and 2018by both with updated methodology  ----------------------------------------------- 
                  
# join 2018by and 2018 data
both_years_disp_risk = disp_risk_2014by_upd %>% 
                        left_join(disp_risk_2018by_upd, by = c("census_tract_id"="census_tract_id")) %>% 
                        mutate(per_dif = per_at_risk_2018by - per_atrisk_2018, 
                               abs_dif = hh_at_risk_2018by - atrisk_2018, 
                               geoid10 = as.character(geoid10) )

# join 2018by and 2014 data
both_years_disp_risk_21_14_upd_meth = disp_risk_2014_upd %>% 
  left_join(disp_risk_2018by_upd, by = c("census_tract_id"="census_tract_id")) %>% 
  mutate(per_dif = per_at_risk_2018by - per_at_risk_2014, 
         abs_dif = hh_at_risk_2018by - hh_at_risk_2014 )  

#join 2018 data to get geoid
geoid_info = disp_risk_2014by %>% 
              select(census_tract_id, geoid10)

both_years_disp_risk_21_14_upd_meth = both_years_disp_risk_21_14_upd_meth %>% 
                                      left_join(geoid_info, by = c("census_tract_id"="census_tract_id")) %>% 
                                      mutate(geoid10 = as.character(geoid10) )


psrc_tracts_dc <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(both_years_disp_risk_21_14_upd_meth, by = c("GEOID"= "geoid10"))

# map 2018by , %

#join 2018 data to get geoid

disp_risk_2018by_upd_geo = disp_risk_2018by_upd %>% 
  left_join(geoid_info, by = c("census_tract_id"="census_tract_id")) %>% 
  mutate(geoid10 = as.character(geoid10) )

psrc_tracts_dr_2018 <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(disp_risk_2018by_upd_geo, by = c("GEOID"= "geoid10"))


bins <- c(0,1,5,10,20,30, max(psrc_tracts_dr_2018$per_at_risk_2018by,na.rm = TRUE))
pal <- colorBin("YlGnBu", domain = psrc_tracts_dr_2018$per_at_risk_2018by, bins = bins)


m <- leaflet(psrc_tracts_dr_2018)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=psrc_tracts_dr_2018,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(psrc_tracts_dr_2018$per_at_risk_2018by),
              fillOpacity = 0.7,
              popup = paste("% of hh at risk in 2018by: ", round(psrc_tracts_dr_2018$per_at_risk_2018by,2),"<br>",
                            "Num of hh at risk in 2018by: ", psrc_tracts_dr_2018$hh_at_risk_2018by,"(",psrc_tracts_dr_2018$hh_total_2018by,")", "<br>",
                            
                            "tract", psrc_tracts_dr_2018$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = psrc_tracts_dr_2018$per_at_risk_2018by, opacity = 0.7, title = "% of household at-risk of diaplacement, 2018 base year, upd methodology",
            position = "bottomright")
print(m)


# map difference perc, %
bins <- c(min(psrc_tracts_dc$per_dif,na.rm = TRUE),-15,-10,-5,-1, 0,10,20, max(psrc_tracts_dc$per_dif,na.rm = TRUE))
pal <- colorBin("PRGn", domain = psrc_tracts_dc$per_dif, bins = bins)


m <- leaflet(psrc_tracts_dc)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=psrc_tracts_dc,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(psrc_tracts_dc$per_dif),
              fillOpacity = 0.7,
              popup = paste("new to old difference, %: ", round(psrc_tracts_dc$per_dif,2),"<br>",
                            "new to old difference, abs: ", psrc_tracts_dc$abs_dif,"<br>",
                            "Num of hh at risk in 2014: ", psrc_tracts_dc$hh_at_risk_2014, "(",psrc_tracts_dc$hh_total_2014,")", "<br>",
                            "Num of hh at risk in 2018by: ", psrc_tracts_dc$hh_at_risk_2018by,"(",psrc_tracts_dc$hh_total_2018by,")", "<br>",
                            "% of hh at risk in 2014: ", round(psrc_tracts_dc$per_at_risk_2014,2),"<br>",
                            "% of hh at risk in 2018by: ", round(psrc_tracts_dc$per_at_risk_2018by,2),"<br>",
                            "tract", psrc_tracts_dc$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = psrc_tracts_dc$per_dif, opacity = 0.7, title = "% Difference between 2018by and 2014 households at risk,upd methodology",
            position = "bottomright")
print(m)

# map difference perc, abs
bins <- c(min(psrc_tracts_dc$abs_dif,na.rm = TRUE),-200,-100,-10, 0,2,15,100,200, max(psrc_tracts_dc$abs_dif,na.rm = TRUE))
pal <- colorBin("PRGn", domain = psrc_tracts_dc$abs_dif, bins = bins)


m <- leaflet(psrc_tracts_dc)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=psrc_tracts_dc,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(psrc_tracts_dc$abs_dif),
              fillOpacity = 0.7,
              popup = paste("new to old difference, %: ", round(psrc_tracts_dc$per_dif,2),"<br>",
                            "new to old difference, abs: ", psrc_tracts_dc$abs_dif,"<br>",
                            "Num of hh at risk in 2014: ", psrc_tracts_dc$hh_at_risk_2014, "(",psrc_tracts_dc$hh_total_2014,")", "<br>",
                            "Num of hh at risk in 2018by: ", psrc_tracts_dc$hh_at_risk_2018by,"(",psrc_tracts_dc$hh_total_2018by,")", "<br>",
                            "% of hh at risk in 2014: ", round(psrc_tracts_dc$per_at_risk_2014,2),"<br>",
                            "% of hh at risk in 2018by: ", round(psrc_tracts_dc$per_at_risk_2018by,2),"<br>",
                            "tract", psrc_tracts_dc$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = psrc_tracts_dc$abs_dif, opacity = 0.7, title = "Absolute difference between 2018by and 2014 households at risk, upd methodology",
            position = "bottomright")
print(m)

# 2014by old methodology and 2014by updated methodology  ----------------------------------------------- 

# join 2014by old methodology and 2014by updated methodology
both_years_disp_risk = disp_risk_2014by_upd %>% 
  left_join(disp_risk_2014by_upd_meth, by = c("census_tract_id"="census_tract_id")) %>% 
  mutate(per_dif = per_at_risk_2014by_upd_meth - per_atrisk_2014by, 
         abs_dif = hh_at_risk_2014by_upd_meth - at_risk_2014by, 
         geoid10 = as.character(geoid10) )

psrc_tracts_dc <- tracts("WA", county = c(033,035,053,061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>% 
  left_join(both_years_disp_risk, by = c("GEOID"= "geoid10"))


# map difference perc, %
bins <- c(min(psrc_tracts_dc$per_dif,na.rm = TRUE),-10,-5,-1, 0,1,5,10,15, max(psrc_tracts_dc$per_dif,na.rm = TRUE))
pal <- colorBin("PRGn", domain = psrc_tracts_dc$per_dif, bins = bins)


m <- leaflet(psrc_tracts_dc)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=psrc_tracts_dc,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(psrc_tracts_dc$per_dif),
              fillOpacity = 0.7,
              popup = paste("new to old difference, %: ", round(psrc_tracts_dc$per_dif,2),"<br>",
                            "new to old difference, abs: ", psrc_tracts_dc$abs_dif,"<br>",
                            "Num of hh at risk in 2014by old meth: ", psrc_tracts_dc$at_risk_2014by, "(",psrc_tracts_dc$baseyear_du,")", "<br>",
                            "Num of hh at risk in 2014by new meth: ", psrc_tracts_dc$hh_at_risk_2014by_upd_meth,"(",psrc_tracts_dc$hh_total_2014by_upd_meth,")", "<br>",
                            "% of hh at risk in 2014by old meth: ", round(psrc_tracts_dc$per_atrisk_2014by,2),"<br>",
                            "% of hh at risk in 2014by new meth: ", round(psrc_tracts_dc$per_at_risk_2014by_upd_meth,2),"<br>",
                            "tract", psrc_tracts_dc$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = psrc_tracts_dc$per_dif, opacity = 0.7, title = "Difference between 2014by upd meth and 2014 old meth, % households at risk",
            position = "bottomright")
print(m)



# map difference abs
bins <- c(min(psrc_tracts_dc$abs_dif,na.rm = TRUE),-100,-50,-10, 0,10,50,100,200, max(psrc_tracts_dc$abs_dif,na.rm = TRUE))
pal <- colorBin("PRGn", domain = psrc_tracts_dc$per_dif, bins = bins)


m <- leaflet(psrc_tracts_dc)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=psrc_tracts_dc,
              stroke = T,
              opacity = 1,
              weight = 1,
              fillColor = ~pal(psrc_tracts_dc$abs_dif),
              fillOpacity = 0.7,
              popup = paste("new to old difference, %: ", round(psrc_tracts_dc$per_dif,2),"<br>",
                            "new to old difference, abs: ", psrc_tracts_dc$abs_dif,"<br>",
                            "Num of hh at risk in 2014by old meth: ", psrc_tracts_dc$at_risk_2014by, "(",psrc_tracts_dc$baseyear_du,")", "<br>",
                            "Num of hh at risk in 2014by new meth: ", psrc_tracts_dc$hh_at_risk_2014by_upd_meth,"(",psrc_tracts_dc$hh_total_2014by_upd_meth,")", "<br>",
                            "% of hh at risk in 2014by old meth: ", round(psrc_tracts_dc$per_atrisk_2014by,2),"<br>",
                            "% of hh at risk in 2014by new meth: ", round(psrc_tracts_dc$per_at_risk_2014by_upd_meth,2),"<br>",
                            "tract", psrc_tracts_dc$TRACTCE
              )) %>% 
  addLegend(pal = pal, values = psrc_tracts_dc$abs_dif, opacity = 0.7, title = "Difference between 2014by upd meth and 2014 old meth, abs households at risk",
            position = "bottomright")
print(m)


# ET, 1/20/22: Not sure what "data" variable is

data <- data %>%
  mutate(GEOID = factor(geoid10), 
         county = factor(str_sub(GEOID, 1, 5)),
         atrisk = baseyear_du - persists_du, 
         per_atrisk = atrisk / baseyear_du)

tract <- read_sf("./gis/tract2010_nowater.shp")
tract <- as_tibble(tract)
glimpse(tract)
tract <- tract %>%
  mutate(GEOID = factor(GEOID10))

#   project tracts
tract <- st_transform(tract, 4269)

# join databases  ----------------------------------------------- 
tract <- tract %>%
  left_join(data, by = ("GEOID")) 

# export databases ----------------------------------------------- 
write_rds(data, "./data/data_013_DevelopmentCapacity.rds")
write_rds(tract, "./data/tract_013_DevelopmentCapacity.rds")
