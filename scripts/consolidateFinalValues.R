# This file creates a final summary of all indicators by iterating through the data folder
# Y:\VISION 2050\Data\Displacement\Displacement Index 2021\data
# Generates plots of current and old distributions, maps, and quintiles for every indicator

# Libraries
library(tidyverse)
library(stats)
library(readxl)
library(writexl)
library(sf)
library(tigris)
library(leaflet)
library(plotly)
library(htmlwidgets)

### 2018 BY data
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/")
numbers <- c(paste("0", as.character(1:9), sep = ""),
             as.character(c(10:13,15)))

fileregex <- "(^[0123456789]{2}_)"

# Get GEOID to join on
temp <- read.csv("./01-People of Color/01_PeopleOfColor.csv")
indicators <- data.frame(GEOID = temp$GEOID)
rm(temp)

# Get data for all indicators (but 14, median rent), save in indicators data frame
for (num in numbers) {
  folder <- paste("./", 
                  list.files(pattern = paste(num, "-", sep = "")),
                  "/", 
                  sep = "")
  files <- list.files(path = folder, pattern = fileregex)
  
  for (file in files){
    data <- read.csv(paste(folder, file, sep = ""))
    data <- data[,c(1,ncol(data))]
    # data[,1] <- as.numeric(data[,1])
    indicators <- indicators %>% left_join(data, by = c("GEOID" = colnames(data)[1]))
  }
  
}
rm(data)

# Add more complicated median rent indicator to indicators data frame
medianRent <- read.csv("./14-Median Rent/14_MedianRent.csv")
medianRent <- medianRent %>% select(GEOID, starts_with("ind_"))
indicators <- indicators %>% left_join(medianRent, by = "GEOID")

rm(medianRent)

indicators <- indicators %>% relocate(votes, .after = ind_rent_5_rooms)

colnames(indicators)[c(11, 12, 18, 19)] <- c("per_pop_transit", 
                                             "per_area_transit", 
                                             "high_inc_neighbor", 
                                             "develop_cap")

write_xlsx(indicators, "./FinalIndicators.xlsx")

### 2014 BY data
setwd("Y:/Vision 2050/Data/Displacement/Displacement_Risk_Script/data/")
old <- read_excel("Y:/Vision 2050/Data/Displacement/displacement-risk-data.xlsx", sheet = 2, skip = 1)
old <- old %>% select(-starts_with("Classification"), -weight)
old <- old[,-(20:(ncol(old)-1))]

colnames(old)[1:15] <- colnames(indicators)[1:15]
colnames(old)[16:ncol(old)] <- c("parks", "school", "high_inc_neighbor", "develop_cap", "per_voted")

data <- read.csv("014_MedianRent.csv")
data <- data %>% select(GEOID, starts_with("ind_"))

old <- old %>% left_join(data, by = "GEOID")

old <- old %>% relocate(parks, .after = school) %>% relocate(per_voted, .after = ind_rent_5_rooms)

old <- data.frame(apply(old, 2, as.numeric))
old[,c(2:8, 11, 12, 19, ncol(old))] <- old[,c(2:8, 11, 12, 19, ncol(old))] * 100
rm(data)



### Plots

# Density comparison
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/docs/Figures/densities/")
for (i in 2:ncol(indicators)) {
  data <- indicators %>% select(i)
  data14 <- old %>% select(i+1)
  colnames(data14) <- colnames(data)
  data$year = as.factor(2020)
  data14$year = as.factor(2016)
  all <- rbind(data14, data)
  
  plot <- all %>% ggplot(aes(get(names(all)[1]), fill = year)) +
    geom_density(alpha=.2) + 
    xlab(names(all)[1])
  ggsave(paste(colnames(all)[1], ".png", sep = ""), plot = plot)
}

# Maps
workingdir <- "Y:/VISION 2050/Data/Displacement/Displacement Index 2021/docs/Figures/Maps/"
setwd(workingdir)
tracts <- tracts("WA", county = c(033, 035, 053, 061), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>%
  select(GEOID, TRACTCE, geometry)
tracts$GEOID <- as.numeric(tracts$GEOID)

makeMaps <- function(dataset, type, colors) {
  for(i in 2:ncol(dataset)) {
    data <- left_join(tracts, dataset %>% select(1, i), by = "GEOID")
    indicator <- names(data)[3]
    
    if (indicator != "high_inc_neighbor") {
      if (i %in% c(11, 12)) {
        bins <- c(floor(min(data[[3]], na.rm = TRUE)),
                  ceiling(seq(min(data[[3]], na.rm = TRUE),
                              max(data[[3]], na.rm = TRUE),
                              length = 10))[-1]
        )
      } else {
        bins <- quantile(data[[3]], na.rm = TRUE, probs = seq(0, 1, by = 0.2))
      }
      
      pal <- colorBin(colors, domain = data[[3]], bins = bins)
      m <- leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = data,
                    stroke = T,
                    opacity = 1,
                    weight = 1,
                    fillColor = ~pal(data[[3]]),
                    fillOpacity = 0.7,
                    popup = paste(
                      indicator,", ", type, ": ", round(data[[3]], 1), "%<br>",
                      "tract", data$TRACTCE
                    )) %>% 
        addLegend(pal = pal, values = data[[3]], opacity = 0.7, title = paste(indicator, ",", type),
                  position = "bottomright")
      saveWidget(as_widget(m), file.path(getwd(), paste(indicator, ".html", sep = "")))
    }
    print(i)
  }
}

# Make new/old maps
makeMaps(indicators, "New", "BrBG")
makeMaps(old, "Old", "BrBG")

# Create difference dataset
unpaired <- setdiff(indicators$GEOID, old$GEOID)
indicators_sub <- indicators %>% filter(!(GEOID %in% unpaired))
indicators_sub <- indicators_sub %>% arrange(GEOID)
old <- old %>% arrange(GEOID)
sum(indicators_sub$GEOID != old$GEOID)

diff <- indicators_sub[,2:ncol(indicators_sub)] - old[,2:ncol(old)]
diff$GEOID <- indicators_sub$GEOID
diff <- diff %>% relocate(GEOID)

# Make difference maps
makeMaps(diff, "Difference (New-Old)", "RdYlBu")

# Make high income neighbor maps
makeNeighborMap <- function(dataset, type, colors) {
  data <- left_join(tracts, dataset %>% select(1, 18), by = "GEOID")
  data[[3]] <- as.factor(data[[3]])
  indicator <- names(data)[3]
  
  pal <- colorFactor(colors, domain = data[[3]])
  
  m <- leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = data,
                stroke = T,
                opacity = 1,
                weight = 1,
                fillColor = ~pal(data[[3]]),
                fillOpacity = 0.7,
                popup = paste(
                  indicator,", ", type, ": ", data[[3]], "%<br>",
                  "tract", data$TRACTCE
                )) %>% 
    addLegend(pal = pal, values = data[[3]], opacity = 0.7, title = paste(indicator, ",", type),
              position = "bottomright")
  saveWidget(as_widget(m), file.path(getwd(), paste(indicator, ".html", sep = "")))
}

# New map
makeNeighborMap(indicators, "New", c("white", "red"))
makeNeighborMap(old, "Old", c("white", "red"))
diff$high_inc_neighbor <- factor(diff$high_inc_neighbor, labels = c("Lost neighbor, -1", "No change, 0", "Gained neighbor, 1"))
makeNeighborMap(diff, "Difference (New-Old)", c("red", "white", "purple"))

### Quintiles summary
clipindicators <- indicators %>% mutate(per_pop_transit = replace(per_pop_transit, 
                                                                  which(per_pop_transit == 0), 
                                                                  NA),
                                        per_area_transit = replace(per_area_transit, 
                                                                   which(per_area_transit == 0), 
                                                                   NA))
quintiles <- apply(clipindicators[,-1], 2, quantile, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
quintiles <- round(quintiles, 2)
quintiles <- t(quintiles)


