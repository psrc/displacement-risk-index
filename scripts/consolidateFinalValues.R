# This file creates a final summary of all indicators by iterating through the data folder - generating density plots of current and old distributions, histograms, maps, and quintiles
# Y:\VISION 2050\Data\Displacement\Displacement Index 2026\data

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
library(psrcelmer)


# Retrieve data ----------
## 2026 (2023 BY; 2024 ACS) ----
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2026/data")
numbers <- c(paste("0", as.character(1:9), sep = ""),
             as.character(c(10:13,15)))

fileregex <- "(^[0123456789]{2}_.*csv)"

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
    data <- data[,c(1,ncol(data))] # all rows, the first (geoid) and last (indicator value) columns
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

# finalize data set
indicators <- indicators %>% relocate(votes, .after = ind_rent_5_rooms)

colnames(indicators)[c(11, 12, 18, 19)] <- c("per_pop_transit", 
                                             "per_area_transit", 
                                             "high_inc_neighbor", 
                                             "develop_cap")
# save final data set as Excel workbook
write_xlsx(indicators, "./FinalIndicators.xlsx")
# indicators <- readxl::read_excel( "../../Displacement Index 2021/data/FinalIndicators.xlsx")


## 2021 (2018 BY; 2019 ACS) ----
old_2021_2018by <- readxl::read_excel( "../../Displacement Index 2021/data/FinalIndicators.xlsx")
# No additional formatting required because 2026 update is based on 2021 update

## 2018 (2014 BY; 2016 ACS) ----
old_2018_2014by <- read_excel("../../displacement-risk-data.xlsx", sheet = 2, skip = 1)

# Additional formatinng required to match 2021 and 2026 final data sets
old_2018_2014by <- old_2018_2014by %>% 
  select(-starts_with("Classification"), -weight)
old_2018_2014by <- old_2018_2014by[,-(20:(ncol(old_2018_2014by)-1))] #removing median rent indicator

colnames(old_2018_2014by)[1:15] <- colnames(indicators)[1:15] #renaming columns to align with newer datasets
colnames(old_2018_2014by)[16:ncol(old_2018_2014by)] <- c("parks", "school", "high_inc_neighbor", "develop_cap", "per_voted") #renaming columns to be consistent with newer datasets, but parks and school fields out of order

med_rent <- read.csv("../../Displacement_Risk_Script/data/014_MedianRent.csv")
med_rent <- med_rent %>% select(GEOID, starts_with("ind_"))

old_2018_2014by <- old_2018_2014by %>% 
  left_join(med_rent, by = "GEOID")

# reorder variables to align with newer datasets
old_2018_2014by <- old_2018_2014by %>% 
  relocate(parks, .after = school) %>% 
  relocate(per_voted, .after = ind_rent_5_rooms)

old_2018_2014by <- data.frame(apply(old_2018_2014by, 2, as.numeric))
old_2018_2014by[,c(2:8, 11, 12, 19, ncol(old_2018_2014by))] <- old_2018_2014by[,c(2:8, 11, 12, 19, ncol(old_2018_2014by))] * 100
rm(med_rent)



# Generate Plots ----------

## Density comparison ----
# This loop automates the creation of the histograms that are generated in the individual vis scripts for each indicator.

setwd("../docs/Figures/Densities/")

for (i in 2:ncol(indicators)) {
  data <- indicators %>% select(i)
  data21 <- old_2021_2018by %>% select(i)
  data18 <- old_2018_2014by %>% select(i)
  colnames(data18) <- colnames(data21)
  data$year = as.factor(2026)
  data21$year = as.factor(2021)
  data18$year = as.factor(2018)
  all <- rbind(data18, data21, data)
  
  plot <- all %>% ggplot(aes(get(names(all)[1]), fill = year)) +
    geom_density(alpha=.2) + 
    xlab(names(all)[1])
  ggsave(paste(colnames(all)[1], ".png", sep = ""), plot = plot)
}


## Histograms ----
# This loop automates the creation of the histograms that are generated in the individual vis scripts for each indicator. These do not include as much information as the individually generated ones, such as title/axis labels.
 
setwd("../Histograms/")

for (i in 2:ncol(indicators)) {
  
  colname <- names(indicators)[i]
  x <- indicators[[colname]]
  
  # quintiles (0%, 20%, 40%, 60%, 80%, 100%)
  q_df <- data.frame(
    xintercept = quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE),
    stat = "Quintiles"
  )
  
  plot <- ggplot(indicators, aes_string(colname)) +
    geom_histogram(fill = "royalblue3") +
    xlab(colname) +
    ggtitle("Distribution") +
    
    # one line per quintile
    geom_vline(
      data = q_df,
      aes(xintercept = xintercept, color = stat)) +
    
    # mean and median
    geom_vline(aes(xintercept = mean(x, na.rm = TRUE), color = "Mean")) +
    geom_vline(aes(xintercept = median(x, na.rm = TRUE), color = "Median")) +
    
    # computes a smoothed curve that approximates the probability density of your variable
    # geom_density(alpha = .2) +
    
    scale_color_manual(
      name = "Statistics",
      values = c("Quintiles" = "black", 
                 "Mean" = "red", 
                 "Median" = "orange"))
  
  ggsave(paste0(colname, ".png"), plot = plot)
  
}


# Maps ----------
setwd("../Maps/")

# Get tract shapefiles
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"
tracts10.url <- file.path(arc_service, "Census_Tracts_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tract_10 <- st_read(tracts10.url)
tract_20 <- st_read(tracts20.url)

tract_10 <- tract_10 %>% 
  select(GEOID=geoid_nm, geometry)
tract_20 <- tract_20 %>% 
  select(GEOID=geoid_nm, geometry)

## New (and old) maps ------
### all indicators, except 1 ----
makeMaps <- function(dataset, tracts, type, colors) {
  for(i in 2:ncol(dataset)) {
    data <- left_join(tracts, dataset %>% select(1, i), by = "GEOID")
    indicator <- names(data)[2]
    
    if (indicator != "high_inc_neighbor") {
      
      # --- BINNING LOGIC ---
      if (i %in% c(11, 12)) {
        
        bins <- c(
          floor(min(data[[2]], na.rm = TRUE)),
          ceiling(seq(
            min(data[[2]], na.rm = TRUE),
            max(data[[2]], na.rm = TRUE),
            length = 10
          ))[-1]
        )
        
      } else {
        
        bins <- quantile(
          data[[2]],
          na.rm = TRUE,
          probs = seq(0, 1, by = 0.2)
        )
        
      }
      
      # --- UNIVERSAL FIX: ENSURE VALID BREAKS ---
      
      # 1. Remove duplicate breakpoints
      bins <- unique(bins)
      
      # 2. Ensure at least 2 breaks (min & max)
      if (length(bins) < 2) {
        rng <- range(data[[2]], na.rm = TRUE)
        bins <- c(rng[1], rng[2])
      }
      
      # 3. Ensure strictly increasing breaks (rare edge case)
      if (is.unsorted(bins, strictly = TRUE)) {
        bins <- sort(bins)
        bins <- unique(bins)
      }
      
      # --- END BINNING LOGIC ---
      
      pal <- colorBin(colors, domain = data[[2]], bins = bins)
      m <- leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = data,
                    stroke = T,
                    opacity = 1,
                    weight = 1,
                    fillColor = ~pal(data[[2]]),
                    fillOpacity = 0.7,
                    popup = paste(
                      indicator,", ", type, ": ", round(data[[2]], 1), "%<br>",
                      "tract", data$GEOID
                    )) %>% 
        addLegend(pal = pal, values = data[[2]], opacity = 0.7, title = paste(indicator, ",", type),
                  position = "bottomright")
      saveWidget(as_widget(m), file.path(getwd(), paste(indicator, ".html", sep = "")))
    }
    print(i)
  }
}


# The map function take about 15-20 minutes to run through all of the indicators. Therefore, if the current project folder is copied from the previous update's project folder (for example, 'Displacement Index 2026' was copied from 'Displacement Index 2021'), only the most recent year's map function needs to be run because all of the previous years' maps (2018, 2021) are saved in the project folder's 'old' sub-folders (Y:\VISION 2050\Data\Displacement\Displacement Index 2026\docs\Figures\Maps).   

# save in: Y:\VISION 2050\Data\Displacement\Displacement Index 2026\docs\Figures\Maps\New 2026
makeMaps(indicators, tract_20, "New 2026", "BrBG")
# makeMaps(old_2018_2014by, tract_10, "Old 2018", "BrBG")
# makeMaps(old_2021_2018by, tract_10, "Old 2021", "BrBG")


### high income neighbors ----
# This indicator requires a little more transforming in a separate function.
makeNeighborMap <- function(dataset, tracts, type, colors) {
  data <- left_join(tracts, dataset %>% select(1, 18), by = "GEOID")
  data[[2]] <- as.factor(data[[2]])
  indicator <- names(data)[2]
  
  pal <- colorFactor(colors, domain = data[[2]])
  
  m <- leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = data,
                stroke = T,
                opacity = 1,
                weight = 1,
                fillColor = ~pal(data[[2]]),
                fillOpacity = 0.7,
                popup = paste(
                  indicator,", ", type, ": ", data[[2]], "%<br>",
                  "tract", data$GEOID
                )) %>% 
    addLegend(pal = pal, values = data[[2]], opacity = 0.7, title = paste(indicator, ",", type),
              position = "bottomright")
  saveWidget(as_widget(m), file.path(getwd(), paste(indicator, ".html", sep = "")))
}


# The maps using the old data don't need to be recreated if the previous project update folder was copied over to create the current update folder.
makeNeighborMap(indicators, tract_20,"New 2026", c("white", "red"))
# makeNeighborMap(old_2018_2014by, tract_10, "Old 2018", c("white", "red"))
# makeNeighborMap(old_2021_2018by, tract_10, "Old 2021", c("white", "red"))



## Difference maps ------
# Create difference data set 
# comparing 2018 to the 2021 update because in 2010 geographies, 2026 update in 2020 geographies and therefore won't be compared.
unpaired <- setdiff(old_2021_2018by$GEOID, old_2018_2014by$GEOID)
indicators_sub <- old_2021_2018by %>% 
  filter(!(GEOID %in% unpaired)) %>% 
  arrange(GEOID)
old_2018_2014by <- old_2018_2014by %>% 
  arrange(GEOID)
sum(indicators_sub$GEOID != old_2018_2014by$GEOID)

diff <- indicators_sub[,2:ncol(indicators_sub)] - old_2018_2014by[,2:ncol(old_2018_2014by)]
diff$GEOID <- indicators_sub$GEOID
diff <- diff %>% relocate(GEOID)

# save in: Y:\VISION 2050\Data\Displacement\Displacement Index 2026\docs\Figures\Maps\Difference

### all indicators, except 1 ----
makeMaps(diff, tract_10, "Difference (2021-2018)", "RdYlBu")

### high income neighbors ----
diff$high_inc_neighbor <- factor(diff$high_inc_neighbor, labels = c("Lost neighbor, -1", "No change, 0", "Gained neighbor, 1"))
makeNeighborMap(diff, tract_10, "Difference (2021-2018)", c("red", "white", "purple"))




# Quintiles summary ----------
# The following code generates quintile breakpoints for each of the individual indicators
clipindicators <- indicators %>% mutate(per_pop_transit = replace(per_pop_transit, 
                                                                  which(per_pop_transit == 0), 
                                                                  NA),
                                        per_area_transit = replace(per_area_transit, 
                                                                   which(per_area_transit == 0), 
                                                                   NA))
quintiles <- apply(clipindicators[,-1], 2, quantile, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
quintiles <- round(quintiles, 2)
quintiles <- t(quintiles)

# The quintile values generated in this matrix are then manually added to the final spreadsheet: Y:\VISION 2050\Data\Displacement\Displacement Index 2026\docs\displacement-risk-data-2026.xlsx > tab: 'Classification of Indicators'
# Based on these updated quintile values and the indicator's distribution, the classification values are adjusted 
# In the 2026 update, the values in the 'Data - Individual Indicators' tab and 'Classification' columns directly link to the adjusted classification values (instead of being manually added in the formula bars)





## Individual maps for each indicator ------
# based on the designated quintiles in the 'displacement-risk-data-2026.xlsx'
setwd("Y:/VISION 2050/Data/Displacement/Displacement Index 2026/docs")

raw <- read_excel("./displacement-risk-data-2026.xlsx",
                  sheet = 'Data - Individual Indicators',
                  col_names = FALSE, 
                  n_max = 2)

indicator_group <- as.character(raw[1, ])
subindicator    <- as.character(raw[2, ])

# filling in NA rows by duplicating the indicator group
indicator_group <- fill(data.frame(indicator_group), indicator_group)$indicator_group

# Replace NA subindicator names
subindicator[is.na(subindicator)] <- "value"


clean_names <- mapply(
  function(group, sub) {
    group_clean <- gsub("[^A-Za-z0-9]+", "_", group)
    sub_clean   <- gsub("[^A-Za-z0-9]+", "_", sub)
    
    # GEOID stays as-is
    if (!is.na(group) && group == "GEOID") {
      return("GEOID")
    }
    
    # CLASSIFICATION COLUMNS
    if (!is.na(sub) && grepl("^Classification", sub)) {
      
      # extract suffix after "Classification"
      suffix <- sub("^Classification", "", sub)   # e.g., "_CB"
      suffix <- gsub("[^A-Za-z0-9]+", "_", suffix)
      
      # if no suffix, leave blank
      if (suffix == "") {
        return(paste0(group_clean, "_class"))
      } else {
        return(paste0(group_clean, "_", suffix, "_class"))
      }
    }
    
    # VALUE COLUMNS
    paste0(group_clean, "_", sub_clean, "_value")
  },
  group = indicator_group,
  sub   = subindicator
)



data <- readxl::read_excel("Y:/VISION 2050/Data/Displacement/Displacement Index 2026/docs/displacement-risk-data-2026.xlsx",
                           sheet = 'Data - Individual Indicators',
                           skip = 2)
names(data) <- clean_names

# Identify all '_class' fields
class_fields <- grep("_class$", names(data), value = TRUE)

# Derive the matching value field for each class field
value_field_from_class <- function(class_name) {
  sub("_class$", "_value", class_name)
}

# Create mapping function
make_map <- function(class_field, data_sf) {
  
  value_field <- value_field_from_class(class_field)
  
  classes <- data_sf[[class_field]]
  values  <- data_sf[[value_field]]
  
  pal <- colorFactor(
    palette = rev(RColorBrewer::brewer.pal(5, "RdYlGn")),
    domain  = 0:4
  )
  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = data_sf,
      fillColor = ~pal(classes),
      fillOpacity = 0.7,
      opacity = 1,
      color = "grey",
      weight = 0.5,
      popup = paste0(
        "<b>", class_field, "</b><br>",
        "Quintile: ", classes, "<br>",
        "Tract: ", data_sf$GEOID
      )
    ) %>%
    addLegend(
      pal = pal,
      values = classes,
      title = class_field,
      opacity = 0.7
    )
}

# Join risk data to tract data
data_sf <- left_join(tract_20, data, by = "GEOID")


# Generate and save maps for every class field
for (cf in class_fields) {
  map <- make_map(cf, data_sf)
  saveWidget(
    map,
    file = paste0("./Figures/Maps/New 2026/Index_Values_0_4/", cf, ".html")
  )
}







# Mapping composite index ----------
## 2026 ------
# The composite index was drafted manually in a spreadsheet. The quintiles that were generated above were used to guide the breakpoints. 

# load data set from manually edited workbook
drm_2026 <- read_excel("Y:/VISION 2050/Data/Displacement/Displacement Index 2026/docs/displacement-risk-data-2026.xlsx", 
                         sheet = 'Data - Composite Index map')

data_26 <- tract_20 %>%
  left_join(drm_2026[, 1:4], by = "GEOID") %>%
  mutate(risk_level_name= factor(risk_level_name, 
                                 levels = c("lower", "moderate", "higher")))


pal_level <- colorFactor(palette = rev(RColorBrewer::brewer.pal(3, "Pastel2")), 
                         domain = data_26$risk_level_name)

m <- leaflet(data_26)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=data_26,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_level(data_26$risk_level_name),
              fillOpacity = 0.7,
              popup = paste("Risk level: ", data_26$risk_level_name,"<br>",
                            "Risk score: ", data_26$risk_score, "(out of 60)", "<br>",
                            "tract: ", data_26$GEOID
              )) %>% 
  addLegend(pal =  pal_level, 
            values = data_26$risk_level_name, 
            opacity = 0.7, 
            title = paste("Displacement Risk Index","<br>",
                          "DRAFT, 2026"),
            position = "bottomright")
print(m)

saveWidget(m, 
           file = "Y:/VISION 2050/Data/Displacement/Displacement Index 2026/docs/Figures/Maps/Composite Index/displacement_risk_composite_index_26_draft.html", 
           selfcontained = TRUE)


## 2021 ------
# load data set from manually edited workbook
drm_2021 <- read_excel("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/docs/displacement-risk-data-2021.xlsx",
                       sheet = 'Data - Composite Index')

data_21 <- tract_10 %>%
  left_join(drm_2021[, 1:4], by = "GEOID") %>%
  mutate(risk_level_name= factor(risk_level_name, 
                                 levels = c("lower", "moderate", "higher")))


pal_level <- colorFactor(palette = rev(RColorBrewer::brewer.pal(3, "Pastel2")), 
                         domain = data_21$risk_level_name)

m <- leaflet(data_21)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=data_21,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_level(data_21$risk_level_name),
              fillOpacity = 0.7,
              popup = paste("Risk level: ", data_21$risk_level_name,"<br>",
                            "Risk score: ", data_21$risk_score, "(out of 60)", "<br>",
                            "tract: ", data_21$GEOID
              )) %>% 
  addLegend(pal =  pal_level, 
            values = data_21$risk_level_name, 
            opacity = 0.7, 
            title = paste("Displacement Risk Index","<br>",
                          "FINAL, 2021"),
            position = "bottomright")
print(m)

saveWidget(m, 
           file = "Y:/VISION 2050/Data/Displacement/Displacement Index 2026/docs/Figures/Maps/Composite Index/displacement_risk_composite_index_21.html", 
           selfcontained = TRUE)



## comparing 2021 and 2026 ------
# We want to see how areas have changed over time, between the previous update (2021) with new update (2026)
# these are in two different geographies - 2010 and 2020 census tracts, requiring a cross walk
crosswalk_10_20 <- get_table(schema="census",
                             tbl_name="v_geo_relationships_tracts") %>% 
  mutate(geoid10=as.numeric(geoid10),
         geoid20=as.numeric(geoid20))

# 2021 data
drm_2021 <- read_excel("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/docs/displacement-risk-data-2021.xlsx",
                       sheet = 'Data - Composite Index')
colnames(drm_2021) <- paste0(colnames(drm_2021), "_2021") # Add suffix year to all column names

# 2026 data
drm_2026 <- read_excel("Y:/VISION 2050/Data/Displacement/Displacement Index 2026/docs/displacement-risk-data-2026.xlsx", 
                       sheet = 'Data - Composite Index')
colnames(drm_2026) <- paste0(colnames(drm_2026), "_2026") # Add suffix year to all column names


# join crosswalk to 2026 and 2021 data (only first four columns, because of formatting other calculations)
compare_data <- crosswalk_10_20 %>% 
  left_join(drm_2021[, 1:4], by = c("geoid10"="GEOID_2021")) %>%
  left_join(drm_2026[, 1:4], by = c("geoid20"="GEOID_2026")) %>% 
  mutate(dif_2026_2021=risk_level_2026-risk_level_2021)


# map the difference
compare_sf <- left_join(tract_20, compare_data, by = c("GEOID"="geoid20"))

pal_dif <- colorFactor(palette = rev("Set3"), 
                       domain = compare_sf$dif_2026_2021)

m <- leaflet(compare_sf)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=compare_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_dif(compare_sf$dif_2026_2021),
              fillOpacity = 0.7,
              popup = paste("Risk level (2021): ", compare_sf$risk_level_name_2021,"<br>",
                            "Risk level (2026): ", compare_sf$risk_level_name_2026, "<br>",
                            "Difference (2026-2021): ", compare_sf$dif_2026_2021 , "<br>",
                            "tract: ", compare_sf$GEOID
              )) %>% 
  addLegend(pal =  pal_dif, 
            values = compare_sf$dif_2026_2021, 
            opacity = 0.7, 
            title = htmltools::HTML("DRI change (2026-2021)<br><span style='font-size:9px; font-style:italic;'>positive value indicates increased risk</span>"),
            position = "bottomright")
print(m)

saveWidget(m, 
           file = "Y:/VISION 2050/Data/Displacement/Displacement Index 2026/docs/Figures/Maps/Composite Index/difference_26_21.html", 
           selfcontained = TRUE)
