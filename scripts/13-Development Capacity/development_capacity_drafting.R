# Libraries --------------------------------------
# install.packages(tidyverse)
# install.packages(tidycensus)
library(tidyverse)
library(tidycensus)
library(readxl) # read_excel()
library(psrccensus)
library(psrcelmer) # access Elmer CHAS data
library(sf)
library(leaflet)
library(htmlwidgets)
library(psrcplot) # access psrc_colors

# Load data ----------
# 2026 update (less restrictive) - 2023BY
# updated file *age8_devfctr1*.csv is the less restrictive one (results in 39%) - 2023BY
less_restrictive <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement Index 2026/data/13-Development Capacity/exploration-2026/hh_at_displacement_risk_age8_devfctr1-BY2023-2026-09-28.csv")

# 2026 update (more restrictive) - 2023BY
# updated file *age50_devfctr3*.csv results in 16.6% - 2023BY
more_restrictive <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement Index 2026/data/13-Development Capacity/exploration-2026/hh_at_displacement_risk_age50_devfctr3-BY2023-2026-09-28.csv")

# 2021 update - 2018BY
# previous file results in 7% - 2018BY
disp_2018 <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement Index 2021/data/13-Development Capacity/2018by_upd_meth/hh_at_displacement_risk-2021-11-29/hh_at_displacement_risk-2021-11-29.csv")


# Calculate percent at risk ----------
# 2026 update (less restrictive) - 2023BY
less_restrictive_grp = less_restrictive %>%
  select(census_tract_id,hh_at_risk,hh_total) %>%
  group_by(census_tract_id) %>% 
  summarise(hh_at_risk_2023by = sum(hh_at_risk), hh_total_2023by = sum(hh_total),
            per_at_risk_2023by = as.double(hh_at_risk_2023by/hh_total_2023by) * 100) 

# 2026 update (more restrictive) - 2023BY
more_restrictive_grp = more_restrictive %>%
  select(census_tract_id,hh_at_risk,hh_total) %>%
  group_by(census_tract_id) %>% 
  summarise(hh_at_risk_2023by = sum(hh_at_risk), hh_total_2023by = sum(hh_total),
            per_at_risk_2023by = as.double(hh_at_risk_2023by/hh_total_2023by) * 100)

# 2021 update - 2018BY
disp_2018_grp = disp_2018 %>%
  select(census_tract_id,hh_at_risk,hh_total) %>%
  group_by(census_tract_id) %>% 
  summarise(hh_at_risk_2018by = sum(hh_at_risk), hh_total_2018by = sum(hh_total),
            per_at_risk_2018by = as.double(hh_at_risk_2018by/hh_total_2018by) * 100) 



# join 'census_tract_id' to 'geoid20' ----------
# load crosswalk between IDs in Hana's data and geoid for census tracts/block groups 
crosswalk_20 <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement Index 2026/data/13-Development Capacity/exploration-2026/parcels_census.csv")

# view dataset
head(crosswalk_20)

# edit to simplify names
names(crosswalk_20) <- sub(":.*", "", names(crosswalk_20))

# mapping at the census tract level, simplify dataset
crosswalk_20_simp <- crosswalk_20 %>% 
  select(census_tract_id, census_2020_tract_id) %>% 
  distinct()

# 2026 update (less restrictive) - 2023BY
less_restrictive_tract <- less_restrictive_grp %>% 
  left_join(crosswalk_20_simp, by='census_tract_id')

# 2026 update (more restrictive) - 2023BY
more_restrictive_tract <- more_restrictive_grp %>% 
  left_join(crosswalk_20_simp, by='census_tract_id')

# 2021 update - 2018BY
# Loading 2014by data to get GEOIDs
disp_risk_2014by <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/013_DevelopmentCapacity.csv")
geoid_info <- disp_risk_2014by %>% 
  select(census_tract_id, geoid10)

# Join GEOIDs with 2018by data
# Loading 2014by data to get GEOIDs
disp_risk_2014by <- read_csv("Y:/VISION 2050/Data/Displacement/Displacement_Risk_Script/data/013_DevelopmentCapacity.csv")
geoid_info <- disp_risk_2014by %>% 
  select(census_tract_id, geoid10)

# Join GEOIDs with 2018by data
disp_2018_grp <- disp_2018_grp %>% 
  left_join(geoid_info, by = "census_tract_id") %>% 
  mutate(geoid10 = as.character(geoid10))

disp_2018_grp <- disp_2018_grp %>% 
  select(-census_tract_id) %>% 
  mutate(geoid10 = as.numeric(geoid10))

# create difference data sets ----
# 2026: less restrictive - more restrictive
more_restrictive_tract_simp <- more_restrictive_tract %>% 
  select(census_2020_tract_id, per_at_risk_2023by, hh_at_risk_2023by, hh_total_2023by) %>% 
  rename(more_percent=per_at_risk_2023by,
         more_hh_at_risk = hh_at_risk_2023by,
         more_hh_total = hh_total_2023by)
less_restrictive_tract_simp <- less_restrictive_tract %>% 
  select(census_2020_tract_id, per_at_risk_2023by, hh_at_risk_2023by, hh_total_2023by) %>% 
  rename(less_percent=per_at_risk_2023by,
         less_hh_at_risk = hh_at_risk_2023by,
         less_hh_total = hh_total_2023by)

dif_2026 <- more_restrictive_tract_simp %>% 
  left_join(less_restrictive_tract_simp, by = "census_2020_tract_id") %>% 
  mutate(dif_2026 = less_percent-more_percent)

# 2026 less restrictive - 2021 original
# convert 2026 data set in 2020 geographies to 2010 geographies using crosswalk  
crosswalk_10_20 <- get_table(schema="census",
                             tbl_name="v_geo_relationships_tracts") %>% 
  mutate(geoid10 = as.numeric(geoid10),
         geoid20 = as.numeric(geoid20))

df_2026less_2010geog <- less_restrictive_tract %>% 
  left_join(crosswalk_10_20, join_by("census_2020_tract_id"=="geoid20")) %>% 
  group_by(geoid10) %>% 
  summarise(hh_at_risk = sum(hh_at_risk_2023by),
            hh_total = sum(hh_total_2023by),
            per_at_risk = (hh_at_risk/hh_total)*100)

# subtract 2026-2021 values
df_2026less_2021 <- df_2026less_2010geog %>% 
  left_join(disp_2018_grp, by = "geoid10") %>% 
  mutate(dif_2026less_2021 = per_at_risk-per_at_risk_2018by)


# 2026 more restrictive - 2021 original
df_2026more_2010geog <- more_restrictive_tract %>% 
  left_join(crosswalk_10_20, join_by("census_2020_tract_id"=="geoid20")) %>% 
  group_by(geoid10) %>% 
  summarise(hh_at_risk = sum(hh_at_risk_2023by),
            hh_total = sum(hh_total_2023by),
            per_at_risk = (hh_at_risk/hh_total)*100)

# subtract 2026-2021 values
df_2026more_2021 <- df_2026more_2010geog %>% 
  left_join(disp_2018_grp, by = "geoid10") %>% 
  mutate(dif_2026more_2021 = per_at_risk-per_at_risk_2018by)

# create spatial layers ----------
# Connecting to ElmerGeo for census geographies through Portal
arc_service <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services"

tracts20.url <- file.path(arc_service, "Census_Tracts_2020/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
tracts10.url <- file.path(arc_service, "Census_Tracts_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

tracts20.lyr <- st_read(tracts20.url)
tracts10.lyr <- st_read(tracts10.url)

# 2026 update (less restrictive) - 2023BY
less_restrictive_tract_sf <- tracts20.lyr %>% 
  left_join(less_restrictive_tract, join_by("geoid_nm"=="census_2020_tract_id"))

# 2026 update (more restrictive) - 2023BY
more_restrictive_tract_sf <- tracts20.lyr %>% 
  left_join(more_restrictive_tract, join_by("geoid_nm"=="census_2020_tract_id"))

# 2021 update - 2018BY
disp_2018_sf <- tracts10.lyr %>% 
  left_join(disp_2018_grp, join_by("geoid_nm"=="geoid10"))

# create difference data sets ----
# 2026: less restrictive - more restrictive
dif_2026_sf <- tracts20.lyr %>% 
  left_join(dif_2026, join_by("geoid_nm"=="census_2020_tract_id"))

# 2026 less restrictive - 2021 original
dif_2026less_2021_sf <- tracts10.lyr %>% 
  left_join(df_2026less_2021, join_by("geoid_nm"=="geoid10"))

# 2026 more restrictive - 2021 original
dif_2026more_2021_sf <- tracts10.lyr %>% 
  left_join(df_2026more_2021, join_by("geoid_nm"=="geoid10"))


# mapping and visualizing ----------
# set up palettes
max_value <- 100 # this value will be the max value in the legend
min_value <- -50 # for difference datasets
# tick_positions <- seq(0, 100, length.out = 5)
# tick_positions_dif <- seq(-50, 100, length.out = 7)

psrc_purple_plus<-c("#FFFFFF","#F6CEFC", psrc_colors$purples_inc)

# true values
pal_2021 <- leaflet::colorNumeric(palette=psrc_purple_plus,
                                  domain = c(min(disp_2018_sf$per_at_risk_2018by, na.rm = TRUE),max_value),
                                  na.color = "transparent")
pal_less_2026 <- leaflet::colorNumeric(palette=psrc_purple_plus,
                                     domain = c(min(less_restrictive_tract_sf$per_at_risk_2023by,na.rm = TRUE),max_value),
                                     na.color = "transparent")
pal_more_2026 <- leaflet::colorNumeric(palette=psrc_purple_plus,
                                     domain = c(min(more_restrictive_tract_sf$per_at_risk_2023by,na.rm = TRUE),max_value),
                                     na.color = "transparent")


# ------------------------------------------------------------
# Code to create an asymmetric legend (-50 > 100) for the difference data sets with the 'midpoint' 0 being white - shown as a continuous range, instead of binned
fixed_range <- c(-50, 100)

legend_breaks <- c(-50, -25, 0, 25, 50, 75, 100)


# NA values remain NA.
rescale_zero <- function(x) {
  
  y <- rep(NA_real_, length(x))
  
  # Only evaluate non-NA observations
  ok <- !is.na(x)
  
  # Negative / zero values
  neg <- ok & x <= 0
  
  y[neg] <- 0.5 *
    (x[neg] + 50) / 50
  
  # Positive values
  pos <- ok & x > 0
  
  y[pos] <- 0.5 +
    0.5 * x[pos] / 100
  
  # Clip values outside -50 to 100
  y[ok] <- pmax(
    0,
    pmin(1, y[ok])
  )
  
  y
}

# Base palette

color_values <- c(
  "#006400",   # -50: dark green
  "#FFFFFF",   #   0: white
  "#4B0082"    # 100: dark purple
)

base_pal_dif <- colorNumeric(
  palette = color_values,
  domain = c(0, 1),
  na.color = "transparent"
)

# Palette used by the polygons

midpoint_pal_dif <- function(x) {
  
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  x <- as.numeric(x)
  z <- rescale_zero(x)
  base_pal_dif(z)
}


# Create a genuine colorNumeric palette for Leaflet

legend_pal_base <- colorNumeric(
  palette = color_values,
  domain = fixed_range,
  na.color = "transparent"
)


# Wrap it while preserving Leaflet's required attributes

legend_pal_dif <- function(x) {
  
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  
  x <- as.numeric(x)
  
  z <- rescale_zero(x)
  
  base_pal_dif(z)
}


attributes(legend_pal_dif) <- attributes(legend_pal_base)


m <- leaflet(less_restrictive_tract_sf)%>%
  # addProviderTiles(providers$OpenStreetMap) %>%  # default OSM
  # addProviderTiles(providers$OpenStreetMap.HOT) %>%
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addLayersControl(overlayGroups = c("Old, 2021 update",
                                     "Less Restrictive, 2026 update",
                                     "More Restrictive, 2026 update",
                                     "Dif less-more, 2026 update",
                                     "Dif 2026 less restrictive - 2021",
                                     "Dif 2026 more restrictive - 2021"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  # 2021 update
  addPolygons(data=disp_2018_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_2021(disp_2018_sf$per_at_risk_2018by),
              fillOpacity = 1,
              group = "Old, 2021 update",
              popup = paste("% hh at risk: ", round(disp_2018_sf$per_at_risk_2018by,2), "<br>",
                            "hh at risk: ", disp_2018_sf$hh_at_risk_2018by, "<br>",
                            "total hh: ", disp_2018_sf$hh_total_2018by, "<br>",
                            "tract: ", disp_2018_sf$geoid10
              )) %>%
  # 2026 less restrictive
  addPolygons(data=less_restrictive_tract_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_less_2026(less_restrictive_tract_sf$per_at_risk_2023by),
              fillOpacity = 1,
              group = "Less Restrictive, 2026 update",
              popup = paste("% hh at risk: ", round(less_restrictive_tract_sf$per_at_risk_2023by,2), "<br>",
                            "hh at risk: ", less_restrictive_tract_sf$hh_at_risk_2023by, "<br>",
                            "total hh: ", less_restrictive_tract_sf$hh_total_2023by, "<br>",
                            "tract: ", less_restrictive_tract_sf$geoid20
              )) %>%
  # 2026 more restrictive
  addPolygons(data=more_restrictive_tract_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_more_2026(more_restrictive_tract_sf$per_at_risk_2023by),
              fillOpacity = 1,
              group = "More Restrictive, 2026 update",
              popup = paste("% hh at risk: ", round(more_restrictive_tract_sf$per_at_risk_2023by,2),"<br>",
                            "hh at risk: ", more_restrictive_tract_sf$hh_at_risk_2023by, "<br>",
                            "total hh: ", more_restrictive_tract_sf$hh_total_2023by, "<br>",
                            "tract: ", more_restrictive_tract_sf$geoid20
              )) %>%
  # 2026 difference (less-more restrictive)
  addPolygons(data=dif_2026_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~midpoint_pal_dif(dif_2026_sf$dif_2026),
              fillOpacity = 1,
              group = "Dif less-more, 2026 update",
              popup = paste("% hh difference at risk: ", round(dif_2026_sf$dif_2026,2),"<br>",
                            "% hh at risk (less): ", round(dif_2026_sf$less_percent,2),"<br>",
                            "% hh at risk (more): ", round(dif_2026_sf$more_percent,2),"<br>",
                            "hh at risk (less): ", dif_2026_sf$less_hh_at_risk, "<br>",
                            "hh at risk (more): ", dif_2026_sf$more_hh_at_risk, "<br>",
                            "total hh (less): ", dif_2026_sf$less_hh_total, "<br>",
                            "total hh (more): ", dif_2026_sf$more_hh_total, "<br>",
                            "tract: ", dif_2026_sf$geoid20
              )) %>%
  # difference 2026 (less restrictive) - 2021 update
  addPolygons(data=dif_2026less_2021_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~midpoint_pal_dif(dif_2026less_2021_sf$dif_2026less_2021),
              fillOpacity = 1,
              group = "Dif 2026 less restrictive - 2021",
              popup = paste("% hh difference at risk: ", round(dif_2026less_2021_sf$dif_2026less_2021,2),"<br>",
                            "% hh at risk (2026): ", round(dif_2026less_2021_sf$per_at_risk,2),"<br>",
                            "% hh at risk (2021): ", round(dif_2026less_2021_sf$per_at_risk_2018by,2),"<br>",
                            "hh at risk (2026): ", dif_2026less_2021_sf$hh_at_risk, "<br>",
                            "hh at risk (2021): ", dif_2026less_2021_sf$hh_at_risk_2018by, "<br>",
                            "total hh (2026): ", dif_2026less_2021_sf$hh_total, "<br>",
                            "total hh (2021): ", dif_2026less_2021_sf$hh_total_2018by, "<br>",
                            "tract: ", dif_2026less_2021_sf$geoid10
              )) %>%
  # difference 2026 (more restrictive) - 2021 update
  addPolygons(data=dif_2026more_2021_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~midpoint_pal_dif(dif_2026more_2021_sf$dif_2026more_2021),
              fillOpacity = 1,
              group = "Dif 2026 more restrictive - 2021",
              popup = paste("% hh difference at risk: ", round(dif_2026more_2021_sf$dif_2026more_2021,2),"<br>",
                            "% hh at risk (2026): ", round(dif_2026more_2021_sf$per_at_risk,2),"<br>",
                            "% hh at risk (2021): ", round(dif_2026more_2021_sf$per_at_risk_2018by,2),"<br>",
                            "hh at risk (2026): ", dif_2026more_2021_sf$hh_at_risk, "<br>",
                            "hh at risk (2021): ", dif_2026more_2021_sf$hh_at_risk_2018by, "<br>",
                            "total hh (2026): ", dif_2026more_2021_sf$hh_total, "<br>",
                            "total hh (2021): ", dif_2026more_2021_sf$hh_total_2018by, "<br>",
                            "tract: ", dif_2026more_2021_sf$geoid10
              )) %>%
  
  # 2021 update
  addLegend(pal =  pal_2021, 
            # values = disp_2018_sf$per_at_risk_2018by, 
            values = tick_positions,  # force legend to use these values
            opacity = 0.7, 
            group="Old, 2021 update",
            title = htmltools::HTML(paste("Development","<br>", "Capacity (%)", "<br>", 
                                          "2021 update"),"<br><span style='font-size:9px; font-weight:normal;'>--2010 geographies--</span>"),
            position = "bottomright") %>%
  # 2026 less restrictive
  addLegend(pal =  pal_less_2026, 
            # values = less_restrictive_tract_sf$per_at_risk_2023by, 
            values = tick_positions,  # force legend to use these values
            opacity = 0.7, 
            group="Less Restrictive, 2026 update",
            title = htmltools::HTML(paste("Development","<br>", "Capacity (%)", "<br>", 
                                          "2026 update"),"<br><span style='font-size:9px; font-weight:normal;'>--2020 geographies--</span>"),
            position = "bottomright") %>%
  # 2026 more restrictive
  addLegend(pal =  pal_more_2026, 
            # values = more_restrictive_tract_sf$per_at_risk_2023by, 
            values = tick_positions,  # force legend to use these values
            opacity = 0.7, 
            group="More Restrictive, 2026 update",
            title = htmltools::HTML(paste("Development","<br>", "Capacity (%)", "<br>", 
                                          "2026 update"),"<br><span style='font-size:9px; font-weight:normal;'>--2020 geographies--</span>"),
            position = "bottomright") %>% 
  # 2026 difference (less-more restrictive)
  addLegend(pal = legend_pal_dif,
            values = legend_breaks,
            opacity = 0.7, 
            group="Dif less-more, 2026 update",
            title = htmltools::HTML(paste("Difference","<br>", "2026 less-more restrictive", "<br>", "Dev. Capacity (%)"),"<br><span style='font-size:9px; font-weight:normal;'>--2020 geographies--</span>"),
            position = "bottomright") %>% 
  # difference 2026 (less restrictive) - 2021 update
  addLegend(pal = legend_pal_dif,
            values = legend_breaks,
            opacity = 0.7, 
            group="Dif 2026 less restrictive - 2021",
            title = htmltools::HTML(paste("Difference", "<br>", "2026 (less restrictive) - 2021","<br>", "Dev. Capacity (%)"),"<br><span style='font-size:9px; font-weight:normal;'>--2010 geographies--</span>","<br><span style='font-size:9px; font-style:italic;'>positive values (purple) indicates increased risk over time</span>"),
            position = "bottomright") %>% 
  
  # difference 2026 (more restrictive) - 2021 update
  addLegend(pal = legend_pal_dif,
            values = legend_breaks,
            opacity = 0.7, 
            group="Dif 2026 more restrictive - 2021",
            title = htmltools::HTML(paste("Difference", "<br>", "2026 (more restrictive) - 2021","<br>", " Dev. Capacity (%)"),"<br><span style='font-size:9px; font-weight:normal;'>--2010 geographies--</span>","<br><span style='font-size:9px; font-style:italic;'>positive values (purple) indicates increased risk over time</span>"),
            position = "bottomright") %>% 
  
  #default hide overlap groups
  hideGroup("Old, 2021 update") %>% 
  hideGroup("Less Restrictive, 2026 update") %>% 
  hideGroup("More Restrictive, 2026 update") %>%
  hideGroup("Dif less-more, 2026 update") %>%
  hideGroup("Dif 2026 less restrictive - 2021") %>%
  hideGroup("Dif 2026 more restrictive - 2021") %>%
  
  print(m)


saveWidget(m, 
           file = "Y:/VISION 2050/Data/Displacement/Displacement Index 2026/data/13-Development Capacity/exploration-2026/exploration-maps.html", 
           selfcontained = TRUE)


















# ------------------------------------------------------------
# Code to create an asymmetric legend (-50 > 100) for the difference data sets with the 'midpoint' 0 being white - organized in bins
# 
# Use the REAL data domain here.
# Leaflet therefore knows the legend is -50 to 100.

pal_dif <- colorBin(
  palette = c(
    "#006400",
    "#66A061",
    "#FFFFFF",
    "#D8C9E8",
    "#B18BCB",
    "#8054A8",
    "#4B0082"
  ),
  domain = c(-50, 100),
  bins = c(-50, -25, 0, 0.000001, 25, 50, 75, 100),
  na.color = "transparent"
)


m <- leaflet(less_restrictive_tract_sf)%>%
  # addProviderTiles(providers$OpenStreetMap) %>%  # default OSM
  # addProviderTiles(providers$OpenStreetMap.HOT) %>%
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addLayersControl(overlayGroups = c("Old, 2021 update",
                      "Less Restrictive, 2026 update",
                      "More Restrictive, 2026 update",
                      "Dif less-more, 2026 update",
                      "Dif 2026 less restrictive - 2021",
                      "Dif 2026 more restrictive - 2021"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  # 2021 update
  addPolygons(data=disp_2018_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_2021(disp_2018_sf$per_at_risk_2018by),
              fillOpacity = 1,
              group = "Old, 2021 update",
              popup = paste("%hh at risk: ", round(disp_2018_sf$per_at_risk_2018by,2), "<br>",
                            "tract: ", disp_2018_sf$geoid10
              )) %>%
  # 2026 less restrictive
  addPolygons(data=less_restrictive_tract_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_less_2026(less_restrictive_tract_sf$per_at_risk_2023by),
              fillOpacity = 1,
              group = "Less Restrictive, 2026 update",
              popup = paste("%hh at risk: ", round(less_restrictive_tract_sf$per_at_risk_2023by,2), "<br>",
                            "tract: ", less_restrictive_tract_sf$geoid20
              )) %>%
  # 2026 more restrictive
  addPolygons(data=more_restrictive_tract_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_more_2026(more_restrictive_tract_sf$per_at_risk_2023by),
              fillOpacity = 1,
              group = "More Restrictive, 2026 update",
              popup = paste("%hh at risk: ", round(more_restrictive_tract_sf$per_at_risk_2023by,2),"<br>",
                            "tract: ", more_restrictive_tract_sf$geoid20
              )) %>%
  # 2026 difference (less-more restrictive)
  addPolygons(data=dif_2026_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_dif(dif_2026_sf$dif_2026),
              fillOpacity = 1,
              group = "Dif less-more, 2026 update",
              popup = paste("%hh difference at risk: ", round(dif_2026_sf$dif_2026,2),"<br>",
                            "tract: ", dif_2026_sf$geoid20
              )) %>%
  # difference 2026 (less restrictive) - 2021 update
  addPolygons(data=dif_2026less_2021_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_dif(dif_2026less_2021_sf$dif_2026less_2021),
              fillOpacity = 1,
              group = "Dif 2026 less restrictive - 2021",
              popup = paste("%hh difference at risk: ", round(dif_2026less_2021_sf$dif_2026less_2021,2),"<br>",
                            "tract: ", dif_2026less_2021_sf$geoid10
              )) %>%
  # difference 2026 (more restrictive) - 2021 update
  addPolygons(data=dif_2026more_2021_sf,
              stroke = T,
              opacity = 1,
              color = "grey",
              weight = 0.5,
              fillColor = ~pal_dif(dif_2026more_2021_sf$dif_2026more_2021),
              fillOpacity = 1,
              group = "Dif 2026 more restrictive - 2021",
              popup = paste("%hh difference at risk: ", round(dif_2026more_2021_sf$dif_2026more_2021,2),"<br>",
                            "tract: ", dif_2026more_2021_sf$geoid10
              )) %>%
  
  # 2021 update
  addLegend(pal =  pal_2021, 
            # values = disp_2018_sf$per_at_risk_2018by, 
            values = tick_positions,  # force legend to use these values
            opacity = 0.7, 
            group="Old, 2021 update",
            title = htmltools::HTML(paste("Development","<br>", "Capacity (%)", "<br>", 
                                          "2021 update"),"<br><span style='font-size:9px; font-weight:normal;'>--2010 geographies--</span>"),
            position = "bottomright") %>%
  # 2026 less restrictive
  addLegend(pal =  pal_less_2026, 
            # values = less_restrictive_tract_sf$per_at_risk_2023by, 
            values = tick_positions,  # force legend to use these values
            opacity = 0.7, 
            group="Less Restrictive, 2026 update",
            title = htmltools::HTML(paste("Development","<br>", "Capacity (%)", "<br>", 
                                          "2026 update"),"<br><span style='font-size:9px; font-weight:normal;'>--2020 geographies--</span>"),
            position = "bottomright") %>%
  # 2026 more restrictive
  addLegend(pal =  pal_more_2026, 
            # values = more_restrictive_tract_sf$per_at_risk_2023by, 
            values = tick_positions,  # force legend to use these values
            opacity = 0.7, 
            group="More Restrictive, 2026 update",
            title = htmltools::HTML(paste("Development","<br>", "Capacity (%)", "<br>", 
                                          "2026 update"),"<br><span style='font-size:9px; font-weight:normal;'>--2020 geographies--</span>"),
            position = "bottomright") %>% 
  # 2026 difference (less-more restrictive)
  addLegend(pal = pal_dif,
            values = dif_2026_sf$dif_2026,
            opacity = 0.7, 
            group="Dif less-more, 2026 update",
            title = htmltools::HTML(paste("Difference","<br>", "2026 less-more restrictive", "<br>", "Dev. Capacity (%)"),"<br><span style='font-size:9px; font-weight:normal;'>--2020 geographies--</span>"),
            position = "bottomright") %>% 
  # difference 2026 (less restrictive) - 2021 update
  addLegend(pal = pal_dif,
            values = dif_2026less_2021_sf$dif_2026less_2021,
            opacity = 0.7, 
            group="Dif 2026 less restrictive - 2021",
            title = htmltools::HTML(paste("Difference", "<br>", "2026 (less restrictive) - 2021","<br>", "Dev. Capacity (%)"),"<br><span style='font-size:9px; font-weight:normal;'>--2010 geographies--</span>","<br><span style='font-size:9px; font-style:italic;'>positive values (purple) indicates increased risk over time</span>"),
            position = "bottomright") %>% 
  
  # difference 2026 (more restrictive) - 2021 update
  addLegend(pal = pal_dif,
            values = dif_2026more_2021_sf$dif_2026more_2021,
            opacity = 0.7, 
            group="Dif 2026 more restrictive - 2021",
            title = htmltools::HTML(paste("Difference", "<br>", "2026 (more restrictive) - 2021","<br>", " Dev. Capacity (%)"),"<br><span style='font-size:9px; font-weight:normal;'>--2010 geographies--</span>","<br><span style='font-size:9px; font-style:italic;'>positive values (purple) indicates increased risk over time</span>"),
            position = "bottomright") %>% 
  
  #default hide overlap groups
  hideGroup("Old, 2021 update") %>% 
  hideGroup("Less Restrictive, 2026 update") %>% 
  hideGroup("More Restrictive, 2026 update") %>%
  hideGroup("Dif less-more, 2026 update") %>%
  hideGroup("Dif 2026 less restrictive - 2021") %>%
  hideGroup("Dif 2026 more restrictive - 2021") %>%

print(m)






