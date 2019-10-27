####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Last updated: 9/26/2019
# Metrics from ACS: 
# * Total population
# * Poverty, child poverty 
# * HH Income, income inequality
# * Educational attainment 
# * Unemployment 
# * Health insurance 
# * Race/ethnicity 
# * Age groups
# Based on: ACS 2013-2017 (currently, can update in December/January)
# Geography: Tracts and Localities in Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson, Buckingham, Madison, Orange
#     (include Augusta, Waynesboro, Staunton?)
####################################################
# 1. Load libraries and data
# 2. Map
####################################################


# ....................................................
# 1. Load libraries and data ----

# Load libraries
library(tidyverse)
library(leaflet)
library(RColorBrewer)

# Load data
tract_data <- readRDS("tract_data.RDS")
tract_data_geo <- readRDS("tract_data_geo.RDS")
counties_geo <- readRDS("county_geo.RDS")


# ....................................................
# 2. Map ----
# play with leaflet -- map two attributes

# define two palettes
nb.cols <- 10

mycolors <- colorRampPalette(brewer.pal(8, "YlGnBu"))(nb.cols)
pal <- colorNumeric(palette = mycolors,
                    domain = tract_data_geo$blackE)

mycolors2 <- colorRampPalette(brewer.pal(8, "BrBG"))(nb.cols)
pal2 <- colorNumeric(palette = mycolors2,
                     domain = tract_data_geo$giniE)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # addTiles() %>% # to show streets more prominently
  addPolygons(data = tract_data_geo, 
              fillColor = ~pal(tract_data_geo$blackE), 
              fillOpacity = 0.5, 
              color = "white",
              weight = 2, 
              smoothFactor = 0.2, 
              # popup = popup,
              highlight = highlightOptions(
                weight = 5,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addPolygons(data = counties_geo,
              color = "grey",
              fillOpacity = 0,
              weight = 2) %>% 
  addCircles(data = tract_data_geo, ~lng, ~lat,
             radius = ~(giniE*2000), 
             color = ~pal2(giniE)) %>% 
  addLegend(pal = pal, 
            values = tract_data_geo$blackE,
            position = "topright", 
            opacity = 0.25,
            title = "Percent Black") %>% 
  addLegend(pal = pal2,
            values = tract_data_geo$giniE,
            position = "bottomright",
            opacity = 0.25,
            title = "Gini Coefficient")

# Not sure if leaflet/shiny will be the way to go 
# look into other spatial libraries?
