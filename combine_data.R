####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Combine data for shiny app
# Last updated: 10/27/2019
####################################################
# 1. Load libraries 
# 2. Load data
# 3. Merge tract attributes, county attributes
# 4. Add geography
# 5. Read in crosswalk and join
# 6. Define color palettes
# 7. Save for app
####################################################


# ....................................................
# 1. Load libraries and data ----
# Libraries
library(tidyverse)
library(RColorBrewer)
library(googlesheets)
library(sf)
library(tools)
library(tigris)
# options(tigris_use_cache = TRUE)
library(sp)
library(geosphere)


# function to move variables to end
move_last <- function(DF, last_col) {
  match(c(setdiff(names(DF), last_col), last_col), names(DF))
}


# ....................................................
# 2. Load data ----
# tract level ACS
tract_data <- readRDS("data/tract_data.RDS")

# county level ACS
county_data <- readRDS("data/county_data.RDS")

# additional tract/county data
life_exp <- readRDS("data/tract_life_exp.RDS")
seg_county <- readRDS("data/seg_county.RDS")

# points and polygons
parks_sf <- st_read("data/parks_sf.geojson") 
schools_sf <- st_read("data/schools_sf.geojson") # may want to segment by type (public, private)
# other files as needed: polygons and points

ccode <- read_csv("county_codes.csv")
region <- ccode$code # list of desired counties


# ....................................................
# 2. Merge tract attributes, county attributes ----
# add life expectancy by tract
life_exp <- life_exp %>% select(geoid, year, life_expE, life_expM) %>% 
  mutate(geoid = as.character(geoid))

tract_data <- tract_data %>% 
  left_join(life_exp, by = c("GEOID" = "geoid", "year" = "year")) %>% 
  select(move_last(., c("state", "locality", "tract")))

# add segregation measures by county
county_data <- county_data %>% 
  left_join(seg_county, by = c("locality" = "county")) %>% 
  select(move_last(., c("state", "locality")))


# ....................................................
# 3. Read in crosswalk and join ----
# read pretty table
# gs_auth(new_user = TRUE)
prettytab <- gs_title("prettytable")
pretty <- gs_read(prettytab, ws = "acs_tract")
pretty$goodname <- toTitleCase(pretty$description)
pretty$description <- as.character(pretty$description)
pretty <- data.frame(pretty)

# prettytab <- gs_title("prettytable")
pretty2 <- gs_read(prettytab, ws = "acs_county")
pretty2$goodname <- toTitleCase(pretty2$description)

# join pretty names to existing tract data
tab <- select(tract_data, locality, NAME)
tab <- separate(tab, NAME,
                into=c("tract","county.nice", "state"), sep=", ", remove=F)

tab <- unique(select(tab, locality, county.nice))
tract_data <- left_join(tract_data, tab, by="locality")

# join pretty names to existing county data
tab2 <- select(county_data, locality, NAME)
tab2 <- separate(tab2, NAME,
                into=c("county.nice", "state"), sep=", ", remove=F)

tab2 <- unique(select(tab2, locality, county.nice))
county_data <- left_join(county_data, tab2, by="locality")


# ....................................................
# 4. Add geography  ----
# get tract polygons
geo <- tracts(state = 'VA', county = region) # from tigris

# join coordinates to data
tract_data_geo <- merge(geo, tract_data, by = "GEOID", duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)
tract_data_geo2 <- geo_join(geo, tract_data, by = "GEOID") # from sf -- keep only 2017 obs (left_join)

# add centroid coordinates for tract polygons: from geosphere
# as possible way of visualizing/layering a second attribute
tract_data_geo$ctr <- centroid(tract_data_geo)
tract_data_geo$lng <- tract_data_geo$ctr[,1]
tract_data_geo$lat <- tract_data_geo$ctr[,2]

tract_data_geo2$ctr <- centroid(tract_data_geo2)
tract_data_geo2$lng <- tract_data_geo2$ctr[,1]
tract_data_geo2$lat <- tract_data_geo2$ctr[,2]

# get locality polygons
counties_geo <- counties(state = 'VA') # from tigris
counties_geo <- counties_geo %>% subset(COUNTYFP %in% region)

# join coordinates to data
county_data_geo <- geo_join(counties_geo, county_data, by = "GEOID")

# add centroid coordinates for tract polygons
# as possible way of visualizing/layering a second attribute
county_data_geo$ctr <- centroid(county_data_geo)
county_data_geo$lng <- county_data_geo$ctr[,1]
county_data_geo$lat <- county_data_geo$ctr[,2]


# ....................................................
# 5. Define color palettes ----
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "YlGnBu"))(nb.cols)


# ....................................................
# 6. Save for app ----
rm(ccode, geo, life_exp, prettytab, seg_county, tab, tab2, region, move_last)

save.image(file = "data/app_data.Rdata")
# load("data/app_data.Rdata")

