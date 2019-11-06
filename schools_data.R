####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire School geometry data
# Last updated: 11/04/2019
# From NCES 
# * https://nces.ed.gov/programs/edge/Geographic/SchoolLocations
#
# Geography: Schools in Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson, Buckingham, Madison, Orange
#     (include Augusta, Waynesboro, Staunton?)
####################################################
# 1. Load libraries
# 2. Download data
# 3. Reduce data, add county FIPS
# 4. Combine data file and save as geojson

# 4. Add geography
# 5. Summarize/Examine
# 6. Save
####################################################

# ....................................................
# 1. Load libraries ----
library(tidyverse)
library(sf)

ccode <- read_csv("county_codes.csv")
region <- ccode$code # list of desired counties



# ....................................................
# 2. Download data ----
# public schools
download.file(url = "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_1718.zip",
              destfile = "tempdata/public_schools.zip")
unzip(zipfile = "tempdata/public_schools.zip", exdir = "tempdata/public_schools")
pubschools_sf = st_read(dsn = "tempdata/public_schools/EDGE_GEOCODE_PUBLICSCH_1718/EDGE_GEOCODE_PUBLICSCH_1718.shp")
# geometry type:  POINT
# dimension:      XY
# epsg (SRID):    4269
# proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs

# private schools
download.file(url = "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PRIVATESCH_17_18.zip",
              destfile = "tempdata/private_schools.zip")
unzip(zipfile = "tempdata/private_schools.zip", exdir = "tempdata/private_schools")
privschools_sf = st_read(dsn = "tempdata/private_schools/EDGE_GEOCODE_PRIVATESCH_17_18/EDGE_GEOCODE_PRIVATESCH_1718.shp")
# geometry type:  POINT
# dimension:      XY
# epsg (SRID):    4269
# proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs

# # public school districts
# download.file(url = "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICLEA_1718.zip",
#               destfile = "tempdata/school_district.zip")
# unzip(zipfile = "tempdata/school_district.zip", exdir = "tempdata/school_district")
# schooldist_sf = st_read(dsn = "tempdata/school_district/EDGE_GEOCODE_PUBLICLEA_1718/EDGE_GEOCODE_PUBLICLEA_1718.shp")
# # geometry type:  POINT
# # dimension:      XY
# # epsg (SRID):    4269
# # proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs


# ....................................................
# 3. Reduce data, add county FIPS ----
# public schools
pubschools_sf <- pubschools_sf %>% 
  filter(STATE == "VA") 

pubschools_sf$STATE <- droplevels(pubschools_sf$STATE)
pubschools_sf$CNTY <- droplevels(pubschools_sf$CNTY)

pubschools_sf <- pubschools_sf %>% 
  mutate(county = substr(CNTY, 3,5)) %>% 
  filter(county %in% region) %>% 
  mutate(type = "public") %>% 
  rename(id = NCESSCH) %>% 
  select(id:LON, type, county, geometry, -OPSTFIPS)

# private schools
privschools_sf <- privschools_sf %>% 
  filter(STATE == "VA")

privschools_sf$STATE <- droplevels(privschools_sf$STATE)
privschools_sf$CNTY <- droplevels(privschools_sf$CNTY)

privschools_sf <- privschools_sf %>% 
  mutate(county = substr(CNTY, 3,5)) %>% 
  filter(county %in% region) %>% 
  mutate(type = "private") %>% 
  rename(id = PPIN) %>% 
  select(id:LON, type, county, geometry)


plot(pubschools_sf[,1])
plot(privschools_sf[,1])


# ....................................................
# 4. Combine data files and save as geojson
# combine public and private schools
schools_sf <- rbind(pubschools_sf, privschools_sf)

# For parks I had coordinate system 4326; these seem to have 4269; do I need to change them?
schools_sf <- st_transform(schools_sf, 4326)

# Save as geojson
st_write(schools_sf, "data/schools_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE) 
# st_crs(schools_sf)
# schools_sf <- st_read("data/schools_sf.geojson") 
