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
# 1. Load libraries, provide api key (if needed), identify variables
# 2. Define variables, pull tables, derive estimates
# 3. Reduce and combine
# 4. Add geography
# 5. Summarize/Examine
# 6. Save
####################################################

# ....................................................
# 1. Load libraries, provide api key (if needed), identify variables ----

# Load libraries
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

# Census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# Variable view helper
# acs_var <- load_variables(2017, "acs5", cache = TRUE)
# acs_var <- load_variables(2017, "acs5/subject", cache = TRUE)
# acs_var <- load_variables(2017, "acs5/profile", cache = TRUE)

# Variable of interest -
##  - Total population -- B01003_001
##  - Poverty rate -- S1701_C03_001
##  - Child poverty rate -- S1701_C03_002
##  - Median HH Income -- S1901_C01_012	
##  - Gini Index of Income Inequality -- B19083_001
##  - Percent high school graduate or higher -- S1501_C02_014
##  - Percent bachelor's degree or higher -- S1501_C02_015
##  - Percent white alone -- DP05_0077P
##  - Percent black or African American alone -- DP05_0078P
##  - Percent American Indian and Alaska Native alone -- DP05_0079P
##  - Percent Asian alone -- DP05_0080P
##  - Percent Native Hawaiian and Other Pacific Islander alone -- DP05_0081P
##  - Percent Some other race alone -- DP05_0082P
##  - Percent Two or more races -- DP05_0083P
##  - Percent Hispanic or Latino -- DP05_0071P
##  - Percent unemployment (Population 16 and over) -- S2301_C04_001
##  - Percent with health insurance (Civilian noninstitutionalized population) -- S2701_C03_001	
##  - Percent with public health insurance (Civilian noninstitutionalized population) -- S2704_C03_001
##  - Age, population under 18 -- S0101_C02_022
##  - Age, population 18 to 24 -- S0101_C02_023	
##  - Age, 26 to 64 -- sum(S0101_C02_007, S0101_C02_008, S0101_C02_009, S0101_C02_010, S0101_C02_011, S0101_C02_012, S0101_C02_013, S0101_C02_014)
##  - Age, 65 and over --S0101_C02_030


# ....................................................
# 2. Define localities, variables, pull tables ----
ccode <- read_csv("county_codes.csv")
region <- ccode$code # list of desired counties

# Pull data
# variables
tract_pop <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(totalpop = "B01003_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

tract_pov <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(povrate = "S1701_C03_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

tract_cpov <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(cpovrate = "S1701_C03_002"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

tract_hhinc <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(hhinc = "S1901_C01_012"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

tract_gini <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(gini = "B19083_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

tract_hs <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(hsmore = "S1501_C02_014"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

tract_ba <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(bamore = "S1501_C02_015"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

tract_unemp <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(unemp = "S2301_C04_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

tract_hlthins <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(hlthins = "S2701_C03_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

tract_pubins <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          variables = c(pubins = "S2704_C03_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

# tables
tract_race <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          table = "DP05", 
          state = "VA", county = x, survey = "acs5")
})

tract_age <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          table = "S0101", 
          state = "VA", county = x, survey = "acs5")
})

# Can I iterate through localities and variables to do variables in one function?
# And the tables in one function?

# This doesn't work
# tract_test <- map_df(region, function(x) {
#   get_acs(geography = "tract", 
#           variables = c(hsmore = "S1501_C01_014",
#                         bamore = "S1501_C01_015"), 
#           state = "VA", county = x, survey = "acs5",
#           output = "wide")
# })


# ....................................................
# 3. Reduce and Combine data ----

# Reduce tables: tract_age, tract_race
# tract_age: three age groups present as rows in the table,
#             one group must be summed
tract_age17 <- tract_age %>% 
  filter(variable == "S0101_C02_022") %>% 
  rename(age17E = estimate,
         age17M = moe) %>% 
  select(-variable)

tract_age24 <- tract_age %>% 
  filter(variable == "S0101_C02_023") %>% 
  rename(age24E = estimate,
         age24M = moe) %>% 
  select(-variable)

tract_age64 <- tract_age %>% 
  filter(variable %in% c("S0101_C02_007", "S0101_C02_008", "S0101_C02_009",
                         "S0101_C02_010", "S0101_C02_011", "S0101_C02_012",
                         "S0101_C02_013", "S0101_C02_014")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(age64E = sum(estimate),
            age64M = moe_sum(moe = moe, estimate = estimate))

tract_age65 <- tract_age %>% 
  filter(variable == "S0101_C02_030") %>% 
  rename(age65E = estimate,
         age65M = moe) %>% 
  select(-variable)

# tract_race: all groups present as rows in the table
#             but other race and native hawaiian/pacific islander combined
#             due to very small values
tract_white <- tract_race %>% 
  filter(variable == "DP05_0077P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  select(-variable)

tract_black <- tract_race %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  select(-variable)

tract_indig <- tract_race %>% 
  filter(variable == "DP05_0079P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  select(-variable)

tract_asian <- tract_race %>% 
  filter(variable == "DP05_0080P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  select(-variable)

tract_othrace <- tract_race %>% 
  filter(variable %in% c("DP05_0081P", "DP05_0082P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate))

tract_multi <- tract_race %>% 
  filter(variable == "DP05_0083P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  select(-variable)

tract_ltnx <- tract_race %>% 
  filter(variable == "DP05_0071P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  select(-variable)


# Combine indicators
# joining columns
tract_data <- tract_pop %>% 
  left_join(tract_pov) %>% 
  left_join(tract_cpov) %>% 
  left_join(tract_hhinc) %>% 
  left_join(tract_gini) %>% 
  left_join(tract_hs) %>% 
  left_join(tract_ba) %>% 
  left_join(tract_unemp) %>% 
  left_join(tract_hlthins) %>% 
  left_join(tract_pubins) %>% 
  left_join(tract_white) %>% 
  left_join(tract_black) %>% 
  left_join(tract_indig) %>% 
  left_join(tract_asian) %>% 
  left_join(tract_othrace) %>% 
  left_join(tract_multi) %>% 
  left_join(tract_ltnx) %>% 
  left_join(tract_age17) %>% 
  left_join(tract_age24) %>% 
  left_join(tract_age64) %>% 
  left_join(tract_age65)

tract_data <- tract_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality", "tract"), 
                  sep = c(2,5)) 


# ....................................................
# 4. Summarize/Examine indicators ----
tract_data %>% select_at(vars(ends_with("E"))) %>% summary()

# Ablemarle tract 109.03 is UVA -- 
#   highest poverty rate, and gini coefficient; missing child pov and hhinc
#   make this NA for all economic variables
tract_data <- tract_data %>% 
  mutate(povrateE = ifelse(GEOID == "51003010903", NA_integer_, povrateE),
         giniE = ifelse(GEOID == "51003010903", NA_integer_, giniE))

ggplot(tract_data, aes(x = bamoreE, y = giniE)) + 
  geom_point() + geom_smooth()


# ....................................................
# 5. Add geography ----
# get tract polygons
geo <- tracts(state = 'VA', county = region)

# join coordinates to data
tract_data_geo <- geo_join(geo, tract_data, by = "GEOID")

# add centroid coordinates for tract polygons
# as possible way of visualizing/layering a second attribute
library(geosphere)
tract_data_geo$ctr <- centroid(tract_data_geo)
tract_data_geo$lng <- tract_data_geo$ctr[,1]
tract_data_geo$lat <- tract_data_geo$ctr[,2]

# get locality polygons
counties <- counties(state = 'VA')
counties <- counties %>% subset(COUNTYFP %in% region)


# ....................................................
# 6. Save ----
# save rcaa_recap and rcaa_recap_geo
saveRDS(tract_data, file = "tract_data.RDS") 
saveRDS(tract_data_geo, file = "tract_data_geo.RDS")
saveRDS(counties, file = "county_geo.RDS")
# tract_data <- readRDS("tract_data.RDS")
# tract_data_geo <- readRDS("tract_data_geo.RDS")
# counties_geo <- readRDS("county_geo.RDS")
