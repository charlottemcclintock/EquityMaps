####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire ACS data
# Last updated: 10/27/2019
# Metrics from ACS (in common with tract level): 
# * Total population
# * Poverty, child poverty 
# * Median HH Income, Gini income inequality index
# * Educational attainment: HS and more, BA and more
# * Unemployment 
# * Health insurance, and Public health insurance
# * Race/ethnicity: White (NH), Black, Asian, Hispanic, Indigenous, Multiracial, Other
# * Age groups: 0-17, 18-24, 25-64, 65 or more
#
# Metrics specific to locality level (from Decennial or ACS):
# * Median HH Income by Race/Ethnicity
# 
# Based on: ACS 2013-2017 (currently, can update in December/January)
# Geography: Localities in Charlottesville region
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
##  - Median HH Income by Race --  B19013B_001 (Black alone),  B19013D_001 (Asian alone), 
##                                 B19013G_001 (Two or more races), B19013H_001 (NH White alone),  
##                                 B19013I_001 (Hispanic)


# ....................................................
# 2. Define localities, variables, pull tables ----
ccode <- read_csv("county_codes.csv")
region <- ccode$code # list of desired counties

# Pull data
# variables
county_pop <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(totalpop = "B01003_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_pov <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(povrate = "S1701_C03_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_cpov <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(cpovrate = "S1701_C03_002"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_hhinc <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(hhinc = "S1901_C01_012"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_gini <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(gini = "B19083_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_hs <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(hsmore = "S1501_C02_014"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_ba <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(bamore = "S1501_C02_015"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_unemp <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(unemp = "S2301_C04_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_hlthins <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(hlthins = "S2701_C03_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_pubins <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(pubins = "S2704_C03_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

# tables
county_race <- map_df(region, function(x) {
  get_acs(geography = "county", 
          table = "DP05", 
          state = "VA", county = x, survey = "acs5")
})

county_age <- map_df(region, function(x) {
  get_acs(geography = "county", 
          table = "S0101", 
          state = "VA", county = x, survey = "acs5")
})


# Can I iterate through localities and variables to get desired variables in one function?
# And the desired tables in one function?

# This doesn't work
# tract_test <- map_df(region, function(x) {
#   get_acs(geography = "county", 
#           variables = c(hsmore = "S1501_C01_014",
#                         bamore = "S1501_C01_015"), 
#           state = "VA", county = x, survey = "acs5",
#           output = "wide")
# })


# ....................................................
# 3. Metrics specific to locality level  ----
county_hhinc_black <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(hhinc_black = "B19013B_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_hhinc_asian <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(hhinc_asian = "B19013D_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_hhinc_multi <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(hhinc_multi = "B19013G_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_hhinc_white <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(hhinc_white = "B19013H_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})

county_hhinc_hisp <- map_df(region, function(x) {
  get_acs(geography = "county", 
          variables = c(hhinc_hisp = "B19013I_001"), 
          state = "VA", county = x, survey = "acs5",
          output = "wide")
})


# ....................................................
# 4. Reduce and Combine data ----

# Reduce tables: tract_age, tract_race
# tract_age: three age groups present as rows in the table,
#             one group must be summed
county_age17 <- county_age %>% 
  filter(variable == "S0101_C02_022") %>% 
  rename(age17E = estimate,
         age17M = moe) %>% 
  select(-variable)

county_age24 <- county_age %>% 
  filter(variable == "S0101_C02_023") %>% 
  rename(age24E = estimate,
         age24M = moe) %>% 
  select(-variable)

county_age64 <- county_age %>% 
  filter(variable %in% c("S0101_C02_007", "S0101_C02_008", "S0101_C02_009",
                         "S0101_C02_010", "S0101_C02_011", "S0101_C02_012",
                         "S0101_C02_013", "S0101_C02_014")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(age64E = sum(estimate),
            age64M = moe_sum(moe = moe, estimate = estimate))

county_age65 <- county_age %>% 
  filter(variable == "S0101_C02_030") %>% 
  rename(age65E = estimate,
         age65M = moe) %>% 
  select(-variable)

# tract_race: all groups present as rows in the table
#             but other race and native hawaiian/pacific islander combined
#             due to very small values
county_white <- county_race %>% 
  filter(variable == "DP05_0077P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  select(-variable)

county_black <- county_race %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  select(-variable)

county_indig <- county_race %>% 
  filter(variable == "DP05_0079P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  select(-variable)

county_asian <- county_race %>% 
  filter(variable == "DP05_0080P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  select(-variable)

county_othrace <- county_race %>% 
  filter(variable %in% c("DP05_0081P", "DP05_0082P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate))

county_multi <- county_race %>% 
  filter(variable == "DP05_0083P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  select(-variable)

county_ltnx <- county_race %>% 
  filter(variable == "DP05_0071P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  select(-variable)


# Combine indicators
# joining columns
county_data <- county_pop %>% 
  left_join(county_pov) %>% 
  left_join(county_cpov) %>% 
  left_join(county_hhinc) %>% 
  left_join(county_gini) %>% 
  left_join(county_hs) %>% 
  left_join(county_ba) %>% 
  left_join(county_unemp) %>% 
  left_join(county_hlthins) %>% 
  left_join(county_pubins) %>% 
  left_join(county_white) %>% 
  left_join(county_black) %>% 
  left_join(county_indig) %>% 
  left_join(county_asian) %>% 
  left_join(county_othrace) %>% 
  left_join(county_multi) %>% 
  left_join(county_ltnx) %>% 
  left_join(county_age17) %>% 
  left_join(county_age24) %>% 
  left_join(county_age64) %>% 
  left_join(county_age65) %>% 
  left_join(county_hhinc_black) %>% 
  left_join(county_hhinc_white) %>% 
  left_join(county_hhinc_asian) %>% 
  left_join(county_hhinc_multi) %>% 
  left_join(county_hhinc_hisp)

county_data <- county_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality"), 
           sep = c(2)) 


# ....................................................
# 5. Summarize/Examine indicators ----
county_data %>% select_at(vars(ends_with("E"))) %>% summary()


# ....................................................
# 6. Add geography ----
# get locality polygons
counties <- counties(state = 'VA')
counties <- counties %>% subset(COUNTYFP %in% region)

# join coordinates to data
county_data_geo <- geo_join(counties, county_data, by = "GEOID")

# add centroid coordinates for tract polygons
# as possible way of visualizing/layering a second attribute
library(geosphere)
county_data_geo$ctr <- centroid(county_data_geo)
county_data_geo$lng <- county_data_geo$ctr[,1]
county_data_geo$lat <- county_data_geo$ctr[,2]


# ....................................................
# 7. Save ----
# save rcaa_recap and rcaa_recap_geo
saveRDS(county_data, file = "data/county_data.RDS") 
saveRDS(county_data_geo, file = "data/county_data_geo.RDS")
saveRDS(counties, file = "data/county_geo.RDS")
# county_data <- readRDS("data/county_data.RDS")
# county_data_geo <- readRDS("data/county_data_geo.RDS")

