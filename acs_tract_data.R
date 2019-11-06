####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire ACS data
# Last updated: 10/28/2019
# Metrics from ACS (in common with locality level): 
# * Total population
# * Poverty, child poverty 
# * Median HH Income, Gini income inequality index
# * Educational attainment: HS and more, BA and more
# * Unemployment 
# * Health insurance, and Public health insurance
# * Race/ethnicity: White (NH), Black, Asian, Hispanic, Indigenous, Multiracial, Other
# * Age groups: 0-17, 18-24, 25-64, 65 or more
# 
# Metrics specific to tract level (from Decennial and ACS):
# * Race/Ethnicity and Total population over time: Decennial 1990, 2000, 2010; ACS 2011-2017
#
# Based on: ACS 2013-2017 (currently, can update in December/January)
# Geography: Tracts in Localities in Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson, Buckingham, Madison, Orange
#     (include Augusta, Waynesboro, Staunton?)
####################################################
# 1. Load libraries, provide api key (if needed), identify variables
# 2. Define variables, pull tables, derive estimates
# 3. Metrics specific to tract level
# 4. Reduce and combine
# 5. Add geography
# 6. Summarize/Examine
# 7. Save
####################################################


# ....................................................
# 1. Load libraries, provide api key (if needed), identify variables ----

# Load libraries
library(tidyverse)
library(tidycensus)
library(tigris)
# options(tigris_use_cache = TRUE)

# Census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# Variable view helper
# acs_var <- load_variables(2017, "acs5", cache = TRUE)
# acs_var <- load_variables(2017, "acs5/subject", cache = TRUE)
# acs_var <- load_variables(2017, "acs5/profile", cache = TRUE)
# dec_var <- load_variables(2010, "sf1", cache = TRUE)

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


# Can I iterate through localities and variables to get desired variables in one function?
# And the desired tables in one function?

# This doesn't work
# tract_test <- map_df(region, function(x) {
#   get_acs(geography = "tract", 
#           variables = c(hsmore = "S1501_C01_014",
#                         bamore = "S1501_C01_015"), 
#           state = "VA", county = x, survey = "acs5",
#           output = "wide")
# })


# ....................................................
# 3. Metrics specific to tract level ----

# Population

# 2010 decennial pop
tract_pop_10 <- map_df(region, function(x) {
  get_decennial(geography = "tract", 
                             variables = c("2010" = "P003001"), 
                             state = "VA", 
                             county = x, year = 2010)
})

tract_pop_10 <- tract_pop_10 %>% 
  rename(year = variable, totalpopE = value)

# 2011-2016 acs pop
# want to loop this over years, like so: https://mattherman.info/blog/tidycensus-mult/

tract_pop_11 <- map_df(region, function(x) {
  get_acs(geography = "tract",
          variables = c("2011" = "B01003_001"),
          state = "VA", county = x, survey = "acs5",
          year = 2011)
})

tract_pop_11 <- tract_pop_11 %>% 
  rename(year = variable, totalpopE = estimate, totalpopM = moe)

tract_pop_12 <- map_df(region, function(x) {
  get_acs(geography = "tract",
          variables = c("2012" = "B01003_001"),
          state = "VA", county = x, survey = "acs5",
          year = 2012)
})

tract_pop_12 <- tract_pop_12 %>% 
  rename(year = variable, totalpopE = estimate, totalpopM = moe)

tract_pop_13 <- map_df(region, function(x) {
  get_acs(geography = "tract",
          variables = c("2013" = "B01003_001"),
          state = "VA", county = x, survey = "acs5",
          year = 2013)
})

tract_pop_13 <- tract_pop_13 %>% 
  rename(year = variable, totalpopE = estimate, totalpopM = moe)

tract_pop_14 <- map_df(region, function(x) {
  get_acs(geography = "tract",
          variables = c("2014" = "B01003_001"),
          state = "VA", county = x, survey = "acs5",
          year = 2014)
})

tract_pop_14 <- tract_pop_14 %>% 
  rename(year = variable, totalpopE = estimate, totalpopM = moe)

tract_pop_15 <- map_df(region, function(x) {
  get_acs(geography = "tract",
          variables = c("2015" = "B01003_001"),
          state = "VA", county = x, survey = "acs5",
          year = 2015)
})

tract_pop_15 <- tract_pop_15 %>% 
  rename(year = variable, totalpopE = estimate, totalpopM = moe)

tract_pop_16 <- map_df(region, function(x) {
  get_acs(geography = "tract",
          variables = c("2016" = "B01003_001"),
          state = "VA", county = x, survey = "acs5",
          year = 2016)
})

tract_pop_16 <- tract_pop_16 %>% 
  rename(year = variable, totalpopE = estimate, totalpopM = moe)

# combine
tract_pop_10_16 <- bind_rows(tract_pop_10, tract_pop_11, tract_pop_12, tract_pop_13, tract_pop_14, tract_pop_15, tract_pop_16)


# Race
# 2010 decennial race
tract_white_10 <- map_df(region, function(x) {
  get_decennial(geography = "tract",
                variables = c("2010" = "P005003"),
                state = "VA", county = x, year = 2010, 
                summary_var = "P005001")
})

tract_white_10 <- tract_white_10 %>% 
  mutate(whiteE = (value/summary_value)*100) %>% 
  rename(year = variable) %>% 
  select(-value, -summary_value)

tract_black_10 <- map_df(region, function(x) {
  get_decennial(geography = "tract",
                variables = c("2010" = "P005004"),
                state = "VA", county = x, year = 2010,
                summary_var = "P005001")
})

tract_black_10 <- tract_black_10 %>% 
  mutate(blackE = (value/summary_value)*100) %>% 
  rename(year = variable) %>% 
  select(-value, -summary_value)

tract_indig_10 <- map_df(region, function(x) {
  get_decennial(geography = "tract",
                variables = c("2010" = "P005005"),
                state = "VA", county = x, year = 2010,
                summary_var = "P005001")
})

tract_indig_10 <- tract_indig_10 %>% 
  mutate(indigE = (value/summary_value)*100) %>% 
  rename(year = variable) %>% 
  select(-value, -summary_value)

tract_asian_10 <- map_df(region, function(x) {
  get_decennial(geography = "tract",
                variables = c("2010" = "P005006"),
                state = "VA", county = x, year = 2010,
                summary_var = "P005001")
})

tract_asian_10 <- tract_asian_10 %>% 
  mutate(asianE = (value/summary_value)*100) %>% 
  rename(year = variable) %>% 
  select(-value, -summary_value)

tract_othrace_10 <- map_df(region, function(x) {
  get_decennial(geography = "tract",
                variables = c("oth1" = "P005007", "oth2" = "P005008"),
                state = "VA", county = x, year = 2010,
                summary_var = "P005001")
})

tract_othrace_10 <- tract_othrace_10 %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(year = "2010") %>% 
  mutate(othraceE = ((oth1 + oth2)/summary_value)*100) %>%
  mutate(othraceE = round(othraceE,2)) %>% 
  select(-oth1, -oth2, -summary_value)

tract_multi_10 <- map_df(region, function(x) {
  get_decennial(geography = "tract",
                variables = c("2010" = "P005009"),
                state = "VA", county = x, year = 2010,
                summary_var = "P005001")
})

tract_multi_10 <- tract_multi_10 %>% 
  mutate(multiE = (value/summary_value)*100) %>% 
  rename(year = variable) %>% 
  select(-value, -summary_value)

tract_ltnx_10 <- map_df(region, function(x) {
  get_decennial(geography = "tract",
                variables = c("2010" = "P005010"),
                state = "VA", county = x, year = 2010,
                summary_var = "P005001")
})

tract_ltnx_10 <- tract_ltnx_10 %>% 
  mutate(ltnxE = (value/summary_value)*100) %>% 
  rename(year = variable) %>% 
  select(-value, -summary_value)


# 2011-2016 acs race
tract_race_11 <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          table = "DP05", 
          state = "VA", county = x, survey = "acs5", year = 2011)
})

tract_white_11 <- tract_race_11 %>% 
  filter(variable == "DP05_0072P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  mutate(year = "2011") %>% 
  select(GEOID, NAME, year, whiteE, whiteM)

tract_black_11 <- tract_race_11 %>% 
  filter(variable == "DP05_0073P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  mutate(year = "2011") %>% 
  select(GEOID, NAME, year, blackE, blackM)

tract_indig_11 <- tract_race_11 %>% 
  filter(variable == "DP05_0074P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  mutate(year = "2011") %>% 
  select(GEOID, NAME, year, indigE, indigM)

tract_asian_11 <- tract_race_11 %>% 
  filter(variable == "DP05_0075P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  mutate(year = "2011") %>% 
  select(GEOID, NAME, year, asianE, asianM)

tract_othrace_11 <- tract_race_11 %>% 
  filter(variable %in% c("DP05_0076P", "DP05_0077P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(year = "2011") %>% 
  select(GEOID, NAME, year, othraceE, othraceM)

tract_multi_11 <- tract_race_11 %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  mutate(year = "2011") %>% 
  select(GEOID, NAME, year, multiE, multiM)

tract_ltnx_11 <- tract_race_11 %>% 
  filter(variable == "DP05_0066P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  mutate(year = "2011") %>% 
  select(GEOID, NAME, year, ltnxE, ltnxM)

tract_race_12 <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          table = "DP05", 
          state = "VA", county = x, survey = "acs5", year = 2012)
})

tract_white_12 <- tract_race_12 %>% 
  filter(variable == "DP05_0072P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  mutate(year = "2012") %>% 
  select(GEOID, NAME, year, whiteE, whiteM)

tract_black_12 <- tract_race_12 %>% 
  filter(variable == "DP05_0073P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  mutate(year = "2012") %>% 
  select(GEOID, NAME, year, blackE, blackM)

tract_indig_12 <- tract_race_12 %>% 
  filter(variable == "DP05_0074P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  mutate(year = "2012") %>% 
  select(GEOID, NAME, year, indigE, indigM)

tract_asian_12 <- tract_race_12 %>% 
  filter(variable == "DP05_0075P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  mutate(year = "2012") %>% 
  select(GEOID, NAME, year, asianE, asianM)

tract_othrace_12 <- tract_race_12 %>% 
  filter(variable %in% c("DP05_0076P", "DP05_0077P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(year = "2012") %>% 
  select(GEOID, NAME, year, othraceE, othraceM)

tract_multi_12 <- tract_race_12 %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  mutate(year = "2012") %>% 
  select(GEOID, NAME, year, multiE, multiM)

tract_ltnx_12 <- tract_race_12 %>% 
  filter(variable == "DP05_0066P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  mutate(year = "2012") %>% 
  select(GEOID, NAME, year, ltnxE, ltnxM)


tract_race_13 <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          table = "DP05", 
          state = "VA", county = x, survey = "acs5", year = 2013)
})

tract_white_13 <- tract_race_13 %>% 
  filter(variable == "DP05_0072P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  mutate(year = "2013") %>% 
  select(GEOID, NAME, year, whiteE, whiteM)

tract_black_13 <- tract_race_13 %>% 
  filter(variable == "DP05_0073P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  mutate(year = "2013") %>% 
  select(GEOID, NAME, year, blackE, blackM)

tract_indig_13 <- tract_race_13 %>% 
  filter(variable == "DP05_0074P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  mutate(year = "2013") %>% 
  select(GEOID, NAME, year, indigE, indigM)

tract_asian_13 <- tract_race_13 %>% 
  filter(variable == "DP05_0075P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  mutate(year = "2013") %>% 
  select(GEOID, NAME, year, asianE, asianM)

tract_othrace_13 <- tract_race_13 %>% 
  filter(variable %in% c("DP05_0076P", "DP05_0077P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(year = "2013") %>% 
  select(GEOID, NAME, year, othraceE, othraceM)

tract_multi_13 <- tract_race_13 %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  mutate(year = "2013") %>% 
  select(GEOID, NAME, year, multiE, multiM)

tract_ltnx_13 <- tract_race_13 %>% 
  filter(variable == "DP05_0066P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  mutate(year = "2013") %>% 
  select(GEOID, NAME, year, ltnxE, ltnxM)


tract_race_14 <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          table = "DP05", 
          state = "VA", county = x, survey = "acs5", year = 2014)
})

tract_white_14 <- tract_race_14 %>% 
  filter(variable == "DP05_0072P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  mutate(year = "2014") %>% 
  select(GEOID, NAME, year, whiteE, whiteM)

tract_black_14 <- tract_race_14 %>% 
  filter(variable == "DP05_0073P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  mutate(year = "2014") %>% 
  select(GEOID, NAME, year, blackE, blackM)

tract_indig_14 <- tract_race_14 %>% 
  filter(variable == "DP05_0074P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  mutate(year = "2014") %>% 
  select(GEOID, NAME, year, indigE, indigM)

tract_asian_14 <- tract_race_14 %>% 
  filter(variable == "DP05_0075P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  mutate(year = "2014") %>% 
  select(GEOID, NAME, year, asianE, asianM)

tract_othrace_14 <- tract_race_14 %>% 
  filter(variable %in% c("DP05_0076P", "DP05_0077P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(year = "2014") %>% 
  select(GEOID, NAME, year, othraceE, othraceM)

tract_multi_14 <- tract_race_14 %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  mutate(year = "2014") %>% 
  select(GEOID, NAME, year, multiE, multiM)

tract_ltnx_14 <- tract_race_14 %>% 
  filter(variable == "DP05_0066P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  mutate(year = "2014") %>% 
  select(GEOID, NAME, year, ltnxE, ltnxM)


tract_race_15 <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          table = "DP05", 
          state = "VA", county = x, survey = "acs5", year = 2015)
})

tract_white_15 <- tract_race_15 %>% 
  filter(variable == "DP05_0072P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  mutate(year = "2015") %>% 
  select(GEOID, NAME, year, whiteE, whiteM)

tract_black_15 <- tract_race_15 %>% 
  filter(variable == "DP05_0073P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  mutate(year = "2015") %>% 
  select(GEOID, NAME, year, blackE, blackM)

tract_indig_15 <- tract_race_15 %>% 
  filter(variable == "DP05_0074P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  mutate(year = "2015") %>% 
  select(GEOID, NAME, year, indigE, indigM)

tract_asian_15 <- tract_race_15 %>% 
  filter(variable == "DP05_0075P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  mutate(year = "2015") %>% 
  select(GEOID, NAME, year, asianE, asianM)

tract_othrace_15 <- tract_race_15 %>% 
  filter(variable %in% c("DP05_0076P", "DP05_0077P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(year = "2015") %>% 
  select(GEOID, NAME, year, othraceE, othraceM)

tract_multi_15 <- tract_race_15 %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  mutate(year = "2015") %>% 
  select(GEOID, NAME, year, multiE, multiM)

tract_ltnx_15 <- tract_race_15 %>% 
  filter(variable == "DP05_0066P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  mutate(year = "2015") %>% 
  select(GEOID, NAME, year, ltnxE, ltnxM)


tract_race_16 <- map_df(region, function(x) {
  get_acs(geography = "tract", 
          table = "DP05", 
          state = "VA", county = x, survey = "acs5", year = 2016)
})

tract_white_16 <- tract_race_16 %>% 
  filter(variable == "DP05_0072P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  mutate(year = "2016") %>% 
  select(GEOID, NAME, year, whiteE, whiteM)

tract_black_16 <- tract_race_16 %>% 
  filter(variable == "DP05_0073P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  mutate(year = "2016") %>% 
  select(GEOID, NAME, year, blackE, blackM)

tract_indig_16 <- tract_race_16 %>% 
  filter(variable == "DP05_0074P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  mutate(year = "2016") %>% 
  select(GEOID, NAME, year, indigE, indigM)

tract_asian_16 <- tract_race_16 %>% 
  filter(variable == "DP05_0075P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  mutate(year = "2016") %>% 
  select(GEOID, NAME, year, asianE, asianM)

tract_othrace_16 <- tract_race_16 %>% 
  filter(variable %in% c("DP05_0076P", "DP05_0077P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate)) %>% 
  mutate(year = "2016") %>% 
  select(GEOID, NAME, year, othraceE, othraceM)

tract_multi_16 <- tract_race_16 %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  mutate(year = "2016") %>% 
  select(GEOID, NAME, year, multiE, multiM)

tract_ltnx_16 <- tract_race_16 %>% 
  filter(variable == "DP05_0066P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  mutate(year = "2016") %>% 
  select(GEOID, NAME, year, ltnxE, ltnxM)

# join race for each year
tract_race_10 <- tract_white_10 %>% 
  left_join(tract_black_10) %>% 
  left_join(tract_asian_10) %>% 
  left_join(tract_indig_10) %>% 
  left_join(tract_othrace_10) %>% 
  left_join(tract_multi_10) %>% 
  left_join(tract_ltnx_10)

tract_race_11 <- tract_white_11 %>% 
  left_join(tract_black_11) %>% 
  left_join(tract_asian_11) %>% 
  left_join(tract_indig_11) %>% 
  left_join(tract_othrace_11) %>% 
  left_join(tract_multi_11) %>% 
  left_join(tract_ltnx_11)

tract_race_12 <- tract_white_12 %>% 
  left_join(tract_black_12) %>% 
  left_join(tract_asian_12) %>% 
  left_join(tract_indig_12) %>% 
  left_join(tract_othrace_12) %>% 
  left_join(tract_multi_12) %>% 
  left_join(tract_ltnx_12)

tract_race_13 <- tract_white_13 %>% 
  left_join(tract_black_13) %>% 
  left_join(tract_asian_13) %>% 
  left_join(tract_indig_13) %>% 
  left_join(tract_othrace_13) %>% 
  left_join(tract_multi_13) %>% 
  left_join(tract_ltnx_13)

tract_race_14 <- tract_white_14 %>% 
  left_join(tract_black_14) %>% 
  left_join(tract_asian_14) %>% 
  left_join(tract_indig_14) %>% 
  left_join(tract_othrace_14) %>% 
  left_join(tract_multi_14) %>% 
  left_join(tract_ltnx_14)

tract_race_15 <- tract_white_15 %>% 
  left_join(tract_black_15) %>% 
  left_join(tract_asian_15) %>% 
  left_join(tract_indig_15) %>% 
  left_join(tract_othrace_15) %>% 
  left_join(tract_multi_15) %>% 
  left_join(tract_ltnx_15)

tract_race_16 <- tract_white_16 %>% 
  left_join(tract_black_16) %>% 
  left_join(tract_asian_16) %>% 
  left_join(tract_indig_16) %>% 
  left_join(tract_othrace_16) %>% 
  left_join(tract_multi_16) %>% 
  left_join(tract_ltnx_16)

# bind each year's file
tract_race_10_16 <- bind_rows(tract_race_10, tract_race_11, tract_race_12, tract_race_13, tract_race_14, tract_race_15, tract_race_16)


# ....................................................
# 4. Reduce and Combine data ----

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
  mutate(year = "2017") %>% 
  select(GEOID, NAME, year, totalpopE, totalpopM, whiteE, whiteM, blackE, blackM, asianE, asianM, indigE, indigM, othraceE, othraceM, multiE, multiM, ltnxE, ltnxM, everything())

# add prior years
tract_data_10_16 <- tract_pop_10_16 %>% 
  left_join(tract_race_10_16)

tract_data <- bind_rows(tract_data, tract_data_10_16)

tract_data <- tract_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality", "tract"), 
           sep = c(2,5)) 

# ....................................................
# 5. Summarize/Examine indicators ----
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
# 6. Add geography ----
# get tract polygons
geo <- tracts(state = 'VA', county = region)

# join coordinates to data
library(sp)
tract_data_geo <- merge(geo, tract_data, by = "GEOID", duplicateGeoms = TRUE)
tract_data_geo2 <- geo_join(geo, tract_data, by = "GEOID")

# add centroid coordinates for tract polygons
# as possible way of visualizing/layering a second attribute
library(geosphere)
tract_data_geo$ctr <- centroid(tract_data_geo)
tract_data_geo$lng <- tract_data_geo$ctr[,1]
tract_data_geo$lat <- tract_data_geo$ctr[,2]

tract_data_geo2$ctr <- centroid(tract_data_geo2)
tract_data_geo2$lng <- tract_data_geo2$ctr[,1]
tract_data_geo2$lat <- tract_data_geo2$ctr[,2]

# ....................................................
# 7. Save ----
# save rcaa_recap and rcaa_recap_geo
saveRDS(tract_data, file = "data/tract_data.RDS") 
saveRDS(tract_data_geo, file = "data/tract_data_geo.RDS")
saveRDS(tract_data_geo2, file = "data/tract_data_geo2.RDS")
# tract_data <- readRDS("data/tract_data.RDS")
# tract_data_geo <- readRDS("data/tract_data_geo.RDS")
