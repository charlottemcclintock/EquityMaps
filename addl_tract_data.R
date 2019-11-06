####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire Additiona Tract-Level data
# Last updated: 10/31/2019
# Metrics from various sources: 
# * Small Area Life Expectancy Estimates: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html 
# * Segregation measures (from ACS data, but with more derivation)
#
# TO ADD
# * HMDA relevant metrics
# * Placeholder for others
#
# Geography: Tracts in Localities in Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson, Buckingham, Madison, Orange
#     (include Augusta, Waynesboro, Staunton?)
####################################################
# 1. Load libraries
# 2. Small-area life expectancy estimates
# 3. Segregation measures
# 4. HMDA measures
####################################################

# ....................................................
# 1. Load libraries, provide api key (if needed), identify variables ----

# Load libraries
library(tidyverse)
library(tidycensus)

ccode <- read_csv("county_codes.csv")
region <- ccode$code # list of desired counties


# ....................................................
# 2. Small-area life expectancy estimates ----
# a. acquire ----
# url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/VA_A.CSV"
# download.file(url, destfile="tempdata/va_usasleep.csv", method="libcurl")

# read data and rename
life_exp <- read_csv("tempdata/va_usasleep.csv")
names(life_exp) <- c("geoid", "state", "county", "tract", "life_exp", "se", "flag")

# b. Limit to region and derive metrics ----
life_exp <- life_exp %>% 
  filter(county %in% region) %>% # 5 missing tracts (80 of 85)
  rename(life_expE = life_exp,
         locality = county) %>% 
  mutate(life_expM = 1.64*se,
         year = "2017") %>% 
  select(-se, -flag)

# check
summary(life_exp)

# c. save ----
saveRDS(life_exp, file = "data/tract_life_exp.RDS") 
# life_exp <- readRDS("data/tract_life_exp.RDS")


# ....................................................
# 3. Segregation measures ----
# a. acquire tract data ----
race_table17 <-  get_acs(geography = "tract", 
                         year=2017, state = "VA",
                         table = "B03002", survey = "acs5",
                         geometry = F, output="wide", 
                         cache_table = T)

# rename
seg_tract <-race_table17 %>%
  mutate(white = B03002_003E,
         black = B03002_004E,
         asian = B03002_006E,
         indig = B03002_005E,
         other = B03002_007E + B03002_008E,
         multi = B03002_009E,
         hisp = B03002_012E, 
         total = B03002_001E,
         year = 2017,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,9)) %>% 
  select(GEOID, white, black, indig, asian, other, multi, hisp, total, year, state, county, tract) 

# b. acquire county data ----
race_table17 <-  get_acs(geography = "county", 
                         year=2017, state = "VA",
                         table = "B03002", survey = "acs5",
                         geometry = F, output="wide", 
                         cache_table = T)

# rename
seg_county <-race_table17 %>%
  mutate(cowhite = B03002_003E,
         coblack = B03002_004E,
         coasian = B03002_006E,
         coindig = B03002_005E,
         coother = B03002_007E + B03002_008E,
         comulti = B03002_009E,
         cohisp = B03002_012E, 
         cototal = B03002_001E,
         year = 2017,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5)) %>% 
  select(GEOID, cowhite, coblack, coindig, coasian, coother, comulti, cohisp, cototal, year, state, county) 


# c. Limit to region and derive metrics ----
# nice explanations for segregation measures: 
# https://sejdemyr.github.io/r-tutorials/statistics/measuring-segregation.html
# https://rstudio-pubs-static.s3.amazonaws.com/473785_e782a2a8458d4263ba574c7073ca5057.html

# limit to region
seg_tract <- seg_tract %>% 
  filter(county %in% region) %>% 
  arrange(GEOID)

# add county totals
seg_tract <- left_join(seg_tract, seg_county, by=c("county"))

# generate seg measures
dissim_wb <- seg_tract %>%
  mutate(d.wb = abs(white/cowhite - black/coblack)) %>%
  group_by(county) %>%
  summarise(dissim_wb = .5*sum(d.wb, na.rm=T))

dissim_wh <- seg_tract %>%
  mutate(d.wh = abs(white/cowhite - hisp/cohisp)) %>%
  group_by(county) %>%
  summarise(dissim_wh = .5*sum(d.wh, na.rm=T))

inter_bw <- seg_tract %>%
  mutate(int.bw=(black/coblack * white/total))%>%
  group_by(county)%>%
  summarise(inter_bw= sum(int.bw, na.rm=T))

inter_hw <- seg_tract %>%
  mutate(int.hw=(hisp/cohisp * white/total))%>%
  group_by(county)%>%
  summarise(inter_hw= sum(int.hw, na.rm=T))

isol_b <- seg_tract %>%
  mutate(isob=(black/coblack * black/total) )%>%
  group_by(county) %>%
  summarise(iso_b = sum(isob, na.rm=T))

isol_h <- seg_tract %>%
  mutate(isoh=(hisp/cohisp * hisp/total)) %>%
  group_by(county) %>%
  summarise(iso_h = sum(isoh, na.rm=T))

seg_county <- dissim_wb %>% 
  left_join(dissim_wh) %>% 
  left_join(inter_bw) %>% 
  left_join(inter_hw) %>% 
  left_join(isol_b) %>% 
  left_join(isol_h)

# could estimate spatial segregation with seg

# check
summary(seg_county)
pairs(seg_county[2:7])

# d. save ----
saveRDS(seg_county, file = "data/seg_county.RDS")
# seg_county <- readRDS("data/seg_county.RDS")


# ....................................................
# 4. HMDA measures ----
# url <- "https://s3.amazonaws.com/cfpb-hmda-public/prod/snapshot-data/2018/2018_public_lar_csv.zip"
# download.file(url, destfile="tempdata/hmda2018.zip", method="libcurl")
# unzip("tempdata/hmda2018.zip", exdir = "tempdata/hmda2018full")

# data dictionary: https://ffiec.cfpb.gov/documentation/2018/lar-data-fields/

hmda2018 <- read_csv("tempdata/hmda2018full/2018_public_lar_csv.csv")
hmda2018 <- hmda2018 %>% filter(state_code == "VA") # limit to VA

hmda2018 <- hmda2018 %>% 
  mutate(locality = substr(county_code, 3,5), 
         state_fip = substr(county_code, 1,2)) # create locality

hmda2018 <- hmda2018 %>% 
  filter(state_fip == "51") %>% 
  filter(locality %in% region) # limit to region

# hmda2018 <- hmda2018 %>% 
#   filter(action_taken == 1, loan_purpose == 1) 
# originated loans/completed purchases; for home purchase (not improvement, refinancing, etc.)

# number of purchases by white, black, asian, indig, hispanic

# maybe get past data
# https://www.consumerfinance.gov/data-research/hmda/historic-data/?geo=va&records=all-records&field_descriptions=codes
# http://cfpb.github.io/api/hmda/index.html




