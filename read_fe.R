#### Read and format fatal encounters for 
library(tidyverse)
library(lubridate)


# ... read fatal encounters
fe <- read_csv("./data/fe_8_4_20.csv", 
               guess_max = 1e5) %>% 
  filter(!(is.na(`Unique ID`))) 

### output for manual inclusion audit
# fe_aian<-fe %>% 
#   filter(`Subject's race`=="Native American/Alaskan") %>% 
#   filter(`Date (Year)`>2010) %>% 
#   write_csv("./data/aian_audit.csv")

### inclusion criteria: death resulted from officer action / inaction
### all causes except identified suicides included for AIAN 2011-2020
### officer action as a proximate cause of death

# .... make names more user-friendly
fe<-fe %>% 
  rename(id = `Unique ID`,
         name = "Subject's name",
         age = "Subject's age",
         gender = "Subject's gender",
         race = "Subject's race",
         state = `Location of death (state)`,
         county = `Location of death (county)`,
         agency = `Agency responsible for death`,
         cause_of_death = `Cause of death`,
         official_disposition = `Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS`,
         year = `Date (Year)`) %>% 
  select(id, name, age, gender, race, state,
         county, Latitude, Longitude, agency, cause_of_death, official_disposition,
         year) %>% 
  filter(year>2010) %>% 
  filter(race=="Native American/Alaskan") %>% 
  mutate(
    fe_cause_of_death = 
      case_when(grepl("suicide", tolower(official_disposition)) ~ "suicide",
                 T ~ cause_of_death))

### code for type of agency

fe<-fe %>% 
  mutate(agency = tolower(agency)) %>% 
  mutate(agency_type = 
           case_when(
             str_detect(agency, "u.s.") ~ "federal",
             str_detect(agency, "tribal") ~ "tribal",
             str_detect(agency, "nation") ~ "tribal",
             str_detect(agency, "tribe") ~ "tribal",
             str_detect(agency, "choctaw") ~ "tribal",
             str_detect(agency, "navajo") ~ "tribal",
             str_detect(agency, "apache") ~ "tribal",
             str_detect(agency, "highway") ~ "state",
             str_detect(agency, "state") ~ "state",
             str_detect(agency, "county") ~ "county",
             str_detect(agency, "sheriff") ~ "county",
             str_detect(agency, "university") ~ "university",
             T ~ "municipal"))

### check unmatched, looks good
# View(fe %>% filter(is.na(agency_type)) %>% select(agency, agency_type, state))


### use FCC API to map lat/long to FIPS Block for census join
#source(map_fe_census_block.R)
### creates block_map.csv

blocks<-read_csv("./data/block_map.csv")

fe<-fe %>% 
  left_join(blocks %>% 
              rename(Latitude = lat,
                     Longitude = long))

### make pl280 variable

fe$pl280<-case_when(
  fe$state %in% c("CA", "MN", "NE", "OR", 
                          "WI", "AK") ~ "Mandatory",
  fe$state %in% c("NV", "FL", "ID", "IA", 
                          "WA", "SD", "MT", "ND", 
                          "AZ", "UT") ~ "Optional",
  T ~ "Non-PL280"
)

### recode age cats

fe<-fe %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age_cat = case_when(
    age<5 ~ "0-5",
    age<10 ~ "5-9",
    age<15 ~ "10-14",
    age<18 ~ "15-17",
    age<20 ~ "18-19",
    age<25 ~ "20-24",
    age<30 ~ "25-29",
    age<35 ~ "30-34",
    age<45 ~ "35-44",
    age<55 ~ "45-54",
    age<65 ~ "55-64",
    age<75 ~ "65-74",
    age<85 ~ "75-84",
    age>85 ~ "85+"
  ))

rm(blocks)