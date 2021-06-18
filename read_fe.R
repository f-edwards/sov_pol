#### Read and format fatal encounters for 
library(tidyverse)
library(lubridate)


# ... read fatal encounters
fe <- read_csv("./data/fe_6_18_21.csv", 
               guess_max = 1e5) %>% 
  filter(!(is.na(`Unique ID`))) %>% 
  filter(!(is.na(Age)))

#### IMPUTE LATER!!!!!!!!!!
#### 2 CASES MISSING AGE
#### NO AIAN IMPUTED CASES IN DATA, I SHOULD DO IMPS

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
         name = Name,
         age = Age,
         gender = Gender,
         race = Race,
         race_imp = `Race with imputations`,
         state = State,
         county = `Location of death (county)`,
         agency = `Agency or agencies involved`,
         force = `Intended use of force (Developing)`) %>% 
  mutate(date = mdy(`Date of injury resulting in death (month/day/year)`),
         year = year(date)) %>% 
  select(id, name, age, gender, race, race_imp,
         state, county, Latitude, Longitude, agency, force,
         year, date) %>% 
  filter(year>=2010) %>% 
  filter(race == "Native American/Alaskan")


### manually correct age for Hubert Thomas Burns Jr.
### age 34 according to obit at https://www.legacy.com/obituaries/name/hubert-burns-obituary?pid=180276209


fe<-fe %>% 
  mutate(age = ifelse(
    id==17739, "34", age))

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
# source(map_fe_census_block.R)
### creates block_map.csv

blocks<-read_csv("./data/block_map.csv")

fe<-fe %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  left_join(blocks %>% 
              rename(Latitude = lat,
                     Longtitude = long))


### recode age cats

fe<-fe %>% 
  mutate(age = as.numeric(age)) %>%
  mutate(age_cat = case_when(
    age<1 ~ "0",
    age>=1 & age<=4 ~ "1-4",
    age>=5 & age<=9 ~ "5-9",
    age>=10 & age<=14 ~ "10-14",
    age>=15 & age<=19 ~ "15-19",
    age>=20 & age<=24 ~ "20-24",
    age>=25 & age<=29 ~ "25-29",
    age>=30 & age<=34 ~ "30-34",
    age>=35 & age<=39 ~ "35-39",
    age>=40 & age<=44 ~ "40-44",
    age>=45 & age<=49 ~ "45-49",
    age>=50 & age<=54 ~ "50-54",
    age>=55 & age<=59 ~ "55-59",
    age>=60 & age<=64 ~ "60-64",
    age>=65 & age<=69 ~ "65-69",
    age>=70 & age<=74 ~ "70-74",
    age>=75 & age<=79 ~ "75-79",
    age>=80 & age<=84 ~ "80-84",
    age>=85  ~ "85+"))

rm(blocks)