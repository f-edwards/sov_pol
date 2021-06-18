library(tidyverse)


### the ACS age/sex/race files are not usable. the AIAN alone category misses ~ 50% of the population
### switching to 2010 Census for national and SEER for age-spec. SEER is undercount relative to census
### but can't get age-spec pop for AIAN in combination with others in Census

state_2010<-read_csv("./data/nhgis0052_csv/nhgis0052_ds173_2010_state.csv")
aianh_2010<-read_csv("./data/nhgis0052_csv/nhgis0052_ds173_2010_aianhh.csv")
county_2010<-read_csv("./data/nhgis0052_csv/nhgis0052_ds173_2010_county.csv")


state_dat<-state_2010 %>% 
  select(GISJOIN, YEAR, STATE, DIVISIONA, STATEA, ICV001) %>% 
  rename(pop = ICV001)

aianh_dat<-aianh_2010 %>% 
  select(GISJOIN, YEAR, AIANHH, AIANHHA,
         ICV001) %>% 
  rename(pop = ICV001)

county_dat<-county_2010 %>% 
  select(GISJOIN, YEAR, STATE, STATEA, COUNTY, COUNTYA,
         ICV001) %>% 
  rename(pop = ICV001)

names<-data.frame(STATE = state.name, state = state.abb)
state_dat<-state_dat %>% 
  left_join(names) %>% 
  mutate(state = ifelse(STATE == "District Of Columbia", "DC", state)) %>% 
  filter(STATE!="Puerto Rico")

### read in seer pop

age_sex_pop<-read_csv("./data/seer_2019_aian.csv")