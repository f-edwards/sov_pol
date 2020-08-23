#### produce models and visuals for 
### sovere

library(tidyverse)

## read in fatal encounters as fe
## fe includes cases from 1/1/2011 - 8/20/2020
## 3,519 days / 365.25 = 9.6 years

source("read_fe.r")

## read in 14-18 ACS nhgis pop data as
## aianh_dat; county_dat; state_dat; nation_dat; aian_shp_file

source("read_pop.r")

### tables/figures

## age and sex specific risk
## adjusted to per year per 100,000 for 9.6 year period

age_sex<- fe %>% 
  group_by(age_cat, gender) %>% 
  summarise(deaths = n()) %>% 
  filter(!(is.na(age_cat))) %>% 
  right_join(nation_dat %>% 
              select(error, sex, age, pop) %>% 
              filter(age!="Total", sex!="Total") %>% 
              mutate(sex = ifelse(sex=="M", "Male", "Female")) %>% 
              rename(gender = sex, age_cat = age)) %>% 
  mutate(deaths = ifelse(is.na(deaths), 0, deaths)) %>% 
  mutate(mort_lwr = deaths / (pop+error) /(3519/365.25) * 1e5,
         mort_upr = deaths/(pop-error) /(3519/365.25) * 1e5,
         mort = deaths / pop /(3519/365.25) * 1e5) %>% 
  mutate(age_lwr = 
           case_when(
             age_cat=="85+" ~ "85",
             nchar(age_cat)==3 ~ substr(age_cat, 1, 1),
             nchar(age_cat)==5 ~ substr(age_cat, 1, 2)
           )) %>% 
  mutate(age_lwr = as.numeric(age_lwr))

ggplot(age_sex, 
       aes(y = mort, 
           x = age_lwr,
           ymin = mort_lwr,
           ymax = mort_upr)) + 
  geom_line() +
  geom_point(size = 0.7) +
  facet_wrap(~gender) + 
  labs(x = "Age", y = "People killed by police per year per 100,000")

## state-level risk

state_counts<-fe %>% 
  group_by(state, pl280) %>% 
  summarise(n = n()) %>% 
  left_join(state_dat %>% 
              filter(sex == "Total", age == "Total") %>% 
              select(state, pop, error)) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  filter(!(is.na(state))) %>% 
  mutate(pop_max = pop + error, ### apply ACS margins for 90% interval
         pop_min = pop - error) %>% 
  mutate(n_year = n / (3519 / 365.25)) %>% ### normalize to deaths / year
  mutate(n_rt_max = n_year / pop_min * 1e5,
         n_rt_min = n_year / pop_max* 1e5,
         n_rt = n_year / pop * 1e5) 

ggplot(state_counts,
       aes(xmin = n_rt_min,
           xmax = n_rt_max,
           x = n_rt,
           y = reorder(state, n_rt),
           color = pl280)) + 
  geom_pointrange() + 
  labs(x = "AIAN People killed by police per year per 100,000 population",
       y = "")

## by agency type

### make agency type deaths per 100,000 per year for national pop
### make pl280 population scaled for pl280 states

agency_type<-fe %>% 
  group_by(agency_type) %>% 
  summarise(n=n()) 

ggplot(agency_type,
       aes(x = n, y = reorder(agency_type, n))) +
  geom_col() + 
  labs(x = "AIAN people killed by police 2011 - 2020",
       y = "Law enforcement agency type")

agency_type<-fe %>% 
  group_by(agency_type, pl280) %>% 
  summarise(n=n()) 

ggplot(agency_type,
       aes(x = n, y = reorder(agency_type, n))) +
  geom_col() + 
  labs(x = "AIAN people killed by police 2011 - 2020",
       y = "Law enforcement agency type") + 
  facet_wrap(~pl280)

## on / off tribal lands
# need to merge onto shape file first

####### 

library(usmap)
us_map <- usmap::us_map() 

us_map<-us_map %>% 
  rename(state = abbr)

state_counts_map<-state_counts %>% 
  right_join(us_map) %>% 
  mutate(n_rt = ifelse(is.na(n_rt), 0, n_rt))

state_counts_map$pl280<-case_when(
  state_counts_map$state %in% c("CA", "MN", "NE", "OR", 
                  "WI", "AK") ~ "Mandatory",
  state_counts_map$state %in% c("NV", "FL", "ID", "IA", 
                  "WA", "SD", "MT", "ND", 
                  "AZ", "UT") ~ "Optional",
  T ~ "Non-PL280"
)

####################################
############ MAPS
####################################
########################

ggplot(state_counts_map,
       aes(x=x, y = y, fill = n_rt,
           group = group)) + 
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  ggsave("./vis/map_fe_rt.png")

ggplot(state_counts_map,
       aes(x=x, y = y, fill = pl280,
           group = group)) + 
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  ggsave("./vis/map_pl280.png")