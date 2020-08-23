library(tidyverse)

### add state names, join to whatever data you are using

aianh_2018 <- read_csv("./data/nhgis0049_ds240_20185_2018_aianhh.csv")

state_2018 <- read_csv("./data/nhgis0049_ds240_20185_2018_state.csv")

county_2018 <- read_csv("./data/nhgis0049_ds240_20185_2018_county.csv")

nation_2018<- read_csv("./data/nhgis0051_ds240_20185_2018_nation.csv")


### reshape and recode pop data to age/sex by geography


reshape_pop<-function(dat, geography){
  ### reshape long with selected geography
  
  dat<-dat %>% 
    mutate(temp = T) %>% 
    select(GISJOIN, YEAR, all_of(geography), NAME_E:temp) %>% 
    select(-NAME_M, -NAME_E, -temp) %>% 
    pivot_longer(cols = 4:65,
                 names_to = "var",
                 values_to = "pop")
  
  ### recode sex
  
  dat<-dat %>% 
    mutate(sex = case_when(
    (as.numeric(str_sub(var, start = -3L, end = -1L)) >=2 &
       as.numeric(str_sub(var, start = -3L, end = -1L)) <=16) ~
      "M",
    (as.numeric(str_sub(var, start = -3L, end = -1L)) >=17 &
       as.numeric(str_sub(var, start = -3L, end = -1L)) <=31) ~
      "F")) 
  
  ### recode age
  dat <- dat %>% 
    mutate(age = case_when(
      str_sub(var, start = -3L, end = -1L) == "002" |
        str_sub(var, start = -3L, end = -1L) == "017" 
      ~ "Total",
      str_sub(var, start = -3L, end = -1L) == "003" |
        str_sub(var, start = -3L, end = -1L) == "018" 
      ~ "0-4",
      str_sub(var, start = -3L, end = -1L) == "004" |
        str_sub(var, start = -3L, end = -1L) == "019" 
      ~ "5-9",
      str_sub(var, start = -3L, end = -1L) == "005" |
        str_sub(var, start = -3L, end = -1L) == "020" 
      ~ "10-14",
      str_sub(var, start = -3L, end = -1L) == "006" |
        str_sub(var, start = -3L, end = -1L) == "021" 
      ~ "15-17",
      str_sub(var, start = -3L, end = -1L) == "007" |
        str_sub(var, start = -3L, end = -1L) == "022" 
      ~ "18-19",
      str_sub(var, start = -3L, end = -1L) == "008" |
        str_sub(var, start = -3L, end = -1L) == "023" 
      ~ "20-24",
      str_sub(var, start = -3L, end = -1L) == "009" |
        str_sub(var, start = -3L, end = -1L) == "024" 
      ~ "25-29",
      str_sub(var, start = -3L, end = -1L) == "010" |
        str_sub(var, start = -3L, end = -1L) == "025" 
      ~ "30-34",
      str_sub(var, start = -3L, end = -1L) == "011" |
        str_sub(var, start = -3L, end = -1L) == "026" 
      ~ "35-44",
      str_sub(var, start = -3L, end = -1L) == "012" |
        str_sub(var, start = -3L, end = -1L) == "027" 
      ~ "45-54",
      str_sub(var, start = -3L, end = -1L) == "013" |
        str_sub(var, start = -3L, end = -1L) == "028" 
      ~ "55-64",
      str_sub(var, start = -3L, end = -1L) == "014" |
        str_sub(var, start = -3L, end = -1L) == "029" 
      ~ "65-74",
      str_sub(var, start = -3L, end = -1L) == "015" |
        str_sub(var, start = -3L, end = -1L) == "030" 
      ~ "75-84",
      str_sub(var, start = -3L, end = -1L) == "016" |
        str_sub(var, start = -3L, end = -1L) == "031" 
      ~ "85+",
    ))
  
  ### pull out margins of error
  
  dat<-dat %>% 
    mutate(margin=
             str_sub(dat$var, start = 5, end = 5) == "M")
  
  ### make separate var for MoE/pop
  
  dat<-dat %>% 
    filter(margin==TRUE) %>% 
    rename(error = pop) %>% 
    select(-margin, -var) %>% 
    right_join(dat %>% 
                filter(margin==FALSE) %>% 
                select(-margin, -var))
  
  ### recode NA sex / age as total
  
  dat<-dat %>% 
    mutate(sex = ifelse(is.na(sex), "Total", sex),
           age = ifelse(is.na(age), "Total", age))
  
  return(dat)
}


aianh_dat<-
  reshape_pop(aianh_2018, geography = "AIANHH")

county_2018<-county_2018 %>% 
  mutate(FIPS = paste(STATEA,
                      COUNTYA,
                      sep = ""))

county_dat<-
  reshape_pop(county_2018, "FIPS")

state_dat<-
  reshape_pop(state_2018, "STATE")

nation_dat<-reshape_pop(nation_2018, "NATION")

### attach abbreviations
names<-data.frame(STATE = state.name, state = state.abb)

rm(county_2018,aianh_2018, state_2018)

state_dat<-state_dat %>% 
  left_join(names)