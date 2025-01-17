#### read and format SEER population data
library(tidyverse)

pop<-read_fwf("./data/us.1990_2019.19ages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips", "cnty_fips", "reg", "race", 
                           "hisp", "sex", "age", "pop")))

pop<-pop%>%
  filter(year==2019) %>% 
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn = 
           case_when(
             race==1 & hisp ==0 ~ "white",
             race==2 ~ "black",
             race==3 ~ "aian",
             race==4 ~ "asian",
             hisp==1 ~ "latinx")) %>%
  mutate(sex = case_when(
    sex=="1" ~ "Male", 
    sex=="2" ~"Female"),
    age = case_when(
      age=="00" ~ "0", 
      age=="01" ~ "1-4",
      age=="02" ~ "5-9",
      age=="03" ~ "10-14",
      age=="04" ~ "15-19",
      age=="05" ~ "20-24",
      age=="06" ~ "25-29",
      age=="07" ~ "30-34",
      age=="08" ~ "35-39",
      age=="09" ~ "40-44",
      age=="10" ~ "45-49",
      age=="11" ~ "50-54",
      age=="12" ~ "55-59",
      age=="13" ~ "60-64",
      age=="14" ~ "65-69",
      age=="15" ~ "70-74",
      age=="16" ~ "75-79",
      age=="17" ~ "80-84",
      age=="18" ~ "85+"))

### use these for national age_spec tables

write_csv(pop, "./data/seer_2019_aian.csv")