##############################

library(tidyverse)
library(haven)
library(lubridate)
library(ipumsr)
library(sf)
library(rgdal)
select<-dplyr::select


aian_shp<-readOGR("./data/nhgis0049_shapefile_tl2018_us_aianhh_2018/")


  
#### clean and transform ann_surv

#### re-map lat/lon to ESRI
d <- data.frame(lon=fe_amind$Longitude, lat=fe_amind$Latitude)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+init=esri:102003")
d.convert <- data.frame(spTransform(d, CRS.new))
fe_amind<- bind_cols(fe_amind, d.convert)
fe_amind_conv<-fe_amind

### join onto AIANH shapes
### joining full data to get all police deaths
### in indian country
d2<- data.frame(lon=fe$Longitude, lat=fe$Latitude)%>%
  filter(!(is.na(lon)))
coordinates(d2) <- c("lon", "lat")
proj4string(d2) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+init=esri:102003")
d2.convert <- data.frame(spTransform(d2, CRS.new))
fe_coord<- bind_cols(fe%>%filter(!(is.na(Longitude))), 
                     d2.convert)


coordinates(fe_amind) = ~lon+lat
proj4string(fe_amind) = proj4string(aian_shp)
join_aian<-over(fe_amind, 
                aian_shp[,c("GISJOIN", "NAME")])
join_aian<-join_aian%>%
  mutate(id = fe_amind$id)%>%
  rename(aianh_name = NAME)
dat_fe_amind<-left_join(fe_amind@data, join_aian)
  

coordinates(fe_coord) = ~lon+lat
proj4string(fe_coord) = proj4string(aian_shp)
join_aian<-over(fe_coord, 
                aian_shp[,c("GISJOIN", "NAME")])
join_aian<-join_aian%>%
  mutate(id = fe_coord$id)%>%
  rename(aianh_name = NAME)
dat_fe_all<-left_join(fe_coord@data, join_aian)

#### this gives us two tables - one with aian deaths
### on aian land, one with all deaths

### for detention facilities
### need to link to tribal area GISJOIN

### USE cens_trib_just$NAME as master, nhgis_aian$NAME as match for 
### GISJOIN
### THIS HAS HIGH RESPONSE RATE AND AWESOME VARIABLES!!

### going to do this manually because of names
# write_csv(cens_trib_just%>%
#   arrange((NAME))%>%
#   select(REC_ID, NAME, STATE),
#   "./data/master_agency_match.csv")
# 
# write_csv(nhgis_aian%>%
#             st_set_geometry(NULL)%>%
#             arrange(NAME_E)%>%
#             filter(str_sub(NAME_E, -2, -1)!="AK")%>%
#             select(NAME_E, GISJOIN),
#           "./data/join_agency_match.csv")
### manually matched file
### in OK, assigning each to OTSA, not joint use areas for now
matched<-read_csv("./data/master_agency_match_mod.csv")
matched<-matched%>%
  select(-NAME, -STATE)

cens_trib_just<-cens_trib_just%>%
  left_join(matched)

### left join on cens_trib_just for analytic sample
aianh_dat<-cens_trib_just%>%
  left_join(aianh_dat)
              
### collapse fe to aianh level
### ~ 600 cases for FE, check and confirm them all
aianh_dat<-aianh_dat%>%
  left_join(dat_fe_all%>%
              filter(!(is.na(GISJOIN)))%>%
              filter(!(grepl("suicide", tolower(official_disposition))))%>%
              group_by(GISJOIN)%>%
              summarise(fe_total = n(),
                        fe_aian = sum(race=="amind", na.rm=TRUE),
                        fe_not_missing = sum(!(is.na(race)))))

### code law enf agency type for on-homeland aian deaths
### eventually do this for all
temp<-dat_fe_amind%>%
  filter(!(is.na(GISJOIN)))

temp<-temp%>%
  mutate(agency_type = case_when(
    grepl("sheriff", tolower(agency)) ~ "county",
    grepl("state", tolower(agency)) ~ "state",
    grepl("bureau", tolower(agency)) | grepl("U.S.", agency)  ~ "federal",
    grepl("tribal", tolower(agency)) | grepl("nation", tolower(agency)) | 
      grepl("tribe", tolower(agency))~ "tribal",
    grepl("wind river", tolower(agency)) ~ "tribal",
    grepl("red lake", tolower(agency)) ~ "tribal",
    grepl("highway", tolower(agency)) ~ "state"))%>%
  mutate(agency_type = ifelse(is.na(agency_type), "local", agency_type))%>%
  group_by(GISJOIN)%>%
  summarise(fe_aian_tribal = sum(agency_type == "tribal"),
            fe_aian_statelocal = sum(agency_type %in% c("local", "county", "state")),
            fe_aian_fed = sum(agency_type == "federal"))

aianh_dat<-aianh_dat%>%
  left_join(temp)%>%
  mutate(fe_aian_tribal = ifelse(is.na(fe_aian_tribal), 0 , fe_aian_tribal),
         fe_aian_statelocal = ifelse(is.na(fe_aian_statelocal), 0, fe_aian_statelocal),
         fe_aian_fed = ifelse(is.na(fe_aian_fed), 0, fe_aian_fed),
         fe_total = ifelse(is.na(fe_total), 0, fe_total),
         fe_aian = ifelse(is.na(fe_aian), 0 , fe_aian),
         fe_not_missing = ifelse(is.na(fe_not_missing), 0 , fe_not_missing))%>%
  rename_all(tolower)%>%
  mutate(state=str_sub(aianhh, -2, -1)) # using last state listed on name for now
### later will distribute either with pop or land area

state_dat<-state_dat%>%
  st_set_geometry(NULL)%>%left_join(
    dat_fe_all%>%
      filter(!(grepl("suicide", tolower(official_disposition))))%>%
      filter(year>2012)%>%
      group_by(loc_state, year)%>%
      summarise(fe_total = n(),
                fe_aian = sum(race=="amind", na.rm=TRUE),
                fe_not_missing = sum(!(is.na(race))))%>%
      rename(stusps = loc_state))%>%
  left_join(aianh_dat%>%
              group_by(state)%>%
              summarise(aianh_pov_rt = sum(pov,na.rm=TRUE)/sum(pov_pop,na.rm=TRUE),
                        aianh_less_hs_rt = sum(less_hs_25,na.rm=TRUE)/sum(pop_hs_25,na.rm=TRUE),
                        aianh_unemp_rt = sum(unemp,na.rm=TRUE)/sum(labforce,na.rm=TRUE),
                        aianh_pct_tribal_police = sum(police_tribal,na.rm=TRUE)/n(),
                        aianh_pct_tribal_jail = sum(jail_tribal,na.rm=TRUE)/n(),
                        aianh_pct_tribal_courts = sum(courts,na.rm=TRUE)/n()
                        )%>%
              rename(stusps = state)) 

### join state child population onto state table




### create state level aian homeland measures for comparisons

saveRDS(aianh_dat%>%
          select(-geometry)%>%
          mutate(pov_rt = pov / pov_pop,
                 unemp_rt = unemp / labforce,
                 hs_rt = less_hs_25 / pop_hs_25), 
        "./data/aianh_dat.rds")

saveRDS(state_dat%>%
          filter(!(stusps%in%c("AK", "HI", "PR")))%>%
          filter(year>2012)%>% 
          mutate(st_income_ineq = pc_income_amind / pc_income_white,
                 pov_ratio = aianh_pov_rt/(pov/pov_pop),
                 unemp_ratio = aianh_unemp_rt / (unemp / labforce),
                 hs_ratio = aianh_less_hs_rt / (less_hs_25 / pop_hs_25)),
        "./data/state_dat.rds")

# ### read in afcars
# afcars<-read_csv("./data/afcars_imputed_all_cases.csv")
# 
# #### join to pop data
# # pop_st<-pop_st %>% 
# #   filter(race_ethn%in%c("White", "AI/AN")) %>% 
# #   pivot_wider(id_cols = c("state", "year"), 
# #               names_from = race_ethn, 
# #               values_from = c("pop_child")) %>% 
# #   rename(pop_child_aian=`AI/AN`,
# #          pop_child_white = White)
# 
# afcars_aian<-afcars %>% 
#   filter(year>2003) %>% 
#   filter(race_ethn=="AI/AN") %>% 
#   filter(.imp!=0) %>% 
#   filter(state!=72)
# 
# na_to_zero<-function(x){ifelse(is.na(x),0,x)}
# 
# afcars_aian<-afcars_aian %>% 
#   mutate_at(vars(rf1amakn, rf2amakn, rf1nhopi, rf2nhopi),
#             na_to_zero) 
# 
# afcars_aian_st<-afcars_aian %>% 
#   group_by(.imp, state, year) %>% 
#   summarise(fc_entered = sum(entered),
#          fc_istpr = sum(istpr),
#          fc_caseload = n(),
#          fc_totalrem_mn = mean(totalrem),
#          fc_inst = sum(curplset%in%c(4,5)),
#          fc_aian_home = sum(curplset%in%c(1,2,3) * (rf1amakn==1|rf2amakn==1))/
#            sum(curplset%in%c(1,2,3,4,5)))
# 
# expands<-expand_grid(.imp=unique(afcars$.imp), 
#                      state=unique(afcars$state),
#                      year= unique(afcars$year)) %>% 
#   filter(.imp>0, year>2003, state!=72)
# 
# afcars_aian_st<-expands %>% 
#   left_join(afcars_aian_st)%>% 
#   left_join(pop_st %>% 
#               filter(race_ethn == "AIAN") %>% 
#               select(st_fips, year, pop_child) %>% 
#               mutate(state = as.numeric(st_fips))) %>% 
#   mutate_at(c("fc_istpr", "fc_caseload", "fc_inst", "fc_entered"), 
#             function(x){ifelse(is.na(x), 0, x)})
# 
# write_csv(afcars_aian_st, "./data/afcars_aian_st.csv")
# 
# quit(save="no")