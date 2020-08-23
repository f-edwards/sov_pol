rm(list=ls()); gc()

source("read_with_shapes.R")

library(tidyverse)
library(ipumsr)
library(sf)
library(rgdal)

###### PLOT
### lower 48 cutpoints
# http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
# top = 49.3457868 # north lat
# left = -124.7844079 # west long
# right = -66.9513812 # east long
# bottom =  24.7433195 # south lat
### BOUNDING BOX
bb <- st_sfc(
  st_polygon(list(cbind(
    c(-72,-127, -127, -72, -72), # x-coordinates (longitudes) of points A,B,C,D
    c(21, 49, 49,  21,  21 )     # y-coordi, nates (latitudes) of points A,B,C,D
  ))),
  crs = as.character(CRS("+init=epsg:4326")@projargs))

# now in in LAEA projection
laeabb <- st_transform(bb, 
                       crs =  as.character(CRS("+init=esri:102003")@projargs))
b <- st_bbox(laeabb)

ca_aianh<-aianh_dat %>% filter(state=="CA")
ca_aianh<-as.character(ca_aianh$name)

p1<-ggplot(data = nhgis) +
  geom_sf(size = 0.1, color = "gray60", fill = "white") +
  geom_sf(data = nhgis_aian,
          color = "dodgerblue", fill = "dodgerblue",
          alpha = 0.5, size = 0.01) +
  coord_sf(crs = st_crs(nhgis), datum = NA,
           xlim = c(b["xmin"], b["xmax"]),
           ylim = c(b["ymin"], b["ymax"])) +
  geom_point(data = fe_amind_conv%>%
               filter(!loc_state%in%(c("AK", "HI", "PR"))),
             aes(x = lon, y = lat),
             size = 0.3) +
  theme_void()+
  labs(caption = "Dots indicate police-involved killings. Data from Fatal Encounters\nBlue areas indicate American Indian reservations, tribal subdivisions, and trust lands.")
theme(plot.subtitle= element_text(hjust = 0.5)) +
  ggsave("./vis/map_aian_fe.png", width = 8, height = 6)

# p1<-ggplot(data = nhgis) +
#   geom_sf(size = 0.1, color = "gray60", fill = "white") +
#   geom_sf(data = nhgis_aian,
#           color = "dodgerblue", fill = "dodgerblue",
#           alpha = 0.5, size = 0.01) +
#   coord_sf(crs = st_crs(nhgis), datum = NA,
#            xlim = c(b["xmin"], b["xmax"]),
#            ylim = c(b["ymin"], b["ymax"])) +
#   geom_point(data = fe_amind_conv%>%
#                filter(!loc_state%in%(c("AK", "HI", "PR"))),
#              aes(x = lon, y = lat),
#              size = 0.3) +
#   theme_void()+
#   ggsave("./vis/map_aian_fe.pdf")

###### MORE MAPS!

## USE NHGIS AS SOURCE, STATE DATA
### make categorical var of each outcome to map, then facet

# plot_dat<-readRDS("./data/state_dat.rds")%>%
#   rename_all(toupper)%>%
#   select(GISJOIN, 
#          PC_INCOME_AMIND,
#          ST_INCOME_INEQ,
#          AIANH_POV_RT,
#          POV_RATIO,
#          UNEMP_RATIO,
#          AIANH_PCT_TRIBAL_POLICE,
#          AIANH_PCT_TRIBAL_COURTS,
#          AIANH_PCT_TRIBAL_JAIL)%>%
#   unique()%>%
#   gather("Variable",
#          "Value",
#          -GISJOIN)
# 
# fe_dat<-readRDS("./data/state_dat.rds")%>%
#   rename_all(toupper)%>%
#   group_by(GISJOIN)%>%
#   summarise(FE_AIAN = mean(FE_AIAN))%>%
#   left_join(readRDS("./data/state_dat.rds")%>%
#               rename_all(toupper)%>%     
#               select(GISJOIN, TOTAL_POPULATION_AMIND)%>%
#               unique())%>%
#   mutate(FE_RT_AIAN = FE_AIAN / TOTAL_POPULATION_AMIND * 1e5)%>%
#   select(-FE_AIAN, -TOTAL_POPULATION_AMIND)%>%
#   gather("Variable",
#          "Value",
#          -GISJOIN)
# 
# plot_dat<-plot_dat%>%
#   bind_rows(fe_dat)
# 
# 
# plot_dat<-left_join(nhgis,
#                  plot_dat)
# 
# p2<-ggplot(data = plot_dat %>%
#              filter(Variable%in%c("AIANH_PCT_TRIBAL_COURTS",
#                                   "AIANH_PCT_TRIBAL_JAIL",
#                                   "AIANH_PCT_TRIBAL_POLICE"))%>%
#              mutate(Variable = case_when(
#                Variable == "AIANH_PCT_TRIBAL_COURTS" ~ "Courts",
#                Variable == "AIANH_PCT_TRIBAL_JAIL" ~ "Jails",
#                Variable == "AIANH_PCT_TRIBAL_POLICE" ~ "Police"
#              ))%>%
#              rename(Proportion = Value)%>%
#              filter(!STUSPS %in%c("AK", "HI", "PR")),
#            aes(fill = Proportion)) +
#   geom_sf() +
#   coord_sf(datum=NA) +
#   facet_wrap(~Variable, ncol = 1) + 
#   scale_fill_distiller(palette = "Spectral")+
#   theme_void() + 
#   #labs(title = "Percent of AIAN governments with each legal institution, 2002") +
#   ggsave("./vis/map_legal.png")
# 
# p2<-ggplot(data = plot_dat %>%
#              filter(Variable%in%c("AIANH_PCT_TRIBAL_COURTS",
#                                   "AIANH_PCT_TRIBAL_JAIL",
#                                   "AIANH_PCT_TRIBAL_POLICE"))%>%
#              mutate(Variable = case_when(
#                Variable == "AIANH_PCT_TRIBAL_COURTS" ~ "Courts",
#                Variable == "AIANH_PCT_TRIBAL_JAIL" ~ "Jails",
#                Variable == "AIANH_PCT_TRIBAL_POLICE" ~ "Police"
#              ))%>%
#              rename(Proportion = Value)%>%
#              filter(!STUSPS %in%c("AK", "HI", "PR")),
#            aes(fill = Proportion)) +
#   geom_sf() +
#   coord_sf(datum=NA) +
#   facet_wrap(~Variable) + 
#   scale_fill_distiller(palette = "Spectral")+
#   theme_void() + 
#   #labs(title = "Percent of AIAN governments with each legal institution, 2002") +
#   ggsave("./vis/map_legal.pdf")
# 
# 
# p3<-ggplot(data = plot_dat %>%
#              filter(Variable%in%c("POV_RATIO",
#                                   "UNEMP_RATIO"))%>%
#              mutate(Variable = case_when(
#                Variable == "POV_RATIO" ~ "Poverty",
#                Variable == "UNEMP_RATIO" ~ "Unemployment"
#              ))%>%
#              rename(Ratio = Value)%>%
#              filter(!STUSPS %in%c("AK", "HI", "PR")),
#            aes(fill = Ratio)) +
#   geom_sf() +
#   coord_sf(datum=NA) +
#   facet_wrap(~Variable, ncol = 1) + 
#   scale_fill_distiller(palette = "Spectral")+
#   theme_void() + 
#   #labs(title = "Ratio of AIAN homeland to state values, 2013-2017") + 
#   ggsave("./vis/map_ineq.png")
# 
# p3<-ggplot(data = plot_dat %>%
#              filter(Variable%in%c("POV_RATIO",
#                                   "UNEMP_RATIO"))%>%
#              mutate(Variable = case_when(
#                Variable == "POV_RATIO" ~ "Poverty",
#                Variable == "UNEMP_RATIO" ~ "Unemployment"
#              ))%>%
#              rename(Ratio = Value)%>%
#              filter(!STUSPS %in%c("AK", "HI", "PR")),
#            aes(fill = Ratio)) +
#   geom_sf() +
#   coord_sf(datum=NA) +
#   facet_wrap(~Variable) + 
#   scale_fill_distiller(palette = "Spectral")+
#   theme_void() + 
#   #labs(title = "Ratio of AIAN homeland to state values, 2013-2017") + 
#   ggsave("./vis/map_ineq.pdf")
# 
# p4<-ggplot(data = plot_dat %>%
#              filter(Variable%in%c("FE_RT_AIAN"))%>%
#              mutate(Variable = case_when(
#                Variable == "FE_RT_AIAN" ~ "AIAN deaths per 100,000 per year"
#              ))%>%
#              rename(Rate = Value)%>%
#              filter(!STUSPS %in%c("AK", "HI", "PR")),
#            aes(fill = Rate)) +
#   geom_sf() +
#   coord_sf(datum=NA) +
#   facet_wrap(~Variable, ncol = 1) + 
#   scale_fill_distiller(palette = "Spectral")+
#   theme_void() + 
#   #labs(title = "AIAN Police killings per 100,000 persons") + 
#   ggsave("./vis/map_fe_aian_st.png")
# 
# p4<-ggplot(data = plot_dat %>%
#              filter(Variable%in%c("FE_RT_AIAN"))%>%
#              mutate(Variable = case_when(
#                Variable == "FE_RT_AIAN" ~ "AIAN deaths per 100,000 per year"
#              ))%>%
#              rename(Rate = Value)%>%
#              filter(!STUSPS %in%c("AK", "HI", "PR")),
#            aes(fill = Rate)) +
#   geom_sf() +
#   coord_sf(datum=NA) +
#   facet_wrap(~Variable) + 
#   scale_fill_distiller(palette = "Spectral")+
#   theme_void() + 
#   #labs(title = "AIAN Police killings per 100,000 persons") + 
#   ggsave("./vis/map_fe_aian_st.pdf")