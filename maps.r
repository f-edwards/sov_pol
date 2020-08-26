rm(list=ls()); gc()

source("read_fe.R")
source("join_shapes.r")

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

#### re-map lat/lon to ESRI


ggplot(data = nhgis) +
  geom_sf(size = 0.1, color = "gray60", fill = "white") +
  geom_sf(data = nhgis_aian,
          color = "dodgerblue", fill = "dodgerblue",
          alpha = 0.5, size = 0.01) +
  coord_sf(crs = st_crs(nhgis), datum = NA) +
  geom_point(data = fe,
             aes(x = lon, y = lat),
             size = 0.3) +
  theme_void()+
  theme(plot.subtitle= element_text(hjust = 0.5)) +
  ggsave("./vis/map_aian_fe_full.pdf")

ggplot(data = nhgis) +
  geom_sf(size = 0.1, color = "gray60", fill = "white") +
  geom_sf(data = nhgis_aian,
          color = "dodgerblue", fill = "dodgerblue",
          alpha = 0.5, size = 0.01) +
  coord_sf(crs = st_crs(nhgis), datum = NA,
           xlim = c(b["xmin"], b["xmax"]),
           ylim = c(b["ymin"], b["ymax"])) +
  geom_point(data = fe,
             aes(x = lon, y = lat),
             size = 0.3) +
  theme_void()+
  ggsave("./vis/map_aian_fe_crop.pdf")

