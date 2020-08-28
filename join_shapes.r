##############################

library(tidyverse)
library(ipumsr)
library(sf)
library(rgdal)
select<-dplyr::select


### load nhgis data
nhgis_csv_file <- "./data/nhgis0036_csv.zip"
nhgis_shp_file <- "./data/nhgis0036_shapefile_tl2017_us_state_2017.zip"
aian_csv_file <- "./data/nhgis0035_csv.zip"
aian_shp_file <- "./data/nhgis0035_shape.zip"

### add state names, join to whatever data you are using
nhgis <- read_nhgis_sf(
  data_file = nhgis_csv_file,
  shape_file = nhgis_shp_file
)

nhgis_aian <- read_nhgis_sf(
  data_file = aian_csv_file,
  shape_file = aian_shp_file
)

d <- data.frame(lon=fe$Longitude, lat=fe$Latitude)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+init=esri:102003")
d.convert <- data.frame(spTransform(d, CRS.new))
fe<- bind_cols(fe, d.convert)


