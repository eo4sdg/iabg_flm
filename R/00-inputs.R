library(sf)
library(gdfR)
library(fs)
library(tidyverse)
library(terra)
library(landscapemetrics)
library(tictoc)


# data is located on Daten9
# see readme on how to get the data

path <- list(aoi = "data/aoi/aoi_gadm_test.shp",
             raster = "data/hessen/hessen_type_test2.tif")


aoi <- st_read(path$aoi)

