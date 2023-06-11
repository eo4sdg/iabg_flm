library(sf)
library(gdfR)
library(fs)
library(tidyverse)
library(terra)
library(landscapemetrics)
library(tictoc)

path <- list(aoi = "data/aoi/aoi_gadm_test.shp",
             raster = "data/hessen/hessen_type_test2.tif")


aoi <- st_read(path$aoi)


tic()
x <- calculate_flm(aoi, max_area = 10000000, plot_id = "NAME_4")
toc()

