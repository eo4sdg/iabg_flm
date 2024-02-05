library(sf)
library(gdfR)
library(fs)
library(tidyverse)
library(terra)
library(landscapemetrics)
library(tictoc)


# data is located on Daten9
# see readme on how to get the data
# run once for data acquisition:
# a<- dir_ls("//gdfs06/DATEN09/ESA-EO4SDG_D9/02_analysis/FLM/Hessen/test_data/gitlab_v1", type = "directory")
# dir_copy(a, new_path = "data")

path <- list(aoi = "data/aoi/aoi_gadm_test.shp",
             raster = "data/hessen/hessen_type_test2.tif")

aoi <- st_read(path$aoi)

