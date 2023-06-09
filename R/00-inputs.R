library(sf)
library(gdfR)
library(fs)
library(tidyverse)
library(terra)

path <- list(aoi = "data/eo4sdg_aoi.shp",
             jrc = "//gdfs06/DATEN09/ESA-EO4SDG_D9/01_inputData/openData/raster/JRC_TMF",
             tmf = "data/jrc_tmp")


aoi <- st_read(path$aoi)
