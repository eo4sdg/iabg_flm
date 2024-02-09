# main.R

# This script executes the forest landscape metrics service.

############################# libraries ########################################
library(landscapemetrics)
library(tidyverse)
library(sf)
library(corrr)
library(terra)

##### paths --------------------------------------------------------------------
# run once for example data acquisition:
# library(fs)
# a<- dir_ls("//gdfs06/DATEN09/ESA-EO4SDG_D9/02_analysis/FLM/Hessen/test_data/gitlab_v1", type = "directory")
# dir_copy(a, new_path = "data")
path <- list(aoi = "data/aoi/aoi_gadm_test.shp",
             raster = "data/hessen/hessen_type_test2.tif",
             tempdir = "data/temp",
             lc_raster = "path/to/lc.tif")
# for testing
# path$lc_raster = "//gdfs06/DATEN09/ESA-EO4SDG_D9/01_inputData/openData/raster/PROBAV_LC100/PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif"
aoi <- sf::st_read(path$aoi)

##### Setting the directories --------------------------------------------------
# worker_dir <- cmd_args[3]
in_dir <- path_create("in_dir")  # your in_dir folder
proc_dir <- path_create("proc_dir")  # your proc_dir folder
out_dir <- path_create("out_dir") # your out_dir folder
workflow_dir <- getwd()  # your project folder

##### load functions -----------------------------------------------------------
source(file.path(workflow_dir, "functions_landscape_metrics.R"))

##### calculate forest landscape metrics ---------------------------------------
calculate_flm(aoi, lc = path$lc_raster, tempdir = proc_dir)
path$metrics <- file.path(proc_dir, "metrics.csv")

##### apply selection methods --------------------------------------------------
calc_beta_rank(path$metrics)
path$metrics_ranked <- file.path(proc_dir, "metrics_ranked.csv")

##### generate maps of the selected metrics ------------------------------------
make_metric_maps(path$metrics_ranked)
path$metrics_maps <- file.path(path$tempdir, "metrics_maps.Rd") # output tbd

##### end ----------------------------------------------------------------------
