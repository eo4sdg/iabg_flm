# eo4sdg-landscape-metrics-script.R

# This script executes the forest landscape metrics service.

############################# libraries ########################################
library(landscapemetrics)
library(tidyverse)
library(sf)
library(corrr)
library(terra)


### F-TEP ###
# Getting command line arguments
# cmd_args <- commandArgs(trailingOnly = TRUE)
#
# # Assigning the arguments to variables
# in_dir <- cmd_args[1]
# proc_dir <- cmd_args[2]
# worker_dir <- cmd_args[3]
# out_dir <- cmd_args[4]
# workflow_dir <- cmd_args[5]

in_dir <- "in_dir"  # your in_dir folder
proc_dir <- "proc_dir"  # your proc_dir folder
# worker_dir <- cmd_args[3]
out_dir <- "out_dir"  # your out_dir folder
workflow_dir <- getwd()  # your project folder

##### load functions -----------------------------------------------------------
source(file.path(workflow_dir, "functions_landscape_metrics.R"))
source(file.path(workflow_dir, "functions_gdfR.R"))

##### paths --------------------------------------------------------------------
# run once for example data acquisition:
# library(fs)
# a <- dir_ls("//gdfs06/DATEN09/ESA-EO4SDG_D9/02_analysis/FLM/Hessen/test_data/gitlab_v1", type = "directory")
# dir_copy(a, new_path = "data")


path <- list(aoi = "data/aoi/aoi_gadm_test.shp", ## we can switch it to WKT POLYGON?
             raster = "data/hessen/hessen_type_test2.tif", # ???
             proc_dir = proc_dir, # change it to proc_dir, actually we can delete it
             lc_raster = "path/to/lc.tif",
             file.path(out_dir, "output_files")) # change the path to PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif
# for testing
path$lc_raster = "//gdfs06/DATEN09/ESA-EO4SDG_D9/01_inputData/openData/raster/PROBAV_LC100/PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif"
aoi <- sf::st_read(path$aoi)

polygon_wkt <- "POLYGON ((9.88457 50.504256, 9.88457 50.992839, 10.10744 50.992839, 10.10744 50.504256, 9.88457 50.504256))"
aoi <- sf::st_as_sfc(polygon_wkt)

polygon_wkt_sf <- sf::st_as_sfc(polygon_wkt)
aoi <- terra::vect(polygon_wkt_sf)
terra::crs(aoi) <- "EPSG:4326"
aoi<- st_as_sf(aoi)

##### Setting the directories --------------------------------------------------
# worker_dir <- cmd_args[3]
in_dir <- path_create("in_dir")  # your in_dir folder
proc_dir <- path_create("proc_dir")  # your proc_dir folder
out_dir <- path_create("out_dir") # your out_dir folder
workflow_dir <- getwd()  # your project folder





##### calculate forest landscape metrics ---------------------------------------
calculate_flm(aoi, lc = path$lc_raster, tempdir = path$proc_dir)
path$metrics <- file.path(path$proc_dir, "metrics.csv")

##### apply selection methods --------------------------------------------------
calc_beta_rank(path$metrics)
path$metrics_ranked <- file.path(path$proc_dir, "metrics_ranked.csv")

# ##### generate maps of the selected metrics ------------------------------------
# make_metric_maps()
# path$metrics_maps <- file.path(path$proc_dir, "metrics_maps.Rd") # output tbd
#
# ##### end ----------------------------------------------------------------------
