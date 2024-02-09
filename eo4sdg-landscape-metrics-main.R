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

##### Setting the directories --------------------------------------------------
# worker_dir <- cmd_args[3]
in_dir <- path_create("in_dir")  # your in_dir folder
proc_dir <- path_create("proc_dir")  # your proc_dir folder
out_dir <- path_create("out_dir") # your out_dir folder
workflow_dir <- getwd()  # your project folder


polygon_wkt <- "POLYGON ((9.98457 50.554256, 9.98457 50.602839, 10.05744 50.602839, 10.05744 50.554256, 9.98457 50.554256))"
polygon_wkt_sf <- sf::st_as_sfc(polygon_wkt)
aoi <- terra::vect(polygon_wkt_sf)
terra::crs(aoi) <- "EPSG:4326"
aoi<- st_as_sf(aoi)


path <- list(proc_dir = proc_dir, # change it to proc_dir, actually we can delete it
             lc_raster = "//gdfs06/DATEN09/ESA-EO4SDG_D9/01_inputData/openData/raster/PROBAV_LC100/PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif",
             output_files = file.path(out_dir, "output_files")) # change the path to PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif










##### calculate forest landscape metrics ---------------------------------------
calculate_flm(aoi, lc = path$lc_raster, tempdir = path$proc_dir)
path$metrics <- file.path(path$proc_dir, "metrics.csv")

##### apply selection methods --------------------------------------------------
calc_beta_rank(df = path$metrics, tempdir = path$proc_dir)
path$metrics_ranked <- file.path(path$proc_dir, "metrics_ranked.csv")

# ##### generate maps of the selected metrics ------------------------------------
make_metric_maps(landscape = path$lc_raster, plotdir = out_dir)
path$metrics_maps <- file.path(out_dir, "plots.R") # output tbd

# ##### end ----------------------------------------------------------------------

