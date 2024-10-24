# eo4sdg-landscape-metrics-script.R

# This script executes the forest landscape metrics service.

############################# libraries ########################################
library(landscapemetrics)
library(tidyverse)
library(sf)
library(corrr)
library(terra)

# Getting command line arguments
cmd_args <- commandArgs(trailingOnly = TRUE)

# Assigning the arguments to variables
in_dir <- cmd_args[1]
proc_dir <- cmd_args[2]
worker_dir <- cmd_args[3]
out_dir <- cmd_args[4]
workflow_dir <- cmd_args[5]
polygon_wkt <- cmd_args[6]

##### load functions -----------------------------------------------------------
source(file.path(workflow_dir, "functions-landscape-metrics.R"))
source(file.path(workflow_dir, "functions_gdfR.R"))

##### paths --------------------------------------------------------------------
# run once for example data acquisition:
# library(fs)
# a <- dir_ls("//gdfs06/DATEN09/ESA-EO4SDG_D9/02_analysis/FLM/Hessen/test_data/gitlab_v1", type = "directory")
# dir_copy(a, new_path = "data")

##### Setting the directories --------------------------------------------------

path <- list(aoi = "data/aoi/aoi_gadm_test.shp", ## we can switch it to WKT POLYGON?
             proc_dir = proc_dir, # change it to proc_dir, actually we can delete it
          #  lc_raster = "path/to/lc.tif",
             file.path(out_dir, "output_files")) # change the path to PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif
# for testing
#path$lc_raster = "//gdfs06/DATEN09/ESA-EO4SDG_D9/01_inputData/openData/raster/PROBAV_LC100/PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif"
#aoi <- sf::st_read(path$aoi)


#polygon_wkt <- "POLYGON ((9.98457 50.554256, 9.98457 50.602839, 10.05744 50.602839, 10.05744 50.554256, 9.98457 50.554256))" # for now imported as user input
polygon_wkt_sf <- sf::st_as_sfc(polygon_wkt)
aoi <- terra::vect(polygon_wkt_sf)
terra::crs(aoi) <- "EPSG:4326"
aoi<- st_as_sf(aoi)

##########################################################################################################################################################
# Load functions from R scripts -------------------------------------------
source(file.path(workflow_dir, "functions_download_forest_mask.R"))
source(file.path(workflow_dir, "functions_dir_management.R"))
output_files_path <- file.path(out_dir, "output_files")

create_dir(output_files_path)

#######################################################################
glc_folders_eodata <- system("find /eodata/CLMS/Global/Vegetation/Global_Land_Cover/ -maxdepth 2 -type d", intern = TRUE)

############################### CODE TO GET FOREST MASK TILES ######################################

library(stringr)

### USER INPUT #################################
specified_year <- "2019"
crs_wkt <- 4326
### END USER INPUT #############################

print(path$proc_dir)

glc_forest_tiles <- path_to_glc_forest_tiles(glc_folders_eodata, polygon_wkt, crs_wkt, specified_year)
print(glc_forest_tiles)
glc_tile_filename <- "PROBAV_LC100_global_v3.0.1_2019_merged.tif"
glc_path <- file.path(path$proc_dir, glc_tile_filename)
path$lc_raster <- glc_path
merge_raster_from_paths(glc_forest_tiles, glc_path)
files <- list.files(path = path$proc_dir)
print(paste0("These are the files: ", paste0(files, collapse = ",\n")))

############################### END CODE TO GET FOREST MASK TILES ######################################
##########################################################################################################################################################



##### calculate forest landscape metrics ---------------------------------------
message("starting calculating_fm()")
crs(rast(glc_path))

calculate_flm(aoi, lc = glc_path, tempdir = path$proc_dir, outdir = output_files_path)
path$metrics <- file.path(output_files_path, "metrics.csv")

##### apply selection methods --------------------------------------------------
message("starting calc_beta_rank()")
calc_beta_rank(df = path$metrics, outdir = output_files_path)
path$metrics_ranked <- file.path(output_files_path, "metrics_ranked.csv")


make_metric_maps(landscape = glc_path,
                 aoi = aoi,
                 ranks = path$metrics_ranked,
                 tempdir = path$proc_dir,
                 plotdir = output_files_path)

# ##### generate maps of the selected metrics ------------------------------------
#message("starting make_metric_maps()")
#make_metric_maps(landscape = glc_path, aoi = aoi, tempdir = path$proc_dir, plotdir = output_files_path)
#print("maps created")
path$metrics_maps <- file.path(output_files_path, "plots.pdf") # output tbd
# print(list.files(recursive = TRUE))
# ##### end ----------------------------------------------------------------------





Sys.getenv("RSTUDIO_PANDOC")
#file.copy(rmarkdown::pandoc_exec(), "/usr/local/bin/pandoc", overwrite = TRUE)

rmarkdown::pandoc_available()

plotdir <- output_files_path



aoi<- terra::vect(aoi)
rmarkdown::render(file.path(workflow_dir, "flm_report.Rmd"),
       output_dir = output_files_path,
       output_format = NULL)
