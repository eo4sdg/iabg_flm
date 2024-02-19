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

########################### F- TEP #############################################
glc_folders_eodata <- system("find /eodata/CLMS/Global/Vegetation/Global_Land_Cover/ -maxdepth 2 -type d", intern = TRUE)

##### Setting the directories --------------------------------------------------
in_dir <- "in_dir"  # your in_dir folder
proc_dir <- "proc_dir"  # your proc_dir folder
# worker_dir <- cmd_args[3]
out_dir <- "out_dir"  # your out_dir folder
workflow_dir <- getwd()  # your project folder
########################### END F- TEP #########################################

########################### LOCALLY ############################################
list_directories <- function(base_dir, max_depth = 1) {
    if (max_depth < 1) return(base_dir)
    dirs <- list.files(base_dir, full.names = TRUE, recursive = FALSE, include.dirs = TRUE)
    dirs <- dirs[dir.exists(dirs)] # Keep only directories
    if (max_depth == 1) return(dirs)
    for (dir in dirs) {
        dirs <- c(dirs, list_directories(dir, max_depth - 1))
    }
    return(dirs)
}

glc_folders_eodata <- list_directories("D:/EO4SDG/codes/LM/eo4sdg-forest-flm/eodata/CLMS/Global/Vegetation/Global_Land_Cover", max_depth = 1)
glc_folders_eodata
##### Setting the directories --------------------------------------------------
# worker_dir <- cmd_args[3]
in_dir <- path_create("in_dir")  # your in_dir folder
proc_dir <- path_create("proc_dir")  # your proc_dir folder
out_dir <- path_create("out_dir") # your out_dir folder
workflow_dir <- getwd()  # your project folder
########################### END LOCALLY ########################################


path <- list(aoi = "data/aoi/aoi_gadm_test.shp", ## we can switch it to WKT POLYGON?
             proc_dir = proc_dir, # change it to proc_dir, actually we can delete it
             file.path(out_dir, "output_files")) # change the path to PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif


##### load functions -----------------------------------------------------------

source(file.path(workflow_dir, "functions_landscape_metrics.R"))
source(file.path(workflow_dir, "functions_gdfR.R"))


polygon_wkt <- "POLYGON ((9.98457 50.554256, 9.98457 50.602839, 10.05744 50.602839, 10.05744 50.554256, 9.98457 50.554256))"
polygon_wkt_sf <- sf::st_as_sfc(polygon_wkt)
aoi <- terra::vect(polygon_wkt_sf)
terra::crs(aoi) <- "EPSG:4326"
aoi<- sf::st_as_sf(aoi)

# Load functions from R scripts ------------------------------------------------
source(file.path(workflow_dir, "functions_download_forest_mask.R"))
source(file.path(workflow_dir, "functions_dir_management.R"))
output_images_path <- file.path(out_dir, "output_images")

create_dir(output_images_path)


############################### CODE TO GET FOREST MASK TILES ##################

library(stringr)

### USER INPUT #################################
specified_year <- "2019"
crs_wkt <- 4326
### END USER INPUT #############################

print(path$proc_dir)

glc_forest_tiles <- path_to_glc_forest_tiles(glc_folders_eodata, polygon_wkt, crs_wkt, specified_year)
print(glc_forest_tiles)
output_filename <- "PROBAV_LC100_global_v3.0.1_2019_merged.tif"
output_path <- file.path(path$procdir, output_filename)

merge_raster_from_paths(glc_forest_tiles, output_path)
files <- list.files(path = path$procdir)
print("blablabla")
print(files)


########################### END CODE TO GET FOREST MASK TILES ##################
################################################################################

################################################################################
# Begin processing of lc map ---------------------------------------------------

# END --------------------------------------------------------------------------
################################################################################

########################## LANDSCAPE METRICS ###################################

path <- list(proc_dir = proc_dir, # change it to proc_dir, actually we can delete it
             lc_raster = "//gdfs06/DATEN09/ESA-EO4SDG_D9/01_inputData/openData/raster/PROBAV_LC100/PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif",
             output_files = file.path(out_dir, "output_files")) # change the path to PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif

##### calculate forest landscape metrics ---------------------------------------
calculate_flm(aoi, lc = path$lc_raster, tempdir = path$proc_dir, outdir = out_dir)
path$metrics <- file.path(out_dir, "metrics.csv")

##### apply selection methods --------------------------------------------------
calc_beta_rank(df = path$metrics, outdir = out_dir)
path$metrics_ranked <- file.path(out_dir, "metrics_ranked.csv")

# ##### generate maps of the selected metrics ----------------------------------
plotdir <- file.path(out_dir, "output_files")
make_metric_maps(landscape = path$lc_raster,
                 aoi = aoi,
                 ranks = path$metrics_ranked,
                 tempdir = path$proc_dir,
                 plotdir = plotdir)
path$metrics_report <- file.path(out_dir, "flm_report.pdf") # output tbd

# ##### end --------------------------------------------------------------------
########################## END LANDSCAPE METRICS ###############################

########################MAKE REPORT#############################################
# Generating r markdown --------------------------------------------------------

render("flm_report.Rmd",
       output_dir = out_dir)

########################END REPORT#############################################




