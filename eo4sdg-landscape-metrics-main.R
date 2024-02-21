# eo4sdg-landscape-metrics-script.R

# This script executes the forest landscape metrics service.

############################# libraries ########################################
library(landscapemetrics)
library(tidyverse)
library(sf)
library(corrr)
library(terra)
library(rmarkdown)
library(knitr)

################################################################################

# ONLY RUN IN F-TEP ------------------------------------------------------------
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

# Mexico AOI and lc map with 12 forest classes to test locally --JOSE
polygon_wkt <- "POLYGON ((-100.389756 20.506008, -100.324451 20.509012, -100.348282 20.557294, -100.388840 20.558581, -100.389756 20.506008))"
# polygon_wkt <- "POLYGON((104.77111816406246 8.49138807297875,105.01281738281247 8.53484919469527,105.39733886718746 8.844365054952519,105.53466796874996 8.985461927760653,106.78710937499996 9.65761552211437,106.88598632812496 10.241951639623437,107.67700195312496 10.409481792727007,108.17138671874999 10.819817515515382,109.33593750000001 11.46387438055038,109.57763671874999 12.7957315905158,109.46228027343749 13.670007054279566,109.09973144531251 14.995198836057881,107.4957275390625 14.687224547196834,107.20458984375001 14.213801273022412,107.38037109374999 13.488460431671314,107.40234375 12.567967727957026,106.81457519531247 12.246075604574798,106.21032714843747 11.91304099528864,105.67199707031247 11.64954641194305,105.73791503906247 11.170318336920303,104.39208984374997 10.441896638117186,104.85900878906247 9.86874606765899,104.71069335937497 9.148197713142977,104.73266601562497 8.768367359687133,104.65576171874997 8.616325445300276,104.70520019531246 8.53484919469527,104.77111816406246 8.49138807297875))" #F-TEP failed

polygon_wkt_sf <- sf::st_as_sfc(polygon_wkt)
aoi <- terra::vect(polygon_wkt_sf)
terra::crs(aoi) <- "EPSG:4326"
aoi<- sf::st_as_sf(aoi)

plot(aoi)


path <- list(proc_dir = proc_dir, # change it to proc_dir, actually we can delete it
             lc_raster = "//gdfs06/DATEN09/ESA-EO4SDG_D9/01_inputData/openData/raster/PROBAV_LC100/PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif",
             output_files = file.path(out_dir, "output_files")) # change the path to PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif

path$lc_raster <- "C:/Users/Cortes-Resendiz/Downloads/PROBAV_LC100_global_v3.0.1_2019_merged.tif"
# path$lc_raster <- "//gdfs06/DATEN09/ESA-EO4SDG_D9/01_inputData/openData/raster/PROBAV_LC100/PROBAV_LC100_global_v3.0.1_2019_Vietnam_merged.tif"
# error when handling large aoi
# metrics csv -> names(attributes) mask must be same length as vector
# Error in names(object) <- nm :
# 'names' attribute [3] must be the same length as the vector [2]

########################## LANDSCAPE METRICS ###################################

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


path$metrics_maps <- file.path(out_dir, "plots.pdf") # output tbd
path$metrics_report <- file.path(out_dir, "flm_report.pdf") # output tbd

# ##### end --------------------------------------------------------------------
########################## END LANDSCAPE METRICS ###############################

########################MAKE REPORT#############################################
# Generating r markdown --------------------------------------------------------

# only works if we have run the code above and have the files
# - metrics.csv,
# - metrics_ranked.csv
# and the following objects in the environment:
# - aoi
# - others (?): TODO, which ones
# (NOTE: depending on environment objects is NOT good practice.)
# TODO: Save all needed inputs and load them within the Rmd file.

aoi<- terra::vect(aoi)
jose_austausch<- "P:/Sonstige/Austausch/cortes-resendiz/flm_report"
render("flm_report.Rmd",
       output_dir = out_dir, # out_dir # for F-TEP CHANGE THIS
       output_format = "all")

path$report_html <- file.path(out_dir, "flm_report.html")
path$report_pdf <- file.path(out_dir, "flm_report.pdf")

fs::file_copy(path$report_html, new_path = jose_austausch, overwrite = TRUE)
fs::file_copy(path$report_pdf, new_path = jose_austausch, overwrite = TRUE)
## \\DRESDENGDFD\Proj\Sonstige\Austausch\cortes-resendiz\flm_report
########################END REPORT#############################################
