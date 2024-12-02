# eo4sdg-landscape-metrics-script.R

# This script executes the forest landscape metrics service.
# install.packages("scales")
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
aoi<- sf::st_as_sf(aoi)

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
calculate_flm(aoi, lc = path$lc_raster, tempdir = path$proc_dir, outdir = path$proc_dir)
path$metrics <- file.path(path$proc_dir, "metrics.csv")

##### apply selection methods --------------------------------------------------
calc_beta_rank(df = path$metrics, outdir = output_files_path)
path$metrics_ranked <- file.path(output_files_path, "metrics_ranked.csv")

##### make maps ----------------------------------------------------------------
tempdir <- path$proc_dir
lc<- terra::rast(glc_path)
aoi<- sf::st_transform(aoi, terra::crs(lc))
print("croping")

landscape <- terra::crop(lc,
                         aoi,
                         mask = TRUE,
                         filename = file.path(tempdir, "lc_from_aoi.tif"),
                         overwrite = TRUE)
print("project to m")
landscape <- project_to_m(landscape, tempdir = tempdir)
aoi<- sf::st_transform(aoi, terra::crs(landscape))
print("get forest classes")
lc_forest_2019 <- select_forest_from_glc_lcc(landscape, all = FALSE, binary = FALSE)

print("calculate patch metrics")
lm_2019 <- landscapemetrics::spatialize_lsm(lc_forest_2019,
                                            level = "patch",
                                            metric = get_patch_metric_names() ,
                                            to_disk = TRUE)
print("calculate diversity metric")
di_2019 <- landscapemetrics::lsm_l_shdi(lc_forest_2019)



print("saving outputs")
out <- terra::rast(lm_2019$layer_1) |> terra::mask(lc_forest_2019, maskvalues = -9999)
for(i in seq_along(lm_2019$layer_1)){
    terra::writeRaster(out[[i]]|> terra::mask(aoi),
                       filename = file.path(output_files_path,
                                            paste0((lm_2019$layer_1 |> names())[[i]],
                                                   "_flm_2019.tif")))
}
terra::writeRaster(lc_forest_2019, file.path(output_files_path, "forest_classes_2019.tif"))
write.csv(di_2019, file = file.path(output_files_path, "diversity_index_2019.csv"))
