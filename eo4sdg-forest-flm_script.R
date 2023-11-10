library(sf)
library(gdfR)
library(fs)
library(tidyverse)
library(terra)
library(landscapemetrics)
library(tictoc)
library(leaflet)
library(DT)
library(leafpop)

# Load the functions from R folder
source("R/01-aoicheck.R")
source("R/02-clip.R")
source("R/03-flm.R")


# data is located on Daten9
# see readme on how to get the data

path <- list(aoi = "data/aoi/aoi_gadm_test.shp",
             raster = "data/hessen/hessen_type_test2.tif")


aoi <- st_read(path$aoi)
max_area = 10000000
plot_id = "NAME_4"

##############################################
#
#
# # initial checks
# if(aoi_too_big(aoi, max_area = max_area)) stop("aoi is too big")
#
#
# # load landscape
# landscape <-
#     terra::rast(path$raster) %>%
#     clip_aoi(aoi) # here put temp file
#
#
#
# # calculate metrics
# area_metrics <-
#     sample_lsm(landscape,
#                aoi,
#                metric = metrics,
#                plot_id = pull(aoi, !!plot_id)
#     )
#
# if(!missing(class_names)) {
#     area_metrics <-
#         area_metrics %>%
#         mutate(class = recode(class, !!!setNames(class_names$cover, class_names$id)))
# }
#
# # outputs
# area_metrics_w <-
#     area_metrics %>%
#     filter(level != 'patch') %>%
#     pivot_wider(id_cols = plot_id,
#                 names_from = c("level", 'class', 'metric'),
#                 values_from = value)
#
# area_metrics_spatial <-
#     left_join(aoi,
#               area_metrics_w,
#               by = setNames("plot_id", plot_id))
#

#############################################
# Exkursus: Plotting initial landscape
# convert to factor (for plotting purposes)
cls <- data.frame(id=1:3, cover=c("Nadelwald","Laubwald", "Mischwald"))
levels(landscape) <- cls
color = c("red", "green", "blue")
plot(landscape, col=color)
plot(st_geometry(aoi), add = T)
text(st_coordinates(st_centroid(aoi)), aoi$NAME_4)



#############################################
# Running the 03-flm script for different metric categories we get:

tic()
x_area <- calculate_flm(aoi, max_area = 10000000, plot_id = "NAME_4")
toc()


tic()
x_shape <- calculate_flm(aoi, max_area = 10000000, plot_id = "NAME_4")
toc()

tic()
x_core <- calculate_flm(aoi, max_area = 10000000, plot_id = "NAME_4")
toc()

