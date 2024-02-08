#' Calculate FLM
#'
#' Computes forest landscape metrics based on aoi. the input lc is hardcoded.
#'
#' @param aoi shp file with administrative units.
#' @param plot_id data frame with columns: "id", "name", id matches ids in shp file.
#'                Integer.  # check names
#' @param max_area
#' @param class_names
#' @param ...
#' @param lc classified raster: land cover map
#' @param metrics
#' @param tempdir
#'
#' @return
#' @export
#'
#' @examples
calculate_flm <- function(aoi,
                          lc,
                          ... ,
                          plot_id = NULL,
                          max_area = 1000,
                          class_names,
                          tempdir = "data/temp"){

    # select metrics
    # metrics = c('area', "ca",  'ta', 'pland',
    #             'core', 'tca','cpland', 'cai', 'ed', 'lpi', 'te')



    # initial checks
    # if(aoi_too_big(aoi, max_area = max_area)) stop("aoi is too big")
    if(missing(plot_id)) plot_id <- NULL
    if(!is.null(plot_id)) plot_id <- dplyr::pull(aoi, {{plot_id}})
    gdfR::path_exists(tempdir, create = T)
    #TODO fix path_exists
    terra::terraOptions(tempdir = tempdir)

    if(!missing(class_names)){
        if(!all(names(class_names) %in% c("name", "id"))) stop("class_names must be a data frame with columns 'id' and 'name'")
    }

    # load landscape
    if(missing(lc)){
        lc <- rast("//gdfs06/DATEN09/ESA-EO4SDG_D9/01_inputData/openData/raster/PROBAV_LC100/PROBAV_LC100_global_v3.0.1_2015-base_Forest-Type-layer_EPSG-4326.tif")
    }

    aoi<- sf::st_transform(aoi, crs(lc))
    landscape <- crop(lc,
                      aoi,
                      filename = file.path(tempdir, "lc_from_aoi.tif")) # put outdir


    # calculate metrics
    area_metrics <-
        landscapemetrics::sample_lsm(landscape,
                   aoi,
                   plot_id = plot_id)

    if(!missing(class_names)) {
        area_metrics <-
            area_metrics %>%
            mutate(class = recode(class, !!!setNames(class_names$name, class_names$id)))
    }

    #the object area metrics is the one used in script 4
    write.csv(area_metrics, file = file.path(tempdir, "metrics.csv")) #


    # outputs
    area_metrics_w <-
        area_metrics %>%
        filter(level != 'patch') %>%
        pivot_wider(id_cols     = plot_id,
                    names_from  = c("level", 'class', 'metric'),
                    values_from = value)

    # area_metrics_spatial <-
    #     left_join(aoi,
    #               area_metrics_w,
    #               by = setNames("plot_id", plot_id))
    # DOES NOT WORK IF plot_id NOT PROVIDED FROM BEGINNING

    #return metrics
    # return(list(area_metrics = area_metrics,
    #             area_metrics_spatial = area_metrics_spatial))
    return(area_metrics)
    # # outputs
        # csv
        # render a rmd file with tables
            #
}

