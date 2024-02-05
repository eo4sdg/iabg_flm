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
#' @param x classified raster
#' @param metrics
#' @param tempdir
#'
#' @return
#' @export
#'
#' @examples
calculate_flm <- function(x,
                          aoi,
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
    if(!is.null(plot_id)) plot_id2 <- dplyr::pull(aoi, {{plot_id}})
    gdfR::path_exists(tempdir, create = T)
    #TODO fix path_exists
    terra::terraOptions(tempdir = tempdir)

    if(!missing(class_names)){
        if(!all(names(class_names) %in% c("name", "id"))) stop("class_names must be a data frame with columns 'id' and 'name'")
    }

    # load landscape
    landscape <-
        clip_aoi(x, aoi) # put outdir



    # calculate metrics
    area_metrics <-
        landscapemetrics::sample_lsm(landscape,
                   aoi,
                   plot_id = plot_id2,
                   ...
        )


    if(!missing(class_names)) {
        area_metrics <-
            area_metrics %>%
            mutate(class = recode(class, !!!setNames(class_names$name, class_names$id)))
    }

    # outputs
    area_metrics_w <-
        area_metrics %>%
        filter(level != 'patch') %>%
        pivot_wider(id_cols     = plot_id,
                    names_from  = c("level", 'class', 'metric'),
                    values_from = value)

    area_metrics_spatial <-
        left_join(aoi,
                  area_metrics_w,
                  by = setNames("plot_id", plot_id))

    return(list(area_metrics = area_metrics,
                area_metrics_spatial = area_metrics_spatial))
    # outputs
        # csv
        # render a rmd file with tables
            #
}

