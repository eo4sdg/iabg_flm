#' Calculate FLM
#'
#' Computes forest landscape metrics based on aoi. the input lc is hardcoded.
#'
#' @param aoi
#' @param plot_id
#' @param max_area
#' @param class_names
#' @param ...
#' @param x
#' @param metrics
#' @param tempdir
#'
#' @return
#' @export
#'
#' @examples
calculate_flm <- function(x,
                          aoi,
                          plot_id = NULL,
                          metrics = c('area', "ca", 'lpi', 'te'),
                          max_area = 1000,
                          class_names,
                          tempdir = "data/temp",
                          ...){

    # select metrics
    # metrics = c('area', "ca",  'ta', 'pland',
    #             'core', 'tca','cpland', 'cai', 'ed', 'lpi', 'te')



    # initial checks
    if(aoi_too_big(aoi, max_area = max_area)) stop("aoi is too big")

    gdfR::path_exists(tempdir, create = T)
    terra::terraOptions(tempdir = tempdir)

    # load landscape
    landscape <-
        clip_aoi(x, aoi, ...)

    # calculate metrics
    area_metrics <-
        sample_lsm(landscape,
                   aoi,
                   metric = metrics,
                   plot_id = dplyr::if_else(missing(plot_id), NULL, dplyr::pull(aoi, !!plot_id))
        )

    if(!missing(class_names)) {
        area_metrics <-
            area_metrics %>%
            mutate(class = recode(class, !!!setNames(class_names$cover, class_names$id)))
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

    return(list(area_metrics, area_metrics_spatial))
    # outputs
        # csv
        # render a rmd file with tables
            #
}

