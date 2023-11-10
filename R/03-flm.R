

#' Calculate FLM
#'
#' Computes forest landscape metrics based on aoi. the input lc is hardcoded.
#'
#' @param aoi
#' @param plot_id
#' @param max_area
#' @param class_names
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
calculate_flm <- function(aoi, plot_id, max_area = 1000, class_names, ...){

    # Area and edge metrics
    #metrics = c('area','gyrate','pland', 'ca', 'lpi', 'te', 'ed', 'ta')

    # Shape and Compactness metrics
    metrics = c('shape', "circle", 'contig') #shape = shape index
                                             #circle = related circumscribing circle
                                             #contig = contiguity index
    # Core area metrics
    #metrics = c('cai', 'cpland' )

    # Aggregation metrics

    # initial checks
    if(aoi_too_big(aoi, max_area = max_area)) stop("aoi is too big")



    # load landscape
    landscape <-
        terra::rast(path$raster) %>%
        clip_aoi(aoi) # here put temp file

    # calculate metrics
    area_metrics <-
        sample_lsm(landscape,
                   aoi,
                   metric = metrics,
                   plot_id = pull(aoi, !!plot_id)
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
        pivot_wider(id_cols = plot_id,
                    names_from = c("level", 'class', 'metric'),
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


