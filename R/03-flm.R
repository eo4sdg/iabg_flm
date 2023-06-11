

function(aoi, plot_id, max_area, ...){

    metrics = c('area', "ca",  'ta', 'pland',
                'core', 'tca','cpland', 'cai', 'ed', 'lpi', 'te')

    # initial checks
    if(aoi_too_big(aoi, max_area = max_area)) stop("aoi is too big")

    # metrics

    # load landscape
    landscape <-
        terra::rast("path$raster") %>%
        clip_aoi(r, aoi, ...) # here put temp file

    # calculate metrics (area and some more)
    area_metrics <-
        sample_lsm(landscape,
                   aoi,
                   metric = metrics,
                   plot_id = aoi[, "plot_id"]
        ) #%>%
        #mutate(class = recode(class, !!!setNames(cls$cover, cls$id))) # this is to rename the classes from numeric to character


    area_metrics_w <-
        area_metrics %>%
        filter(level != 'patch') %>%
        pivot_wider(id_cols = plot_id,
                    names_from = c("level", 'class', 'metric'),
                    values_from = value)

    area_metric_spatial <-
        left_join(aoi,
                  area_metrics_w,
                  by = setNames("plot_id", plot_id))

    # outputs
        # csv
        # render a rmd file with tables
            #
}
