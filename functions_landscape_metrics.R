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
    # TODO fix path_exists
    terra::terraOptions(tempdir = tempdir)

    if(!missing(class_names)){
        if(!all(names(class_names) %in% c("name", "id"))) stop("class_names must be a data frame with columns 'id' and 'name'")
    }

    # load landscape
    if(missing(lc)){
        stop("please input `lc` a land cover map")
    } else if(inherits(lc, "character")){
        lc<- terra::rast(lc)
    }

    aoi<- sf::st_transform(aoi, terra::crs(lc))
    landscape <- terra::crop(lc,
                      aoi,
                      filename = file.path(tempdir, "lc_from_aoi.tif"),
                      overwrite = TRUE)


    # calculate metrics
    area_metrics <-
        landscapemetrics::sample_lsm(landscape,
                                     aoi,
                                     plot_id = plot_id)

    if(!missing(class_names)) {
        area_metrics <-
            area_metrics %>%
            dplyr::mutate(class = recode(class, !!!setNames(class_names$name, class_names$id)))
    }

    #the object area metrics is the one used in script 4
    write.csv(area_metrics, file = file.path(tempdir, "metrics.csv")) #


    # outputs
    # area_metrics_w <-
    #     area_metrics %>%
    #     dplyr::filter(level != 'patch') %>%
    #     dplyr::pivot_wider(id_cols     = plot_id,
    #                 names_from  = c("level", 'class', 'metric'),
    #                 values_from = value)

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

#' Calculate beta and rank accordingly
#'
#' This function calculates the beta coefficient of each metric, and
#' ranks them in descending order. In case of ties, it calculates the
#' correlation of each metric against all others, calculates the average
#' this correlations, and ranks them according to the least average correlation.
#'
#'
#' @param df metrics csv or data frame derived from calculate flm function
#' @param correlation logical. Return correlation matrices?
#' @param landscape logical. Return landscape object?
#' @param df_with_intermediate_steps logical. Return intermediate steps?
#' @param tempdir character. Folder where the temporary files should be created.
#'
#' @return
#' @export
#'
#' @examples
calc_beta_rank <- function(df,
                           correlation = FALSE,
                           landscape = FALSE,
                           df_with_intermediate_steps = FALSE,
                           tempdir = "data/temp"){
    # this function can be completely internal, no need for inputs
    if(inherits(df, "character")) df<- read.csv(file.path(tempdir, "metrics.csv"))

    # 0. function input checks
    required_names <- c("layer", "level", "class", "id", "metric", "value",
                        "plot_id")
    if(!inherits(df, "data.frame")) stop("input must be a data.frame")
    if(!all(required_names %in% names(df)))
        stop(paste0("input must contain columns with all of the following names: ",
                    paste0(required_names, collapse = ", ")))

    # 1. Beta calculation
    message("calculating beta")
    beta <- df |>
        dplyr::group_by(level, plot_id, metric) |>
        dplyr::summarise(min  = min(value),
                  max  = max(value),
                  beta = (max-min)/max) |>
        dplyr::mutate(rank = min_rank(desc(beta)))

    # 2. Correlation matrices (for tiebreaks)
    my_stretch <- function(z){
        z |>
            corrr::stretch() |>
            dplyr::group_by(x) |>
            dplyr::summarise(mean = abs(mean(r, na.rm = TRUE)))
    }
    message("calculating correlations")
    correlations <- df |>
        dplyr::group_by(level, plot_id) |>
        dplyr::select(id, class, metric, value) |>
        tidyr::nest() |>
        dplyr::mutate(pivoted = map(data, pivot_wider, names_from=metric, values_from=value),
               pivoted = map(pivoted, select, -id, -class),
               corr    = map(pivoted, correlate)) |>
        dplyr::mutate(corr_up  = map(corr, shave),
               corr_up2 = map(corr_up, my_stretch))

    # 2.1 create landsc_ object
    if(landscape) {
        landsc_ <- correlations |>
            dplyr::filter(level == "landscape") |>
            dplyr::pull(pivoted) |>
            dplyr::bind_rows()
    } else{
        landsc_ <- NULL
    }

    # 2.2 make the tiebreaks from here
    message("starting tiebreaks")
    my_comb_rank <- function(y){
        y |>
            dplyr::arrange(dplyr::desc(beta), mean) |>
            tibble::rowid_to_column("rank_no_ties") |>
            dplyr::relocate(rank_no_ties, .after = last_col())
    }

    out <-
        beta |>
        dplyr::group_by(level, plot_id) |>
        tidyr::nest(.key = "beta") |>
        dplyr::left_join(correlations) |>
        dplyr::mutate(beta2 = purrr::map2(beta, corr_up2, left_join, by = c(metric = "x")),
               beta3 = purrr::map(beta2, my_comb_rank))

    # 3. output formatting
    message("formatting output")
    if(!df_with_intermediate_steps){
        out <-
            out |>
            dplyr::select(level, plot_id, beta3) |>
            dplyr::rename(beta = beta3)
    }

    if(!correlation) correlations <- NULL

    # save to csv
    to_save<- out |>
        tidyr::unnest(beta)
    write.csv(to_save, file = file.path(tempdir, "metrics_ranked.csv"))

    return(list(ranked_data = out,
                correlations = correlations,
                landsc_ = landsc_))
}
