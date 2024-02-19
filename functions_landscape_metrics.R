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
                          plot_id,
                          max_area = 1000,
                          class_names,
                          tempdir = "data/temp",
                          outdir){

    # select metrics
    # metrics = c('area', "ca",  'ta', 'pland',
    #             'core', 'tca','cpland', 'cai', 'ed', 'lpi', 'te')



    # initial checks
    # if(aoi_too_big(aoi, max_area = max_area)) stop("aoi is too big")
    if(missing(plot_id)) plot_id <- NULL
    if(!is.null(plot_id)) plot_id <- dplyr::pull(aoi, {{plot_id}})
    path_exists(tempdir, create = T)
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

    landscape <- project_to_m(landscape, tempdir = tempdir)
    aoi<- sf::st_transform(aoi, terra::crs(landscape))
    landscape<- terra::subst(landscape, NA, 0,
                             raw = TRUE,
                             filename = file.path(tempdir, "landscape_no_NA.tif"),
                             overwrite = TRUE)    # calculate metrics
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
    write.csv(area_metrics, file = file.path(outdir, "metrics.csv")) #


    # outputs
    if(!is.null(plot_id)){
        area_metrics_w <-
            area_metrics %>%
            dplyr::filter(level != 'patch') %>%
            tidyr::pivot_wider(id_cols     = plot_id,
                               names_from  = c("level", 'class', 'metric'),
                               values_from = value)

        area_metrics_spatial <-
            dplyr::left_join(aoi,
                      area_metrics_w,
                      by = setNames("plot_id", plot_id))
        # DOES NOT WORK IF plot_id NOT PROVIDED FROM BEGINNING
        write.csv(area_metrics_spatial, file = file.path(outdir, "metrics_spatial.csv")) #

        #return metrics
        return(list(area_metrics = area_metrics,
                    area_metrics_spatial = area_metrics_spatial))
    }

        return(area_metrics)
    # # outputs
    # csv
    # render a rmd file with tables

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
#' @param df_with_intermediate_steps logical. Return intermediate steps?
#' @param tempdir character. Folder where the temporary files should be created.
#'
#' @return
#' @export
#'
#' @examples
calc_beta_rank <- function(df,
                           correlation = FALSE,
                           df_with_intermediate_steps = FALSE,
                           outdir = "data/temp"){
    if(inherits(df, "character")) df<- read.csv(df)


    # 0. function input checks
    required_names <- c("layer", "level", "class", "id", "metric", "value",
                        "plot_id")
    if(!inherits(df, "data.frame")) stop("input must be a data.frame")
    if(!all(required_names %in% names(df)))
        stop(paste0("input must contain columns with all of the following names: ",
                    paste0(required_names, collapse = ", ")))

    # 1. Beta calculation
    message("calculating beta")
    beta <- df %>%
        dplyr::group_by(level, plot_id, metric) %>%
        dplyr::summarise(min  = min(value),
                  max  = max(value),
                  beta = (max-min)/max) %>%
        dplyr::mutate(rank = min_rank(desc(beta)))

    # 2. Correlation matrices (for tiebreaks)
    my_stretch <- function(z){
        z %>%
            corrr::stretch() %>%
            dplyr::group_by(x) %>%
            dplyr::summarise(mean = abs(mean(r, na.rm = TRUE)))
    }
    message("calculating correlations")
    correlations <- df %>%
        dplyr::group_by(level, plot_id) %>%
        dplyr::select(id, class, metric, value) %>%
        tidyr::nest() %>%
        dplyr::mutate(pivoted = map(data, pivot_wider, names_from=metric, values_from=value),
               pivoted = map(pivoted, select, -id, -class),
               corr    = map(pivoted, correlate)) %>%
        dplyr::mutate(corr_up  = map(corr, shave),
               corr_up2 = map(corr_up, my_stretch))

    # 2.1 make the tiebreaks from here
    message("starting tiebreaks")
    my_comb_rank <- function(y){
        y %>%
            dplyr::arrange(dplyr::desc(beta), mean) %>%
            tibble::rowid_to_column("rank_no_ties") %>%
            dplyr::relocate(rank_no_ties, .after = last_col())
    }

    out <-
        beta %>%
        dplyr::group_by(level, plot_id) %>%
        tidyr::nest(.key = "beta") %>%
        dplyr::left_join(correlations) %>%
        dplyr::mutate(beta2 = purrr::map2(beta, corr_up2, left_join, by = c(metric = "x")),
               beta3 = purrr::map(beta2, my_comb_rank))

    # 3. output formatting
    message("formatting output")
    if(!df_with_intermediate_steps){
        out <-
            out %>%
            dplyr::select(level, plot_id, beta3) %>%
            dplyr::rename(beta = beta3)
    }

    if(!correlation) correlations <- NULL

    # save to csv
    to_save<- out %>%
        tidyr::unnest(beta)
    write.csv(to_save, file = file.path(outdir, "metrics_ranked.csv"))

    return(list(ranked_data = out,
                correlations = correlations))
}

make_metric_maps<- function(landscape,# classified landscape, with NO NA's
                            aoi, # ideally with administrative subdivisions, NOT IMPLEMENTED
                            metrics, # which metrics to plot NOT IMPLEMENTED
                            ranks, # path to ranked metrics csv
                            gadm,
                            tempdir = "data/temp", # where should temp data be saved
                            plotdir,
                            ...){ # NOT IMPLEMENTED: extra arguments for writeRaster
    if(inherits(landscape, "character")){
        landscape <- terra::rast(landscape)
    }

    if(!missing(ranks) && inherits(ranks, "character")) ranks<- read.csv(ranks)

    aoi<- terra::project(terra::vect(aoi),
                         terra::crs(landscape))
    landscape <- terra::crop(landscape,
                             aoi,
                             filename = file.path(tempdir, "landscape_crop.tif"),
                             overwrite = TRUE)
    # get only forest classes
    landscape <- project_to_m(landscape, tempdir = tempdir)
    landscape<- select_forest_from_glc_lcc(landscape, tempdir = tempdir, binary = FALSE)
    aoi<- terra::project(aoi, terra::crs(landscape))

    #get administrative boundaries if needed, up to level 1, i.e., country and state boundaries
    if(missing(gadm)){
        my_adm_bound <- aoi |> sf::st_as_sf() |> st_where_is(tempdir = tempdir)
        adm_bound <- geodata::gadm(my_adm_bound, path = tempdir, level = 2)
        adm_bound <- adm_bound |> terra::project(terra::crs(aoi))
    }


    # projection can introduce NAs in the borders!
    # fixed: do projection before classification of forests
    # landscape<- terra::subst(landscape, as.numeric(NA), -9999,
    #                          raw = TRUE,
    #                          filename = file.path(tempdir, "landscape_no_NA.tif"),
    #                          overwrite = TRUE)

    to_mask_for_plot<- terra::ifel(landscape == -9999, NA,1,
                                   filename = file.path(tempdir, "landscape_to_mask.tif"),
                                   overwrite = TRUE)

    types <-
        landscapemetrics::list_lsm() %>%
        dplyr::distinct(metric, .keep_all = TRUE)

    # landscape <- "input raster with classes"

    # reprex
    # for testing
    # landscape <- terra::unwrap(landscapemetrics::landscape)
    # landscape <- terra::rast(path$raster) %>% subst(NA, -9999)
    # create a spatial raster with patch id and LSM values --------------------

    patches <- landscapemetrics::get_patches(landscape, to_disk = TRUE)

    # this reduction is performed in memory!
    # patches <- setNames(Reduce(cover, patches$layer_1), "id")

    # this reduction is performed out of memory! confirmed equal ouptut
    patches <- patches |> unlist() |> terra::sprc() |>
        terra::merge(filename = file.path(tempdir, "patches.tif"),
              overwrite = TRUE,
              names = "id")

    # level should be user specified (e.g based on the beta selection method Andres)

    # ifranks are provided, we grab the top n
    wh_metrics<- NULL
    if(!missing(ranks) && inherits(ranks, "data.frame")){ # ranks was converted from chr to df at beginning...
        wh_metrics <- ranks |>
            dplyr::filter(level == "patch") |>
            dplyr::top_n(n = -5, wt = rank_no_ties) |>
            dplyr::pull(metric)
    }
    #sp
    ms <- landscapemetrics::spatialize_lsm(landscape,
                                           level = "patch",
                                           metric = wh_metrics,
                                           to_disk = TRUE)

    # for the hessen example, to_disk = TRUE throws error:
    # Error: [writeValues] too few values for writing: 5312856 < 11153310
    # fix: change NA values to integer, eg., 99.
    # TODO: check results vs no 99 class later

    # multiband raster
    ms <- terra::rast(ms$layer_1)
    ms2 <- c(patches, landscape, ms)

    # generate a pdf of the maps -----------------------------------------------
    names <- ms2 %>%
        names() %>%
        tibble::tibble() %>%
        setNames("function_name") %>%
        dplyr::left_join(types) %>%
        dplyr::mutate(plot_name = ifelse(is.na(name), function_name, name))

    # specify path to save PDF to
    destination <- file.path(plotdir, "plots.pdf")
    message("creating plots in pdf")

    pdf(file=destination)

    #specify to save plots in 2x2 grid
    par(mfrow = c(2,2))

    #save plots to PDF
    my_coltab<- data.frame(coltab(path$lc_raster |> terra::rast()))
    my_coltab<- rbind(my_coltab, c(value = -9999, my_coltab[1,2:5]))

    for (i in 1:nrow(names)) {
        tmp_name <- names$plot_name[i] |> gsub(" ", "_", x = _)
        tmp <- ms2[[i]] |>
            terra::mask(to_mask_for_plot,
                        filename = file.path(plotdir, tmp_name) |>
                            fs::path_ext_set("tif"),
                        overwrite = TRUE)
        if(tmp_name == "category") coltab(tmp) <- my_coltab
        plot(tmp,main = tmp_name)
        lines(aoi, col = "red", alpha = 0.6)
        lines(adm_bound, lwd = 2.5, alpha = 0.5)
    }
    par(mfrow = c(1,1))
    #turn off PDF plotting
    dev.off()

    # adding info from type to join later with m
    # add
    # ms3 <-
    #     ms2 %>%
    #     tibble::as_tibble() %>%
    #     tidyr::pivot_longer(c(-id, -clumps), names_to = "function_name", values_to = "value_s") %>%
    #     dplyr::left_join(types)
    #
    # ms4 <-
    #     ms3 %>%
    #     dplyr::group_by(id, function_name, level, metric) %>%
    #     dplyr::summarise(value_s = max(value_s, na.rm = TRUE))
    #
    # # # create tabular version of LSM  ------------------------------------------
    # m <- calculate_lsm(landscape, level = "patch")
    #
    # # joining to check if the outputs from calculate_lsm()
    # # and spatialize_lsm() are identical
    # foo<-
    #     m %>%
    #     dplyr::left_join(ms4) %>%
    #     dplyr::mutate(check = dplyr::near(value, value_s, tol = .00001))
    # foo %>% dplyr::pull(check) %>% table()
    # # correct up to specified tolerance
    return()
}

project_to_m<- function(x, tempdir = "data/temp"){
    # x is a terraraster object
    if(landscapemetrics::check_landscape(x)$units != "m"){
        wh_epsg <- utm_zone(x, proj4string = TRUE)
        # wh_epsg <- "epsg:6933" fallback option
        terra::project(x, wh_epsg, method = "near",
                filename = file.path(tempdir, "landscape_crop_project_m.tif"),
                overwrite = TRUE)
    } else{ # dont change anything if already in m
        x
    }

    # https://nsidc.org/data/user-resources/help-center/guide-ease-grids
}

select_forest_from_glc_lcc <- function(x, tempdir = "data/temp", binary = FALSE){
    # set binary to TRUE if you just want a 1,0 (forest/no forest) mask , e.g., for disturbance
    # here we assume the land cover class leayer as in
    # https://land.copernicus.eu/global/sites/cgls.vito.be/files/products/CGLOPS1_PUM_LC100m-V3_I3.4.pdf
    # table 4 page 28-29
    forest_map_code <- c("111" = "closed_forest_evergreen_needle_leaf",
                         "112" = "closed_forest_evergreen_broad_leaf",
                         "113" = "closed_forest_decidious_needle_leaf",
                         "114" = "closed_forest_decidious_broad_leaf",
                         "115" = "closed_forest_mixed",
                         "116" = "closed_forest_unknown",
                         "121" = "open_forest_evergreen_needle_leaf",
                         "122" = "open_forest_evergreen_broad_leaf",
                         "123" = "open_forest_decidious_needle_leaf",
                         "124" = "open_forest_decidious_broad_leaf",
                         "125" = "open_forest_mixed",
                         "126" = "open_forest_unknown",
                         "-9999" = "no_forest") |>
        list(. = _) |> with(data.frame(ID = as.numeric(names(.)), category = .))
    if(binary){
        out<- terra::ifel(x %in% forest_map_code$ID, 1, 0,
                   filename = file.path(tempdir, "forest_mask.tif"),
                   overwrite = TRUE)
    } else{
        out<- terra::ifel(x %in% forest_map_code$ID, x, -9999,
                   filename = file.path(tempdir, "forest_mask_tmp.tif"),
                   overwrite = TRUE) |>
            terra::categories(value = forest_map_code) |>
            terra::writeRaster("forest_mask.tif", overwrite = TRUE)
    }

    return(out)

}
