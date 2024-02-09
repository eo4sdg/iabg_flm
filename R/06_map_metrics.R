library(landscapemetrics)
library(terra)
library(tidyverse)

# specify tempdir because next functions only write to
# tempdir

make_metric_maps<- function(landscape,# classified landscape dir, with NO NA's
                            metrics, # which metrics to plot NOT IMPLEMENTED
                            aoi, # ideally with administrative subdivisions, NOT IMPLEMENTED
                            tempdir = "data/temp", # where should temp data be saved
                            plotdir,
                            ...){ # NOT IMPLEMENTED: extra arguments for writeRaster


    types <-
        landscapemetrics::list_lsm() |>
        dplyr::distinct(metric, .keep_all = TRUE)

    # landscape <- "input raster with classes"

    # reprex
    # for testing
    # landscape <- terra::unwrap(landscapemetrics::landscape)
    # landscape <- terra::rast(path$raster) |> subst(NA, -9999)
    # create a spatial raster with patch id and LSM values --------------------
    # remove na
    landscape<- terra::subst(landscape, NA, -9999)
    patches <- landscapemetrics::get_patches(landscape, to_disk = TRUE)

    # this reduction is performed in memory!
    patches <- setNames(Reduce(cover, patches$layer_1), "id")

    # level should be user specified (e.g based on the beta selection method Andres)
    ms <- landscapemetrics::spatialize_lsm(landscape, level = "patch", to_disk = TRUE)

    # for the hessen example, to_disk = TRUE throws error:
    # Error: [writeValues] too few values for writing: 5312856 < 11153310
    # fix: change NA values to integer, eg., 99.
    # TODO: check results vs no 99 class later

    # multiband raster
    ms <- terra::rast(ms$layer_1)
    ms2 <- c(patches, landscape, ms)

    # generate a pdf of the maps -----------------------------------------------
    names <- ms2 |>
        names() |>
        tibble::tibble() |>
        setNames("function_name") |>
        dplyr::left_join(types) |>
        dplyr::mutate(plot_name = ifelse(is.na(name), function_name, name))

    #specify path to save PDF to
    destination <- file.path(plotdir, "plots.pdf")

    #open PDF
    pdf(file=destination)

    #specify to save plots in 2x2 grid
    par(mfrow = c(2,2))

    #save plots to PDF
    for (i in 1:length(names)) {
        plot(ms2[[i]],main = names$plot_name[i])
    }
    par(mfrow = c(1,1))
    #turn off PDF plotting
    dev.off()

    # adding info from type to join later with m
    # add
    # ms3 <-
    #     ms2 |>
    #     tibble::as_tibble() |>
    #     tidyr::pivot_longer(c(-id, -clumps), names_to = "function_name", values_to = "value_s") |>
    #     dplyr::left_join(types)
    #
    # ms4 <-
    #     ms3 |>
    #     dplyr::group_by(id, function_name, level, metric) |>
    #     dplyr::summarise(value_s = max(value_s, na.rm = TRUE))
    #
    # # create tabular version of LSM  ------------------------------------------
    # m <- calculate_lsm(landscape, level = "patch")
    #
    # # joining to check if the outputs from calculate_lsm()
    # # and spatialize_lsm() are identical
    # foo<-
    #     m |>
    #     dplyr::left_join(ms4) |>
    #     dplyr::mutate(check = dplyr::near(value, value_s, tol = .00001))
    # foo |> dplyr::pull(check) |> table()
    # correct up to specified tolerance
    return()
}
landscape <- terra::unwrap(landscapemetrics::landscape)
make_metric_maps(landscape = landscape,
                 plotdir = "out_dir")

