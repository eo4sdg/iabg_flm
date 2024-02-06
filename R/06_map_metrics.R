library(landscapemetrics)
library(terra)



# specify tempdir because next functions only write to
# tempdir

tempdir <- "data/temp"
terraOptions(tempdir = tempdir)
fs::dir_create(tempdir)

# nee the extra information later for joining
types <-
    list_lsm() |>
    distinct(metric, .keep_all = TRUE)


landscape <- "input raster with classes"

# reprex
landscape <- terra::unwrap(landscape)



# create a spatial raster with patch id and LSM values --------------------


patches <- get_patches(landscape, to_disk = TRUE)

# this reduction is performed in memory!
patches <- setNames(Reduce(cover, patches$layer_1), "id")

# level should be user specified (e.g based on the beta selection method Andres)
ms <- spatialize_lsm(landscape, level = "patch", to_disk = TRUE)

# multiband raster
ms <- rast(ms$layer_1)
ms2 <- c(patches, ms)


# adding info from type to join later with m
ms3 <-
    ms2 |>
    as_tibble() |>
    pivot_longer(-id, names_to = "function_name", values_to = "value_s") |>
    left_join(types)



# create tabular version of LSM  ------------------------------------------


m <- calculate_lsm(landscape)



# joining to check if the outputs from calculate_lsm()
# and spatialize_lsm() are identical
m |>
    filter(level == "patch") |>
    left_join( ms3) |>
    mutate(check = value == value_s) |>
    pull(check)


# something is wrong. investigation needed.
