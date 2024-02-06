library(landscapemetrics)
library(terra)

# specify tempdir because next functions only write to
# tempdir
terraOptions(tempdir = "data/temp")



landscape <- "input raster with classes"

# reprex
landscape <- terra::unwrap(landscape)


patches <- get_patches(landscape, to_disk = TRUE)

# this reduction is performed in memory!
patches <- Reduce(cover, patches$layer_1)

# user specified level or based on the beta selection method
ms <- spatialize_lsm(landscape, level = "patch", to_disk = TRUE)


m <- calculate_lsm(landscape)

m |> filter(level == "patch")


