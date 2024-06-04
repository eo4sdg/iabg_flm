# functions_download_forest_mask.R

# Adjusted function to extract longitude and latitude from folder names
extract_lon_lat_from_filenames <- function(filename) {
    pattern <- "([EW])(\\d+)([NS])(\\d+)_PROBAV_LC100"
    if (grepl(pattern, filename)) {
        parts <- str_match(filename, pattern)
        lon_dir <- ifelse(parts[2] == "E", 1, -1)

        lon <- as.numeric(parts[3]) * lon_dir
        lat_dir <- ifelse(parts[4] == "N", 1, -1)
        lat <- as.numeric(parts[5]) * lat_dir

        return(c(lon, lat))
    } else {
        return(c(NA, NA)) # Return NA if no match is found
    }
}

# Function to create tile bounding box
create_tile_bbox <- function(tile_lon, tile_lat) {
    lon_min <- tile_lon
    lon_max <- tile_lon + 20
    lat_min <- tile_lat - 20
    lat_max <- tile_lat

    coords <- matrix(c(lon_min, lat_max, lon_max, lat_max, lon_max, lat_min, lon_min, lat_min, lon_min, lat_max),
                     ncol = 2, byrow = TRUE)
    tile_bbox_polygon <- sf::st_polygon(list(coords))
    tile_bbox_sfc <- sf::st_sfc(tile_bbox_polygon)

    sf::st_crs(tile_bbox_sfc) <- 4326
    return(tile_bbox_sfc)
}

path_to_glc_forest_tiles<- function(glc_folders_eodata, polygon_wkt, crs_wkt, specified_year) {

    # Change the WKT to sf polygon
    polygon_sf <- sf::st_as_sfc(polygon_wkt, crs = crs_wkt)
    bbox <- sf::st_bbox(polygon_sf)

    glc_folders_eodata_2019 <- grep(specified_year, glc_folders_eodata, value = TRUE)

    # Applying the function to each filename to extract coordinates
    tiles_info <- t(sapply(glc_folders_eodata_2019, extract_lon_lat_from_filenames))

    # Initialize vector to hold paths of intersecting tiles
    intersecting_tiles <- character()

    # Loop through tiles_info to check intersection with AOI
    for (i in seq_len(nrow(tiles_info))) {
        if (!is.na(tiles_info[i, 1]) && !is.na(tiles_info[i, 2])) { # Ensure coordinates are not NA
            tile_lon <- tiles_info[i, 1]
            tile_lat <- tiles_info[i, 2]
            tile_bbox_sfc <- create_tile_bbox(tile_lon, tile_lat)

            if (sf::st_intersects(tile_bbox_sfc, polygon_sf, sparse = FALSE)[1,1]) {
                intersecting_tiles <- c(intersecting_tiles, glc_folders_eodata_2019[i])
            }
        }
    }

    # List all files to merge them later
    list_all_files <- function(directory) {
        list.files(directory, pattern = "Type-layer_EPSG-4326", full.names = TRUE, recursive = TRUE)
    }
    glc_forest_files <- unlist(lapply(intersecting_tiles, list_all_files))

    return(glc_forest_files)

}

# Merge if AOI is in more than 1 tile, if not just pass the 1 tile
 merge_raster_from_paths <- function(paths, output_path, ...){
    rasters <- lapply(paths, terra::rast)
    if (length(rasters) > 1) {
        merged_raster <- do.call(terra::merge, rasters)
    } else {
        merged_raster <- rasters[[1]]  # If only one raster, just use it directly
    }
    terra::writeRaster(merged_raster, filename = output_path, filetype = "GTiff", overwrite = TRUE)
}
