# aoi clip

clip_aoi <- function(x, aoi, ...){
    terra::crop(x, aoi, ...)
}
