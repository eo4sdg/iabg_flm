#' Check size of aoi
#'
#' @param aoi  sf object
#' @param max_area max area in ha
#'
#' @return
#' @export
#'
#' @examples
aoi_too_big <-
    function(aoi, max_area = 1000){
        max_area <- units(max_area, ha)
        area <- sum(st_area(aoi))
        area > max_area
}

