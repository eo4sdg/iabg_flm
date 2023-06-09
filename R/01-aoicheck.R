# aoi


aoi_too_big <-
    function(aoi, max_area = units(1000, ha)){
    area <- sum(st_area(aoi))
    area > max_area
}


if (aoi_too_big(aoi)){}
