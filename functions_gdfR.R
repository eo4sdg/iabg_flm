# functions_gdfR.R

path_create <- function (folder, path = ".")
{
  fs::dir_create(fs::path(path, folder))
}


path_decompose <- function(x) {
  # Decompose the path into directory, filename without extension, and extension
  dir <- fs::path_dir(x)
  name <- fs::path_file(fs::path_ext_remove(x))
  ext <- fs::path_ext(x)

  # Create a data frame with the decomposed path components
  decomposed_path <- data.frame(dir = dir, name = name, ext = ext, stringsAsFactors = FALSE)

  return(decomposed_path)
}

path_compose <- function (x)
{
  out <- vector()
  for (row in 1:nrow(x)) {
    out[row] <- fs::path(x[["dir"]][row], x[["name"]][row],
                         ext = x[["ext"]][row])
  }
  return(out)
}


path_insert <- function (x, string, pos, sep = "_")
{
  x <- path_decompose(x)
  s <- stringr::str_split(x$name, pattern = sep)
  a <- unlist(purrr::map_chr(s, .insert, string = string,
                             pos = pos, sep = sep))
  x$name <- a
  path_compose(x)
}



# path_date function from gdfR
path_date <- function (x, format = "%Y%m%dT%H%M%S", convert = TRUE)
{
  out <- stringr::str_extract(x, "(?<=_)\\d{8}T\\d{6}(?=_)")
  if (convert)
    out <- as.Date(out, format = format)
  return(out)
}


# r_vi function from gdfR
calc_vegetation_index <- function (x, indices = NULL, cores = 1, writeRaster = FALSE,
                                   outfolder, filename, overwrite = FALSE, wopt = list())
{
  idxDB <- list(EVI = function(nir1, red, blue) 2.5 * (nir1 -
                                                         red)/((nir1 + 6 * red - 7.5 * blue) + 10000), NBR = function(nir1,
                                                                                                                      swir1) (nir1 - swir1)/(nir1 + swir1), NMDI = function(nir2,
                                                                                                                                                                            swir1, swir2) (nir2 - (swir1 - swir2))/(nir2 + (swir1 -
                                                                                                                                                                                                                              swir2)), SAVI = function(nir1, red) ((nir1 - red)/(nir1 +
                                                                                                                                                                                                                                                                                   red + 500)) * (10000 + 500), SIPI1 = function(nir1,
                                                                                                                                                                                                                                                                                                                                 red, blue) (nir1 - blue)/(nir1 - red), NDVI = function(nir1,
                                                                                                                                                                                                                                                                                                                                                                                        red) (nir1 - red)/(nir1 + red), CIG = function(nir1,
                                                                                                                                                                                                                                                                                                                                                                                                                                       green) (nir1/green) - 1, CVI = function(nir1, red, green) nir1 *
                  (red/(green * green)), DSWI = function(nir1, green,
                                                         swir2, red) (nir1 + green)/(swir2 + red), GLI = function(green,
                                                                                                                  red, blue) (2 * green - red - blue)/(2 * green + red +
                                                                                                                                                         blue), NDVI = function(nir1, red) (nir1 - red)/(nir1 +
                                                                                                                                                                                                           red), NDYI = function(green, blue) (green - blue)/(green +
                                                                                                                                                                                                                                                                blue), NGRDI = function(green, red) (green - red)/(green +
                                                                                                                                                                                                                                                                                                                     red), RGI = function(red, green) red/green, RDVI = function(nir,
                                                                                                                                                                                                                                                                                                                                                                                 red) (nir1 - red)/sqrt(nir1 + red))
  if (inherits(x, "character"))
    x <- terra::rast(x)
  if (missing(x)) {
    print(paste("Currently implemented indices:", paste(names(idxDB),
                                                        collapse = ", ")))
  }
  else {
    x <- stats::setNames(x, tolower(names(x)))
    if (!any(names(x) %in% gdfR::bandnames())) {
      stop("Band names are not set properly. Allowed band names are: ",
           paste(gdfR::bandnames(), collapse = ", "), call. = FALSE)
    }
    if (is.null(indices)) {
      ind <- names(idxDB)
    }
    else if (indices == "eo4sdg") {
      ind <- c("EVI", "NBR", "NDVI", "NMDI", "SAVI", "SIPI1")
    }
    else {
      ind <- toupper(indices)
    }
    if (!any(ind %in% c(names(idxDB)))) {
      stop("Indices must either be NULL to calculate all indices",
           "\nor character vector containing some of: ",
           paste(names(idxDB), collapse = ", "), call. = FALSE)
    }
    args <- purrr::map(idxDB[ind], ~names(formals(.)))
    bands_available <- purrr::map_lgl(args, ~all(. %in%
                                                   names(x)))
    if (!any(bands_available)) {
      stop("Neither of the indices can be calculated using the available bands.\n                 \nDid you named bands in x properly. Allowed band names area: ",
           paste(gdfR::bandnames(), collapse = ", "), call. = FALSE)
    }
    possible <- names(bands_available)[bands_available]
    if (length(ind) > length(possible)) {
      warning("Some bands required to calculate the selected indices were missing or not named properly.\n                    \nConsider setting names using `gdfR::bandnames()`",
              call. = FALSE)
    }
    ind <- ind[ind %in% possible]
    filenames <- list()
    if (writeRaster) {
      for (i in ind) {
        if (!missing(filename)) {
          source <- filename
        }
        else if (!missing(outfolder)) {
          source <- fs::path(outfolder, fs::path_ext_set(fs::path_file(terra::sources(x)),
                                                         "tif"))
        }
        else {
          source <- terra::sources(x)
        }
        if (stringr::str_detect(source, "10-Band")) {
          filenames[[i]] <- gdfR::replace_10band(source,
                                                 i)
        }
        else {
          filenames[[i]] <- gdfR::path_insert(source,
                                              i, 99)
        }
      }
    }
    else {
      for (i in ind) {
        filenames[[i]] <- ""
      }
    }
    message(paste("Calculating indices", paste(ind, collapse = ", ")))
    out <- list()
    for (i in ind) {
      out[[i]] <- terra::lapp(x, fun = idxDB[[i]], usenames = TRUE,
                              cores = cores, filename = filenames[[i]], overwrite = overwrite,
                              wopt = wopt)
    }
    return(out)
  }
}


## Fuction from gdfR (inside of path_insert.R)
.insert <- function(s, string, pos, sep = "_"){
  if (pos >= length(s)){

    a <- c(s, string)

  } else if (pos == 0) {

    a <- c(string, s)

  } else {

    a <- c(s[1:pos], string, s[(pos+1):length(s)])
  }

  return(paste(a, collapse = sep))
}

st_crs_equal <- function (x, y)
{
  sf::st_crs(x) == sf::st_crs(y)
}

path_exists<- function (path, check = FALSE, create = TRUE, verbose = FALSE)
{
    exists <- dir.exists(path)
    if (check) {
        exists
    }
    else {
        if (!exists) {
            if (create) {
                fs::dir_create(path)
                if (verbose)
                    return(path)
            }
            if (!create & verbose)
                print("Folder does not exist,\n                                    use path_exists(create = TRUE) to create.")
        }
        else if (verbose) {
            path
        }
    }
}

#' UTM Zone
#'
#' Find UTM Zone from spatial object or x y coordinates
#' TODO: implement a raster method
#'
#' @param x longitude or Spatial object of class sf or sp
#' @param y latitude or missing
#' @param proj4string boolean
#'
#' @return
#' @export
#'
#' @examples
#' utm_zone(6, 24)
#'
#' # if a polygon is give, takes its centroid
#' library(sf)
#' bayern <- germany.sf[germany.sf$NAME_1 == "Bayern", ]
#' utm_zone(bayern)
#'
setGeneric("utm_zone", function(x, y, proj4string=FALSE){
    standardGeneric("utm_zone")
})


#' @rdname utm_zone
#' @aliases utm_zone,numeric,numeric,logical-method
setMethod("utm_zone", signature("numeric", "numeric"),
          function(x, y, proj4string) {
              return(gfcanalysis::utm_zone(x, y, proj4string))
          }
)

#' @rdname utm_zone
#' @aliases utm_zone,numeric,numeric,logical-method
setMethod("utm_zone", signature("Spatial", "missing"),
          function(x, y, proj4string) {
              return(gfcanalysis::utm_zone(x, y, proj4string))
          }
)



#' @rdname utm_zone
#' @importFrom sf st_centroid st_coordinates
#' @aliases utm_zone,Spatial,missing,logical-method
setMethod("utm_zone", signature(x='sf', y='missing'),
          function(x, proj4string) {
              x <- sf::st_transform(x, 4236)
              centroid <- sf::st_coordinates(sf::st_centroid(x))
              return(gfcanalysis::utm_zone(centroid[1,1], centroid[1,2], proj4string))
          }
)

setMethod("utm_zone", signature(x='SpatRaster', y='missing'),
          function(x, proj4string) {
              centroid<- x |> ext() |> (function(x) {c((x[1] + x[2])/2, ((x[3] + x[4])/2))})()
              return(gfcanalysis::utm_zone(centroid[1], centroid[2], proj4string))
          }
)
