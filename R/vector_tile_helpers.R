#' Get vector tile data, expects option variable nextzen_API_key to be set
#' @param bbox bounding box for which to get vector tile data
#' @export
get_vector_tiles <- function(bbox){
  rmapzen::mz_set_tile_host_nextzen(getOption("nextzen_API_key"))
  mx_box=rmapzen::mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  rmapzen::mz_vector_tiles(mx_box)
}

#' Vector tiles for Metro Vancouver
#' @param clipped should part of Nroth Shore mountains be clipped?
#' @export
metro_van_bbox <- function(clipped=TRUE){
  if (clipped) {
    bbox=sf::st_bbox(c(sf::st_point(c(-123.43189,   49.00193)), sf::st_point(c(-122.40864,   49.4806169)) ),crs=4326)
  } else {
    bbox=sf::st_bbox(c(sf::st_point(c(-123.43189,   49.00193)), sf::st_point(c(-122.40864,   49.57428)) ),crs=4326)
  }
  bbox
}

#' Vector tiles for Metro Vancouver
#' @export
metro_van_vector_tiles <- function(){
  simpleCache(get_vector_tiles(metro_van_bbox(FALSE)),"metro_van_vector_tiles")
}



#' @importFrom dplyr %>%
#' @importFrom rlang .data
NULL

