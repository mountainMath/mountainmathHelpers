#' Get vector tile data, expects option variable nextzen_API_key to be set
#' @param bbox bounding box for which to get vector tile data
#' @param width optional, width in pixels for the display image
#' @param height optional, width in height for the display image
#' @param bbox bounding box for which to get vector tile data
#' @param refresh results are cached for current session, optionally refresh cache
#' @return a list of layers with vector tile data
#' @export
get_vector_tiles <- function(bbox,width=NULL,height=NULL,refresh=FALSE){
  mz_box=rmapzen::mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  tile_coords <- mz_box %>% rmapzen::as.mz_tile_coordinates(width=width,height=height)
  digest <- paste0("vector_tiles_",digest::digest(tile_coords %>% jsonlite::toJSON()  %>% as.character()),".Rda")
  path=file.path(tempdir(),digest)
  if (refresh | !file.exists(path)) {
    rmapzen::mz_set_tile_host_nextzen(getOption("nextzen_API_key"))
    vector_tiles <- rmapzen::mz_vector_tiles(tile_coords)
    saveRDS(vector_tiles,path)
  } else {
    vector_tiles <- readRDS(path)
  }
  vector_tiles
}

#' Adds a water lay for the map
#' @param bbox bounding box for the layer
#' @param fill fill color
#' @param color color for the outline
#' @param size size of outline
#' @param tile_size_px tile size in pixels, may increase resolution of tile data
#' @param transform transform function to apply to the vector tile data
#' @return a geom_sf object with the layer
#' @export
water_layer <- function(bbox,fill="lightblue",color=NA,size=1,tile_size_px=NULL,
                        transform=function(d)d){
  vector_tiles <- mountainmathHelpers::get_vector_tiles(bbox,tile_size_px,tile_size_px)
  water <- rmapzen::as_sf(vector_tiles$water) %>% transform()
  geom_sf(data=water,fill=fill,color=color,size=size)
}

#' Adds a roads lay for the map
#' @param bbox bounding box for the layer
#' @param color color for the outline
#' @param size size of outline
#' @param tile_size_px tile size in pixels, may increase resolution of tile data
#' @param transform transform function to apply to the vector tile data
#' @return a geom_sf object with the layer
#' @export
roads_layer <- function(bbox,color="black",size=0.1,tile_size_px=NULL,
                        transform=function(d)d){
  vector_tiles <- mountainmathHelpers::get_vector_tiles(bbox,tile_size_px,tile_size_px)
  roads <- rmapzen::as_sf(vector_tiles$roads) %>% transform()
  geom_sf(data=roads,color=color,size=size)
}

#' convenience function to cut off map at bounding box
#' @param bbox bounding box
#' @export
#' @return a coord_sf object cutting the map view to the bounding box
coord_bbox <- function(bbox){
  coord_sf(datum = NA,xlim=c(bbox$xmin,bbox$xmax),ylim=c(bbox$ymin,bbox$ymax))
}


#' Vector tiles for Metro Vancouver
#' @param clipped should part of Nroth Shore mountains be clipped?
#' @export
#' @return a bounding box
metro_van_bbox <- function(clipped=TRUE){
  if (clipped) {
    bbox=sf::st_bbox(c(sf::st_point(c(-123.43189,   49.00193)), sf::st_point(c(-122.40864,   49.4806169)) ),crs=4326)
  } else {
    bbox=sf::st_bbox(c(sf::st_point(c(-123.43189,   49.00193)), sf::st_point(c(-122.40864,   49.57428)) ),crs=4326)
  }
  bbox
}

#' Vector tiles for City of Vancouver
#' @param clipped should part of Nroth Shore mountains be clipped?
#' @export
#' @return a bounding box
cov_bbox <- function(include_ubc=TRUE){
  if (include_ubc) {
    bbox=sf::st_bbox(c(sf::st_point(c(-123.280,   49.194)), sf::st_point(c(-123.013,   49.320)) ),crs=4326)
  } else {
    bbox=sf::st_bbox(c(sf::st_point(c(-123.229,   49.194)), sf::st_point(c(-123.013,   49.320)) ),crs=4326)
  }
  bbox
}

#' Vector tiles for Metro Vancouver
#' cached vector tiles for Metro Vancouver
#' @export
metro_van_vector_tiles <- function(){
  simpleCache(get_vector_tiles(metro_van_bbox(FALSE)),"metro_van_vector_tiles")
}

#' Vector tiles for City of Vancouver + UBC
#' cached vector tiles for the City of Vancouver
#' @export
cov_vector_tiles <- function(){
  simpleCache(get_vector_tiles(cov_bbox(TRUE)),"cov_vector_tiles")
}



#' @importFrom dplyr %>%
#' @importFrom rlang .data
NULL

