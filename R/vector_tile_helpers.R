#' Get vector tile data, expects option variable nextzen_API_key to be set
#' @param bbox bounding box for which to get vector tile data
#' @param width optional, width in pixels for the display image
#' @param height optional, width in height for the display image
#' @param bbox bounding box for which to get vector tile data
#' @param refresh results are cached for current session, optionally refresh cache
#' @param nextzen_api_key nextzen API key for vector tile data
#' @return a list of layers with vector tile data
#' @export
get_vector_tiles <- function(bbox,width=NULL,height=NULL,refresh=FALSE,
                             nextzen_api_key = getOption("nextzen_API_key")){
  mz_box=rmapzen::mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  tile_coords <- mz_box %>% rmapzen::as.mz_tile_coordinates(width=width,height=height)
  digest <- paste0("vector_tiles_",digest::digest(tile_coords %>% jsonlite::toJSON()  %>% as.character()),".Rda")
  path=file.path(tempdir(),digest)
  if (refresh | !file.exists(path)) {
    rmapzen::mz_set_tile_host_nextzen(nextzen_api_key)
    vector_tiles <- rmapzen::mz_vector_tiles(tile_coords)
    saveRDS(vector_tiles,path)
  } else {
    vector_tiles <- readRDS(path)
  }
  vector_tiles
}



StatVectorTiles <- ggplot2::ggproto("StatVectorTiles", ggplot2::Stat,
                     compute_panel = function(data, scales, type,
                                              nextzen_api_key = getOption("nextzen_API_key"),
                                              tile_size_px=NULL, transform=function(d)d) {
                       if (class(data)=="data.frame") {
                         data <- sf::st_as_sf(data)
                       }
                       if ("sf" %in% class(data)) {
                         data <- sf::st_bbox(data)
                       }
                       stopifnot("geom_roads needs sf or bbox object as data"=("bbox" %in% class(data)))
                       if ("roads" %in% type) type="roads" else type=type[1]
                       bbox <- sf::st_bbox(data)
                       vector_tiles <- get_vector_tiles(bbox,tile_size_px,tile_size_px)
                       tile_data <- rmapzen::as_sf(vector_tiles[[type]])
                       tile_data %>% transform()
                     }
)

#' Adds a vector tile layer for the map
#' @param type type of vector layer
#' @param nextzen_api_key nextzen API key for vector tile data
#' @param tile_size_px tile size in pixels, may increase resolution of tile data
#' @param transform transform function to apply to the vector tile data
#' @param ... extra arguments
#' @return a geom_sf object
#' @export
geom_vector_tiles <- function(...,
                              type=c("water", "buildings", "places", "transit", "pois",
                                     "boundaries", "roads", "earth", "landuse"),
                              nextzen_api_key = getOption("nextzen_API_key"),
                              tile_size_px=NULL,
                              transform=function(d)d){
  ggplot2::geom_sf(stat = StatVectorTiles,
          ...,
          type=type,
          nextzen_api_key = nextzen_api_key,
          tile_size_px=tile_size_px,
          transform=transform)
}

#' Adds a roads layer for the map
#' @param color color for the roads
#' @param size size of roads
#' @param nextzen_api_key nextzen API key for vector tile data
#' @param tile_size_px tile size in pixels, may increase resolution of tile data
#' @param transform transform function to apply to the vector tile data, by default filter out ferry lines
#' @param ... extra arguments
#' @return a geom_sf object with the layer
#' @export
geom_roads <- function(..., color = "black", size = 0.1,
                       nextzen_api_key = getOption("nextzen_API_key"),
                       tile_size_px = NULL,
                       transform = function(d)d[d$kind!="ferry",]){
  geom_vector_tiles(...,type="roads", color = color, size = size,
                    nextzen_api_key = nextzen_api_key,
                    tile_size_px = tile_size_px, transform = transform)
}

#' Adds a water layer for the map
#' @param fill fill for the water featuers, default is `lightblue`
#' @param size size of outline
#' @param nextzen_api_key nextzen API key for vector tile data
#' @param tile_size_px tile size in pixels, may increase resolution of tile data
#' @param transform transform function to apply to the vector tile data
#' @param ... extra arguments
#' @return a geom_sf object with the layer
#' @export
geom_water <- function(..., fill = "lightblue", size = 0,
                       nextzen_api_key = getOption("nextzen_API_key"),
                       tile_size_px = NULL,
                       transform=function(d)d) {
  geom_vector_tiles(..., type="water",
                    fill = fill, size = size,
                    nextzen_api_key = nextzen_api_key,
                    tile_size_px = tile_size_px, transform = transform)
}

#' convenience function to cut off map at bounding box
#' @param bbox bounding box
#' @export
#' @return a coord_sf object cutting the map view to the bounding box
coord_bbox <- function(bbox){
  ggplot2::coord_sf(datum = NA,xlim=c(bbox$xmin,bbox$xmax),ylim=c(bbox$ymin,bbox$ymax))
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
#' @param include_ubc include UBC area in boundng box
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
