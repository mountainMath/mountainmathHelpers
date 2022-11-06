#' Get vector tile data
#'
#' @description
#' expects mapzen API key being available as MAPBOX_PUBLIC_TOKEN environment variable
#'
#' @param bbox bounding box for which to get vector tile data
#' @param tileset_id Mapbox tileset ID, default is "mapbox.mapbox-streets-v8"
#' @param zoom optional, zoom level for tiles
#' @param max_tiles Maximum number of tiles to query, overrides desired zoom level.
#' @param bbox bounding box for which to get vector tile data
#' @param refresh results are cached for current session, optionally refresh cache
#' @return a list of layers with vector tile data
#' @export
get_mapbox_vector_tiles <- function(bbox,
                                    tileset_id = "mapbox.mapbox-streets-v8",
                                    zoom = NULL,
                                    max_tiles = 9,
                                    refresh=FALSE){
  orig_crs <- sf::st_crs(bbox)
  if (is.na(orig_crs$epsg) | orig_crs$epsg != 4326) {
    bbox <- sf::st_as_sfc(bbox) %>%
      sf::st_transform(4326) %>%
      sf::st_bbox()
  }
  c<-slippymath::bbox_tile_query(bbox) %>%
    dplyr::filter(.data$total_tiles<=max_tiles)
  if (!is.null(zoom)) {
    z <- pmin(zoom,max(c$zoom))
    if (z!=zoom) message(paste0("Adjusting zoom level to ",z))
    zoom <- z
  } else {
    zoom=max(c$zoom)
  }

  digest_id <- c %>% dplyr::filter(.data$zoom==!!zoom) %>% as.list() %>% digest::digest()
  digest <- paste0("mapbox_vector_tiles_",digest_id,".Rda")

  path=file.path(tempdir(),digest)
  if (refresh | !file.exists(path)) {
    width <- bbox$xmax-bbox$xmin
    height <- bbox$ymax-bbox$ymin
    location <- sf::st_as_sfc(bbox) %>% sf::st_buffer(pmax(width,height)/5)
    vector_tiles<-mapboxapi::get_vector_tiles(tileset_id, location, zoom, access_token = NULL)
    saveRDS(vector_tiles,path)
  } else {
    vector_tiles <- readRDS(path)
  }
  vector_tiles
}



StatMapboxVectorTiles <- ggplot2::ggproto("StatMapboxVectorTiles", ggplot2::Stat,
                                    compute_panel = function(data, scales, type,
                                                             zoom = NULL, max_tiles = 5,
                                                             transform=function(d)d) {
                                      if (inherits(data,"data.frame")) {
                                        data <- sf::st_as_sf(data)
                                      }
                                      if (inherits(data,"sf")) {
                                        data <- sf::st_bbox(data)
                                      }

                                      stopifnot("geom_vector_tiles needs sf or bbox object as data"=("bbox" %in% class(data)))
                                      if ("roads" %in% type) type="roads" else type=type[1]
                                      bbox <- sf::st_bbox(data)
                                      vector_tiles <- get_mapbox_vector_tiles(bbox=bbox,zoom=zoom,max_tiles = max_tiles)
                                      tile_data <- vector_tiles[[type]]
                                      orig_crs <- sf::st_crs(bbox)
                                      if (is.na(orig_crs$epsg) | orig_crs$epsg != 4326) {
                                        tile_data <- tile_data %>%
                                          transform %>%
                                          sf::st_transform(orig_crs)
                                      } else {
                                        tile_data <- tile_data %>% transform
                                      }
                                      tile_data
                                    }
)

#' Adds a vector tile layer for the map
#' @param type type of vector layer
#' @param zoom optional, zoom level for tiles
#' @param max_tiles Maximum number of tiles to query, overrides desired zoom level.
#' @param transform transform function to apply to the vector tile data
#' @param ... extra arguments
#' @return a geom_sf object
#' @export
geom_mapbox_vector_tiles <- function(...,
                              type=c("water", "road", "building", "poi_label", "place_label",
                                     "structure", "transit_stop_label", "landuse"),
                              zoom=NULL, max_tiles = 5,
                              transform=function(d)d){
  ggplot2::geom_sf(stat = StatMapboxVectorTiles,
                   ...,
                   type=type,
                   zoom=zoom,
                   max_tiles = max_tiles,
                   transform=transform)
}




#' Adds a roads layer for the map
#' @param color color for the roads
#' @param size size of roads
#' @param zoom optional, zoom level for tiles
#' @param max_tiles Maximum number of tiles to query, overrides desired zoom level.
#' @param transform transform function to apply to the vector tile data, by default filter out ferry lines
#' @param ... extra arguments
#' @return a geom_sf object with the layer
#' @export
geom_mapbox_roads <- function(..., color = "black", size = 0.1,
                              zoom = NULL, max_tiles = 5,
                              transform = function(d)d[d$class %in% c("street","primary","secondary","tertiary","motorway"),]){
  geom_mapbox_vector_tiles(...,type="road", color = color, size = size,
                           zoom=zoom, max_tiles = max_tiles,
                           transform = function(d)d$lines %>% transform)
}

#' Adds a water layer for the map
#' @param fill fill for the water featuers, default is `lightblue`
#' @param size size of outline
#' @param zoom optional, zoom level for tiles
#' @param max_tiles Maximum number of tiles to query, overrides desired zoom level.
#' @param transform transform function to apply to the vector tile data
#' @param ... extra arguments
#' @return a geom_sf object with the layer
#' @export
geom_mapbox_water <- function(..., fill = "lightblue", size = 0,
                              zoom = NULL, max_tiles = 5,
                              transform=function(d)d) {
  geom_mapbox_vector_tiles(..., type="water",
                    fill = fill, size = size,
                    zoom = zoom, max_tiles = max_tiles,
                    transform = function(d)d %>%
                      transform %>%
                      dplyr::filter(sf::st_geometry_type(.) %in% c("MULTIPOLYGON","POLYGON")))
}
