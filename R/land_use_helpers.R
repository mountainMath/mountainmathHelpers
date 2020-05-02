#' 2016 census hydro layer
#' @param refresh if true, refresh the data
#' @export
get_2016_census_hydro_layer <- function(refresh=FALSE){
  path=file.path(getOption("custom_data_path"),"census_2016_hydro_layer_path")
  if (!dir.exists(path)){
    tmp=tempfile()
    utils::download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lhy_000c16a_e.zip",tmp)
    dir.create(path)
    utils::unzip(tmp,exdir = path)
  }
  sf::read_sf(path)
}

#' Metro Vancouver land use data (2011 version)
#' @param refresh if true, refresh the data
#' @export
get_metro_vancouver_land_use_data <- function(refresh=FALSE){
  land_use_data <- simpleCache(get_shapefile("http://www.metrovancouver.org/data/Data/LandUse/Landuse2011.zip"),
                               "metro_van_land_use_data",refresh = refresh) %>%
    sf::st_sf()
}


