#' 2016 census hydro layer
#' @param refresh if true, refresh the data
#' @param cache_path directory for caching the data
#' @return an sf object with the 2016 hydro layer
#' @export
get_2016_census_hydro_layer <- function(cache_path=getOption("custom_data_path"),refresh=FALSE){
  path=file.path(cache_path,"census_2016_hydro_layer_path")
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
#' @param cache_path directory for caching the data
#' @return an sf object with the metro vancouver land use data
#' @export
get_metro_vancouver_land_use_data <- function(cache_path=getOption("custom_data_path"),refresh=FALSE){
  land_use_data <- simpleCache(get_shapefile("http://www.metrovancouver.org/data/Data/LandUse/Landuse2011.zip"),
                               "metro_van_land_use_data",
                               path=cache_path,
                               refresh = refresh) %>%
    sf::st_sf()
}


#' Geographic attribute data with DA representative points (2016 census version)
#' @param refresh if true, refresh the data
#' @param cache_path directory for caching the data
#' @return an sf object with the representative point data
#' @export
get_statcan_geographic_attribute_data <- function(cache_path=getOption("custom_data_path"),refresh=FALSE) {
  path <- file.path(cache_path,"geosuite_data")
  if (!dir.exists(path)) {
    tmp=tempfile(fileext = ".zip")
    download.file("http://www12.statcan.gc.ca/census-recensement/2016/geo/ref/gaf/files-fichiers/2016_92-151_XBB_csv.zip",tmp)
    fs <- unzip(tmp,exdir=path)
  }
  fn <-dir(path)[grepl("\\.csv$",dir(path))]
  readr::read_csv( file.path(path,fn), col_types = readr::cols(.default = "c")) %>%
    rlang::set_names(gsub("/.+$","",names(.))) %>%
    dplyr::mutate_at(c("DBpop2016", "DBtdwell2016", "DBurdwell2016"),as.integer) %>%
    dplyr::mutate_at(c("DBarea2016","DArplamx", "DArplamy", "DArplat", "DArplong"),as.numeric) %>%
    sf::st_as_sf(coords = c("DArplong", "DArplat"), crs = 4326, agr = "constant")
}

