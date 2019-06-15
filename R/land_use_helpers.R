#' 2016 census hydro layer
#' @export
get_2016_census_hydro_layer <- function(refresh=FALSE){
  path=file.path(getOption("custom_data_path"),"census_2016_hydro_layer_path")
  if (!dir.exists(path)){
    tmp=tempfile()
    download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lhy_000c16a_e.zip",tmp)
    dir.create(path)
    unzip(tmp,exdir = path)
  }
  sf::read_sf(path)
}

#' Metro Vancouver land use data (2011 version)
#' @export
get_metro_vancouver_land_use_data <- function(refresh=FALSE){
  land_use_data <- simpleCache(get_shapefile("http://www.metrovancouver.org/data/Data/LandUse/Landuse2011.zip"),
                               "metro_van_land_use_data",refresh = refresh) %>%
    sf::st_sf()
}


#' Metro Vancouver DA level geography cut down by MEtro Vancouevr land use data
#' @export
get_metro_van_cut_geo_data_db <- function(refresh=FALSE){
  get_data <- function(){
    land_use_data <- get_metro_vancouver_land_use_data() %>%
      dplyr::filter(!(LU_CodeDes %in% c("Agriculture", "Harvesting and Research", "Industrial – Extractive",
                                        "Recreation, Open Space and Protected Natural Areas", "Cemetery",
                                        "Lakes, Large Rivers and Other Water", "Airport/Airstrip", "Port Metro Vancouver",
                                        "Protected Watershed","Road Right-of-Way",
                                        "Rail, Rapid Transit, Other Transportation, Utility and Communication" ))) %>%
      sf::st_union() %>%
      sf::st_sf() %>%
      sf::st_transform(4326) %>%
      lwgeom::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_cast("MULTIPOLYGON") %>%
      rmapshaper::ms_simplify(0.99) %>%
      lwgeom::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_cast("MULTIPOLYGON")

    cancensus::get_census("CA16",regions=list(CMA="59933"),level="DB",geo_format='sf') %>%
      sf::st_intersection(land_use_data)
  }
  simpleCache(get_data(),"metro_van_cut_db",refresh = refresh) %>%
    sf::st_sf()
}

#' Metro Vancouver DA level geography cut down by MEtro Vancouevr land use data
#' @export
get_metro_van_cut_geo_data_da <- function(refresh=FALSE){
  get_data <- function(){
    land_use_data <- get_metro_vancouver_land_use_data() %>%
      dplyr::filter(!(LU_CodeDes %in% c("Agriculture", "Harvesting and Research", "Industrial – Extractive",
                                        "Recreation, Open Space and Protected Natural Areas", "Cemetery",
                                        "Lakes, Large Rivers and Other Water", "Airport/Airstrip", "Port Metro Vancouver",
                                        "Protected Watershed","Road Right-of-Way",
                                        "Rail, Rapid Transit, Other Transportation, Utility and Communication" ))) %>%
      sf::st_union() %>%
      sf::st_sf() %>%
      sf::st_transform(4326) %>%
      lwgeom::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_cast("MULTIPOLYGON") %>%
      rmapshaper::ms_simplify(0.99) %>%
      lwgeom::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_cast("MULTIPOLYGON")

    cancensus::get_census("CA16",regions=list(CMA="59933"),level="DA",geo_format='sf') %>%
      sf::st_intersection(land_use_data)
  }
  simpleCache(get_data(),"metro_van_cut_da",refresh = refresh) %>%
    sf::st_sf()
}

