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


#' Frequent transit layer
#' @export
get_yvr_frequent_transit_network <- function(refresh=FALSE){
  cache_path=file.path(getOption("custom_data_path"),"frequent_transit_areas.shp")
  if (refresh||!file.exists(cache_path)){
  # grab landuse data
  #land_use_data <- get_metro_vancouver_land_use_data()
  bbox=metro_van_bbox(clipped=FALSE)

  # grab transit stops
  stops <- simpleCache(get_transit_stops(list(bbox=bbox,per_page=1000),get_all=TRUE),"metro_yvr_transit_stops") %>%
    sf::st_transform(4326) #%>%
    #dplyr::filter(st_intersects(.,land_use_data %>% sf::st_union()) %>% as.logical %>% tidyr::replace_na(FALSE))

  # transit frequency
  mon_params <- list(bbox=bbox,
                     per_page=10000,
                     origin_departure_between="06:00:00,21:00:00",
                     date="2018-11-19")
  schedule_stops_monday <- simpleCache(transitland::get_transit_schedule_stops(mon_params,get_all = TRUE),"yvr_schedule_11-19") %>%
    dplyr::filter(origin_onestop_id %in% stops$onestop_id )
  sat_params <- list(bbox=bbox,
                     per_page=10000,
                     origin_departure_between="07:00:00,21:00:00",
                     date="2018-11-17")
  schedule_stops_saturday <- simpleCache(transitland::get_transit_schedule_stops(sat_params,get_all = TRUE),"yvr_schedule_11-17") %>%
    dplyr::filter(origin_onestop_id %in% stops$onestop_id )
  sun_params <- list(bbox=bbox,
                     per_page=10000,
                     origin_departure_between="08:00:00,21:00:00",
                     date="2018-11-18")
  schedule_stops_sunday <- simpleCache(transitland::get_transit_schedule_stops(sun_params,get_all = TRUE),"yvr_schedule_11-18") %>%
    dplyr::filter(origin_onestop_id %in% stops$onestop_id )

  get_schedule_stop_data <- function(){

    add_time <- function(data,time){
      data %>%
        dplyr::bind_rows(tibble(origin_onestop_id=dplyr::unique(data$origin_onestop_id),origin_departure_time=time))
    }

    summarize_stop_data <- function(data,hours,start_time,end_time){
      data %>%
        add_time(start_time) %>%
        add_time(end_time) %>%
        dplyr::group_by(origin_onestop_id) %>%
        dplyr::arrange(origin_departure_time) %>%
        dplyr::mutate(time=paste0("2018-11-19 ",origin_departure_time)) %>%
        dplyr::mutate(previous_time=lag(time)) %>%
        dplyr::mutate(wait_time=difftime(time,previous_time,units="mins") %>% as.numeric) %>%
        dplyr::summarise(wait_times=list(wait_time),
                  departure_times=list(origin_departure_time),
                  departures_per_hour=(length(origin_departure_time)-2)/hours)
      #summarize(count=n()/hours, max_wait=max(wait_time,na.rm=TRUE),name=first(name))
    }

    # count departures and weight by times and days
    # assume weekdays are all the same, only get Monday data
    dplyr::bind_rows(
      schedule_stops_monday %>% summarize_stop_data(15,"06:00:00","21:00:00") %>% mutate(weekday="Monday"),
      schedule_stops_saturday %>% summarize_stop_data(14,"07:00:00","21:00:00") %>% mutate(weekday="Saturday"),
      schedule_stops_sunday %>% summarize_stop_data(13,"08:00:00","21:00:00") %>% mutate(weekday="Sunday")
    )
  }

  schedule_stop_data <- simpleCache(get_schedule_stop_data(),"yvr_schedule_stops_computed",refresh = FALSE) %>%
    dplyr::mutate(count=lengths(wait_times)-2) %>%
    dplyr::mutate(max_wait=purrr::map(wait_times,max,na.rm=TRUE) %>% unlist) %>%
    dplyr::mutate(wait_80=purrr::map(wait_times,function(x)quantile(x,0.8,na.rm=TRUE)) %>% unlist) %>%
    dplyr::mutate(wait_95=purrr::map(wait_times,function(x)quantile(x,0.95,na.rm=TRUE)) %>% unlist)


  frequency_counts <- schedule_stop_data %>%
    dplyr::group_by(origin_onestop_id) %>%
    dplyr::mutate(weight=dplyr::case_when(weekday=="Monday"~5,TRUE~1)) %>%
    dplyr::summarize(count=sum(count*weight)/7,max_wait=max(max_wait),wait_80=max(wait_80),wait_95=max(wait_95)) %>%
    dplyr::mutate(frequent=wait_95<=20,wait_80<=16,max_wait<=30)


  bline_routes="099|095|096"
  skytrain_routes=" Line"
  frequent_stops <- stops %>%
    dplyr::right_join(filter(frequency_counts,frequent) %>%
                        dplyr::select(max_wait,wait_80,count,origin_onestop_id),
                      by=c("onestop_id"="origin_onestop_id")) %>%
    dplyr::mutate(routes=lapply(routes_serving_stop,function(x){jsonlite::fromJSON(x)$route_name %>% unique()})) %>%
    dplyr::mutate(routes_string = lapply(routes,function(x){paste(x,collapse=",")})) %>%
    dplyr::mutate(bline=grepl(bline_routes,routes_string)) %>%
    dplyr::mutate(skytrain=grepl(skytrain_routes,routes_string)) %>%
    dplyr::mutate(radius=dplyr::case_when(skytrain ~ 800, bline ~ 600, TRUE ~ 400))

  # compute frequent transit netowrk area
  frequent_transit_areas <- frequent_stops %>%
    sf::st_transform(26910) %>%
    sf::st_buffer(.,frequent_stops$radius) %>%
    sf::st_transform(4326) %>%
    sf::st_sf() %>%
    dplyr::select(name,max_wait,wait_80,count,bline,skytrain,radius,geometry)
  sf::write_sf(frequent_transit_areas,cache_path,delete_layer = TRUE)
  } else {
    frequent_transit_areas <- sf::read_sf(cache_path)
  }
  frequent_transit_areas
}
