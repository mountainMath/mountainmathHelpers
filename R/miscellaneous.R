#' Simple key-value cache function accepting closures
#' @param object closure with return expression to be cached
#' @param key cache key
#' @param path path where to cache the data
#' @param refresh bool option to force refresh of cache, default FALSE
#' @return object, (potentially cached version)
#' @export
simpleCache <- function(object,key,path=getOption("custom_data_path"),refresh=FALSE){
  if (is.null(path)) {
    path <- tempdir()
    message("Data is cached for the duration of the current session.\nFor longer term caching, please specify a cache path or set the 'custom_data_path' option.")
  }
  cache_path=file.path(path,key)
  if(!refresh & file.exists(cache_path)) {
    readRDS(cache_path)
  } else {
    data=object
    saveRDS(data,file=cache_path)
    data
  }
}


#' Cut values with pretty labels
#' @param values a numeric vector which is to be converted to a factor by cutting.
#' @param breaks a numeric vector of two or more unique cut points
#' @param format a function specifying how to format values in the labels
#' @param binding a string binding two values in the label
#' @param spacing a spacing string between elements in the label
#' @param under_text a string taking the space of the lower value and binding if the lower value is `-Inf`
#' @param over_text a string taking the space of the upper value and binding if the upper value is `-Inf`
#' @param ... additional arguments passed to `cut`
#' @return object, (potentially cached version)
#' @export
pretty_cut <- function(values,breaks,format=function(d)d,
                       binding="to",spacing=" ",
                       under_text="<",over_text=">",
                       ...){
  labels <- seq(1,length(breaks)-1) %>%
    lapply(function(i){
      a=breaks[i]
      b=breaks[i+1]
      if (is.infinite(a) & a<0) {
        text=paste0(under_text,spacing,format(b))
      } else if (is.infinite(b) & b>0) {
        text=paste0(over_text,spacing,format(a))
      } else {
        ta=paste0(format(a),spacing,binding,spacing,format(b))
      }
    }) %>%
    unlist()
  cut(values,breaks=breaks,labels=labels,...)
}

#' Convert a bounding box object into a polygon object
#' @param bbox bounding box object
#' @return a polygon with the same oulines of the bounding box
#' @export
bbox_to_polygon <- function(bbox){
  data.frame(X=c(bbox$xmin,x=bbox$xmin,bbox$xmax,bbox$xmax),
                 Y=c(bbox$ymin,bbox$ymax,bbox$ymin,bbox$ymax)) %>%
    dplyr::as_tibble() %>%
    sf::st_as_sf(coords=c("X","Y"),crs=sf::st_crs(bbox),agr="constant") %>%
    sf::st_union() %>%
    sf::st_convex_hull()
}

#' transfer sf object to gzipped geojson file on aws s3
#' @param data sf object
#' @param s3_bucket s3 bucket name
#' @param s3_path s3 path in bucket
#' @return upload result (boolean)
#' @export
sf_to_s3_gzip <- function(data,s3_bucket,s3_path) {
  tmp <- tempfile(fileext = ".geojson")
  sf::st_write(data,tmp,delete_dsn=file.exists(tmp))
  result <- file_to_s3_gzip(tmp,s3_bucket,s3_path)
  unlink(tmp)
  result
}

#' transfer file to gzipped file on aws s3
#' @param path path to local file
#' @param s3_bucket s3 bucket name
#' @param s3_path s3 path in bucket, if it is a path component ending with a slash (`/`)
#' the basename of the input path will be appended
#' @param content_type mime type of the data, default is inferred from file extension
#' @return upload result (boolean)
#' @export
file_to_s3_gzip <- function(path,s3_bucket,s3_path,content_type=NULL) {
  if (is.null(content_type)) {
    if (endsWith(path,"json")) {
      content_type='application/json'
    } else if (endsWith(path,"csv")) {
      content_type='application/csv'
    } else {
      content_type='application/text'
    }
  }
  tmp.gz=paste0(path,".gz")
  R.utils::gzip(path,destname=tmp.gz,overwrite=TRUE,remove=FALSE)
  if (endsWith(s3_path,"/")) {
    s3_path=paste0(s3_path,basename(tmp.gz))
  }
  result <- aws.s3::put_object(tmp.gz,s3_path,
                               s3_bucket,
                               multipart = TRUE,
                               acl="public-read",
                               headers=list("Content-Type"=content_type,
                                            "Content-Encoding"='gzip'))
  unlink(tmp.gz)
  result
}


#' transfer file to aws s3
#' @param path path to local file
#' @param s3_bucket s3 bucket name
#' @param s3_path s3 path in bucket, if it is a path component ending with a slash (`/`)
#' the basename of the input path will be appended
#' @param content_type mime type of the data, default is inferred from file extension
#' @return upload result (boolean)
#' @export
file_to_s3 <- function(path,s3_bucket,s3_path,content_type=NULL) {
  if (is.null(content_type)) {
    if (endsWith(path,"json")) {
      content_type='application/json'
    } else if (endsWith(path,"csv")) {
      content_type='application/csv'
    } else if (endsWith(path,"zip")) {
      content_type='application/zip'
    } else {
      content_type='application/text'
    }
  }
  if (endsWith(s3_path,"/")) {
    s3_path=paste0(s3_path,basename(path))
  }
  result <- aws.s3::put_object(path,s3_path,
                               s3_bucket,
                               multipart = TRUE,
                               acl="public-read",
                               headers=list("Content-Type"=content_type))
  result
}


#' download zipped shapefile and read shapefile.
#' @param path URL string to zipped shape file
#' @param file_mask optional grep string in case there are several shape files in the package
#' @return an sf object with the data from the shape file
#' @export
get_shapefile <- function(path,file_mask=NA){
  tmp <- tempfile()
  utils::download.file(path,tmp)
  tmpdir <- tempdir()
  fs<-utils::unzip(tmp,exdir=tmpdir)
  file_names <- fs[grepl("\\.shp$",fs)]
  if (is.na(file_mask)) {
    file_name=file_names[1]
  } else {
    file_name <- file_names[grepl(file_mask,file_names)]
    if (length(file_names)>1)file_names=file_names[1]
  }
  message_string <- paste0("Reading ",file_name,".")
  if (length(file_names)>0) {
    message_string <- paste0(message_string,"\nIgnoring ",
                             paste0(setdiff(file_names,file_name),collapse = ", "),".")
  }
  message(message_string)
  data <- sf::read_sf(file_name)
  unlink(tmp)
  unlink(fs)
  #unlink(tmpdir,recursive = TRUE)
  data
}

# Suppress warnings for missing bindings for '.' in R CMD check.
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom utils unzip
#' @importFrom utils download.file
NULL

