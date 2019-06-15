#' Simple key-value cache function accepting closures
#' @param object closure with return expression to be cached
#' @param key cache key
#' @param path path to cache the data
#' @param refresh bool option to force refresh of cache, default FALSE
#' @export
simpleCache <- function(object,key,path=getOption("custom_data_path"),refresh=FALSE){
  cache_path=file.path(path,key)
  if(!refresh & file.exists(cache_path)) {
    readRDS(cache_path)
  } else {
    data=object
    saveRDS(data,file=cache_path)
    data
  }
}

#' transfer sf object to gzipped geojson file on aws s3
#' @param data sf object
#' @param bucket s3 bucket name
#' @param s3_path s3 path in bucket
#' @export
sf_to_s3_gzip <- function(data,s3_bucket,s3_path) {
  tmp=tempfile(fileext = ".geojson")
  tmp.gz=paste0(tmp,".gz")
  sf::st_write(data,tmp,delete_dsn=file.exists(tmp))
  R.utils::gzip(tmp,overwrite=TRUE)
  unlink(tmp)
  result = aws.s3::put_object(tmp.gz,s3_path,s3_bucket,acl="public-read",
                              headers=list("Content-Type"='application/json', "Content-Encoding"='gzip'))
  unlink(tmp.gz)
  result
}
