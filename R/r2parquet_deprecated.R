# Parquet, GeoParquet and Cloudflare R2 helpers moved to the r2parquet package.
# These wrappers keep existing code working and forward to r2parquet with a deprecation warning.

# check that r2parquet is installed and warn about the deprecated function
r2parquet_deprecated <- function(name){
  if (!requireNamespace("r2parquet",quietly=TRUE)) {
    stop(paste0(name," has moved to the r2parquet package, install it via ",
                "remotes::install_github(\"mountainMath/r2parquet\")."),call.=FALSE)
  }
  .Deprecated(paste0("r2parquet::",name),package="mountainmathHelpers",old=name)
}

#' Deprecated parquet, GeoParquet and Cloudflare R2 functions
#'
#' @description
#' These functions have moved to the `r2parquet` package, install it via
#' `remotes::install_github("mountainMath/r2parquet")`. The functions here forward to the
#' `r2parquet` functions of the same name and arguments, see their documentation for details.
#'
#' @param path path to local parquet file or directory, or for `parquet_tbl` URL or local path
#' @param r2_bucket R2 bucket name
#' @param r2_path path in bucket
#' @param purge_cache if `TRUE`, purge changed files from the Cloudflare cache of custom domains of the bucket
#' @param public if `TRUE`, enable public read access to the bucket via the `r2.dev` URL
#' @param custom_domain optional domain to connect to the bucket for public read access
#' @param refresh if `TRUE`, close the cached connection and open a new one
#' @param hive_partitioning if `TRUE`, partition columns get derived from `key=value` components of the file paths
#' @param con DuckDB connection
#' @param data sf object
#' @param sort_by optional vector of column names to sort by before sorting spatially
#' @param row_group_size number of rows per row group
#' @param bbox_column if `TRUE`, add a `bbox` struct column with the bounding box of each geometry
#' @param ... further arguments passed on to `r2parquet::sf_to_geoparquet`
#' @param tbl lazy DuckDB table
#' @param y sf, sfc or bbox object to filter by
#' @param geometry_column name of the geometry column
#' @param exact if `TRUE`, keep rows whose geometry intersects `y`, otherwise compare bounding boxes only
#' @return the return value of the `r2parquet` function of the same name
#' @name mountainmathHelpers-deprecated
NULL

#' @rdname mountainmathHelpers-deprecated
#' @export
create_r2_bucket <- function(r2_bucket,public=FALSE,custom_domain=NULL) {
  r2parquet_deprecated("create_r2_bucket")
  r2parquet::create_r2_bucket(r2_bucket,public=public,custom_domain=custom_domain)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
parquet_to_r2 <- function(path,r2_bucket,r2_path="",purge_cache=TRUE) {
  r2parquet_deprecated("parquet_to_r2")
  r2parquet::parquet_to_r2(path,r2_bucket,r2_path=r2_path,purge_cache=purge_cache)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
remove_parquet_from_r2 <- function(r2_bucket,r2_path,purge_cache=TRUE) {
  r2parquet_deprecated("remove_parquet_from_r2")
  r2parquet::remove_parquet_from_r2(r2_bucket,r2_path,purge_cache=purge_cache)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
r2_duckdb_connection <- function(refresh=FALSE) {
  r2parquet_deprecated("r2_duckdb_connection")
  r2parquet::r2_duckdb_connection(refresh=refresh)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
r2_parquet_tbl <- function(r2_bucket,r2_path,hive_partitioning=TRUE,con=r2parquet::r2_duckdb_connection()) {
  r2parquet_deprecated("r2_parquet_tbl")
  r2parquet::r2_parquet_tbl(r2_bucket,r2_path,hive_partitioning=hive_partitioning,con=con)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
r2_parquet_arrow <- function(r2_bucket,r2_path,hive_partitioning=TRUE) {
  r2parquet_deprecated("r2_parquet_arrow")
  r2parquet::r2_parquet_arrow(r2_bucket,r2_path,hive_partitioning=hive_partitioning)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
r2_parquet_polars <- function(r2_bucket,r2_path,hive_partitioning=TRUE) {
  r2parquet_deprecated("r2_parquet_polars")
  r2parquet::r2_parquet_polars(r2_bucket,r2_path,hive_partitioning=hive_partitioning)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
parquet_tbl <- function(path,hive_partitioning=TRUE,con=NULL) {
  r2parquet_deprecated("parquet_tbl")
  # the default connection of r2parquet is internal, so only pass con on when given
  if (is.null(con)) r2parquet::parquet_tbl(path,hive_partitioning=hive_partitioning)
  else r2parquet::parquet_tbl(path,hive_partitioning=hive_partitioning,con=con)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
sf_to_geoparquet <- function(data,path,sort_by=NULL,row_group_size=10000L,bbox_column=FALSE) {
  r2parquet_deprecated("sf_to_geoparquet")
  r2parquet::sf_to_geoparquet(data,path,sort_by=sort_by,row_group_size=row_group_size,bbox_column=bbox_column)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
sf_to_r2_geoparquet <- function(data,r2_bucket,r2_path,...) {
  r2parquet_deprecated("sf_to_r2_geoparquet")
  r2parquet::sf_to_r2_geoparquet(data,r2_bucket,r2_path,...)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
filter_spatial <- function(tbl,y,geometry_column=NULL,exact=TRUE) {
  r2parquet_deprecated("filter_spatial")
  r2parquet::filter_spatial(tbl,y,geometry_column=geometry_column,exact=exact)
}

#' @rdname mountainmathHelpers-deprecated
#' @export
collect_sf <- function(tbl,geometry_column=NULL) {
  r2parquet_deprecated("collect_sf")
  r2parquet::collect_sf(tbl,geometry_column=geometry_column)
}
