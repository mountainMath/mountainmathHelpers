# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`mountainmathHelpers` is an R package of helper functions used across MountainMath analysis code (blog posts, reports). It is installed from GitHub (`remotes::install_github("mountainMath/mountainmathHelpers")`), not CRAN, and other MountainMath code depends on the exported function names and argument defaults — treat exported signatures as a public API.

## Commands

Run from the package root (it is also an RStudio project with `PackageUseDevtools: Yes`).

```sh
Rscript -e 'devtools::document()'    # regenerate NAMESPACE and man/*.Rd from roxygen comments
Rscript -e 'devtools::load_all()'    # load the package for interactive testing
Rscript -e 'devtools::install()'     # install locally
Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning")'  # what CI runs
Rscript -e 'devtools::build_readme()'    # README.Rmd -> README.md (needs nextzen key + cancensus API key, hits the network)
Rscript -e 'pkgdown::build_site()'       # rebuilds docs/ (served via GitHub Pages)
```

- There is no test suite (`tests/` does not exist) and no vignettes. `R CMD check` is the only automated verification; CI (`.github/workflows/R-CMD-check.yaml`) runs it on macOS and Windows and fails on warnings.
- `NAMESPACE` and `man/` are roxygen-generated — never edit them by hand. After adding or changing an exported function or its `#'` docs, run `devtools::document()` and commit the result.
- `README.md` is generated from `README.Rmd`; edit the `.Rmd`.
- `docs/` is committed pkgdown output (last built in 2022), not source.

## Configuration the functions expect

These are read at call time from options/environment, not from package config:

| Setting | Used by |
| --- | --- |
| `options(nextzen_API_key=)` or env var `nextzen_API_key` | `get_vector_tiles()` and the `geom_vector_tiles()` family (`geom_roads`, `geom_water`, `geom_transit`) |
| env var `MAPBOX_PUBLIC_TOKEN` (read by `mapboxapi`) | `get_mapbox_vector_tiles()` and the `geom_mapbox_*` family |
| `options(custom_data_path=)` | default `path`/`cache_path` for `simpleCache()` and all `get_*` data downloaders |
| AWS credentials in the environment (as `aws.s3` expects) | `file_to_s3()`, `file_to_s3_gzip()`, `sf_to_s3_gzip()` |
| env vars `R2_ACCOUNT_ID`, `R2_ACCESS_KEY_ID`, `R2_SECRET_ACCESS_KEY` | `parquet_to_r2()`, `remove_parquet_from_r2()` (Cloudflare R2 via `arrow::S3FileSystem`, not `aws.s3` — `aws.s3` cannot address R2 correctly when `AWS_DEFAULT_REGION` is set. Deletes go through a hand-signed request (`r2_object_request()`) in `r2_delete_object()` because arrow's `DeleteFile` leaves empty directory marker objects behind) |
| env vars `R2_ACCOUNT_ID`, `R2_ACCESS_KEY_ID`, `R2_SECRET_ACCESS_KEY` | `r2_duckdb_connection()`, `r2_parquet_tbl()` (lazy `dbplyr` tables on a session-cached in-memory DuckDB connection with `httpfs` and an R2 secret), `r2_parquet_polars()` (polars `LazyFrame` via `pl$scan_parquet()` with `storage_options`), `r2_parquet_arrow()` (arrow `Dataset` via `arrow::open_dataset()` on `r2_filesystem()`). `arrow`, `duckdb`, `DBI`, `dbplyr`, `polars` are in `Suggests` and checked with `requireNamespace()`; `polars` is not on CRAN and comes from the r-multiverse repository listed under `Additional_repositories`. Directories of parquet files are often themselves named `*.parquet`, so `r2_parquet_glob()` (DuckDB and polars, arrow resolves this itself) sends a signed `HEAD` request for such paths to tell a file from a directory. The DuckDB secret sets `REGION 'auto'` explicitly, otherwise DuckDB takes the region from `AWS_DEFAULT_REGION` and R2 rejects it |
| env vars `R2_ACCOUNT_ID`, `R2_API_TOKEN` | `create_r2_bucket()` (Cloudflare REST API via `httr`; bucket settings like public access are not available through R2's S3-compatible API). With `custom_domain` it looks up the domain's zone via `/zones`, so the token also needs Zone Read on that zone, and the zone has to be active in the same account. `parquet_to_r2()` and `remove_parquet_from_r2()` also use this token (if set) to purge changed files from the Cloudflare cache of the bucket's custom domains (`r2_purge_cache()`, needs Zone Cache Purge); a failed purge only warns since the objects have already changed |

## Architecture

All code is in `R/`, grouped by theme. The pieces that span files:

**Vector tile map layers — two parallel implementations.** `R/vector_tile_helpers.R` (Nextzen via `rmapzen`) and `R/mapbox_vetor_tiles.R` (Mapbox via `mapboxapi`) have the same three-level structure, and a change to one usually needs mirroring in the other:

1. A fetcher (`get_vector_tiles` / `get_mapbox_vector_tiles`) that reprojects the bbox to EPSG:4326, fetches tiles, and caches the result as an RDS file in `tempdir()` keyed by a digest of the tile coordinates (session-lifetime cache; `refresh=TRUE` bypasses it).
2. An unexported ggproto `Stat` (`StatVectorTiles` / `StatMapboxVectorTiles`) whose `compute_panel` ignores the layer's data values and only takes its bounding box, fetches tiles for it, picks the layer named by `type`, applies the user's `transform` function, and reprojects back to the data's CRS.
3. `geom_*` wrappers that call `ggplot2::geom_sf(stat = ...)` with styling defaults and a default `transform` (e.g. dropping ferries from roads, keeping only polygons for water).

   Layer names and attribute schemas differ between providers (`roads`/`kind` for Nextzen vs `road`/`class` with a `$lines` element for Mapbox), so `transform` functions are not interchangeable. The wrappers expose a `width` argument but pass it to ggplot2 as `linewidth`.

**Caching.** There are two mechanisms:

- `simpleCache(object, key, path, refresh)` in `R/miscellaneous.R` is the general key→RDS cache. It relies on R's lazy evaluation: the `object` expression is only evaluated on a cache miss, so callers pass the expensive call inline (`simpleCache(get_shapefile(url), "key")`) and must not assign it to a variable first. With no `path` and no `custom_data_path` option it falls back to `tempdir()`.
- The StatCan downloaders in `R/land_use_helpers.R` (`get_2016_census_hydro_layer`, `get_2016_census_fsa_geos`, `get_2016_census_fsa_data`) instead go through the unexported `cached_unzip()`, which uses "does the unzip directory exist" under `cache_path` as the cache check. `refresh=TRUE` downloads again and replaces that directory. They stop with an error when `cache_path` is NULL (no `tempdir()` fallback, unlike `simpleCache()`). These only exist because `cancensus` has no equivalent: its `get_statcan_geographies()` and `get_statcan_wds_data()` are 2021 only and there is no hydro layer. StatCan data that `cancensus` does cover (e.g. the geographic attribute file via `cancensus::get_statcan_geographic_attributes()`) belongs there, not here.

`get_metro_vancouver_land_use_data()` dispatches on a `vintage` string; each vintage has its own source URL and cache key, so adding a data release means adding a branch rather than changing an existing one (old vintages are kept for reproducibility of earlier analyses).

**GeoParquet.** `R/geoparquet.R` writes GeoParquet 2.0 (native parquet `GEOMETRY` type with the crs in the column type and a bounding box per row group) via DuckDB `COPY ... GEOPARQUET_VERSION 'V2'`, with rows sorted by `ST_Hilbert` (after optional `sort_by` attribute columns) so the row group bounding boxes are selective. `filter_spatial()` works on any lazy DuckDB table (e.g. `r2_parquet_tbl()`) and always emits a `geom && envelope` condition before `ST_Intersects`: as of DuckDB 1.5.6 only `&&`/`ST_Intersects_Extent` prune row groups via the geometry statistics, a plain `ST_Intersects` downloads all geometry. It also filters on a `bbox` struct column if present (written with `bbox_column=TRUE`, for arrow/polars readers that can't use native geometry statistics). `collect_sf()` turns collected GEOMETRY columns (WKB raw lists) into sf, taking the crs from the column type via `DESCRIBE`.

**Logo.** `add_mm_logo()` in `R/theme_mm.R` returns a theme that replaces `plot.caption` with an S3 subclass of `element_text` (`element_mm_logo`, the same old-style element mechanism `ggtext` uses, works with ggplot2 3.x and 4.x). Its `element_grob` method draws the caption text plus the logo at the bottom left of the caption row, and a `heightDetails` method keeps that row at least as high as the logo. ggplot2 refuses to merge a plain `element_text` onto a subclass, so `add_mm_logo()` has to be the last caption-related theme call.

**Dependencies.** Everything is called with explicit `pkg::fn()` qualification; the only imports into the namespace are `%>%`, `.data`, `download.file` and `unzip` (declared at the bottom of `R/miscellaneous.R`, which also registers `.` as a global variable). New package dependencies must be added to `Imports` in `DESCRIPTION` (base packages like `grid`, `stats` and `utils` included), or to `Suggests` with a `requireNamespace()` check in the function when only a single optional function needs them (`png` for `add_mm_logo()`, the R2 query packages).

## Conventions

- Two-space indentation, `=` commonly used for assignment inside function bodies alongside `<-`, compact spacing (`function(d)d`). Match the surrounding file rather than reformatting.
- Every exported function has a roxygen block with `@param` for each argument, `@return`, and `@export`.
- `geocode()` calls the BC Geocoder API one address at a time and skips rows that already have `X` filled, so partially geocoded data frames can be passed back in to resume.
