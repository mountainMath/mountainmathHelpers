#' Use BC Geocoder to geocode addresses. This is slow, goes through address list one by one
#' @param data data frame with rows to be geocoded
#' @param address_field column in data frame that contains the address string
#' @param localities optional array of locality names to restrict search to
#' @return data frame with `X`, `Y` columns for longitude and latitude, as well as
#' matching `score`, `matchPrecision` and cleaned `usedAddressString`
#'
#' @export
geocode <- function(data,address_field="addressString",localities=NULL) {
  if (nrow(data)==0) return(data)
  # api_key <- getOption("bc_geocoder_api_key") Not needed apparently
  base_url="https://geocoder.api.gov.bc.ca/addresses.json"
  matchPrecision <- 'SITE, UNIT, CIVIC_NUMBER, INTERSECTION, BLOCK'
  new_fields <- c("X","Y","matchPrecision","score","usedAddressString","faults","fullAddress")
  match_fields <- intersect(names(data),new_fields)
  if (length(match_fields)>0) {
    warning(paste0("Will overwrite fieds ",match_fields %>% paste0(collapse = ", ")))
  }
  missing_fields <- setdiff(new_fields,names(data))
  for (field in missing_fields) data[,field]=NA

  d <- data %>%
    dplyr::filter(is.na(.data$X)) %>%
    dplyr::select(dplyr::all_of(address_field)) %>%
    unique

  for (i in 1:nrow(data)) {
    if ((!("X" %in% names(data))) || is.na(data[i,"X"])) {
      address_string=data[[address_field]][i]
      if (!is.null(localities) && length(localities)==1 && localities[1]=="Vancouver")
        address_string=paste0(sub(",$","",sub(" #\\d+.*,",",",sub(" Vancouver.*$","",address_string))),", Vancouver, BC")
      query=list(addressString=address_string,
                 matchPrecision=matchPrecision,
                 provinceCode="BC")
      if (!is.null(localities) && length(localities)>0) query["localities"]=localities
      response<-httr::GET(base_url,query=query)
      if (response$status_code==200) {
        # suppressMessages(suppressWarnings(r <- readr::read_csv(response$content)))
        features <- httr::content(response)$features
        r <- features |>
          purrr::map_dfr(\(f)as.data.frame(f$properties) |>
                           dplyr::mutate(X=f$geometry$coordinates[[1]],
                                  Y=f$geometry$coordinates[[2]])) |>
          tibble::as_tibble()
        if (nrow(r)>0){
          r <- r[1,]
        data$X[i]=r$X
        data$Y[i]=r$Y
        data$score[i]=r$score
        data$matchPrecision[i]=r$matchPrecision
        data$usedAddressString[i]=address_string
        data$fullAddress[i]=r$fullAddress
        data$faults[i]=paste0(r$faults.element," ", r$faults.fault)
        }
      }
    }
    if (i %% 100 ==0 ) print(paste0("Done with ",i,"/",nrow(data)))
  }
  data
}


