#' Use BC Geocoder to geocode addresses. This is slow, goes through address list one by one
#' @param data data frame with rows to be geocoded
#' @param address_field column in data frame that contains the address string
#' @return data frame with `X`, `Y` columns for longitude and latitude, as well as
#' matching `score`, `matchPrecision` and cleaned `usedAddressString`
#'
#' @export
geocode <- function(data,address_field="addressString") {
  # api_key <- getOption("bc_geocoder_api_key") Not needed apparently
  base_url="https://apps.gov.bc.ca/pub/geocoder/addresses.csv"
  matchPrecision <- 'SITE, UNIT, CIVIC_NUMBER, INTERSECTION, BLOCK'
  for (i in 1:nrow(data)) {
    if (is.na(data[i,"X"])) {
      address_string=data[[address_field]][i]
      address_string=paste0(sub(",$","",sub(" #\\d+.*,",",",sub(" Vancouver.*$","",address_string))),", Vancouver, BC")
      query=list(addressString=address_string,
                 localities="Vancouver",
                 matchPrecision=matchPrecision,
                 provinceCode="BC")
      response<-httr::GET(base_url,query=query)
      if (response$status_code==200) {
        suppressMessages(suppressWarnings(r <- readr::read_csv(response$content)))
        data$X[i]=r$X
        data$Y[i]=r$Y
        data$score[i]=r$score
        data$matchPrecision[i]=r$matchPrecision
        data$usedAddressString[i]=address_string
      }
    }
    if (i %% 100 ==0 ) print(paste0("Done with ",i,"/",nrow(data)))
  }
  data
}

batch_geocode <- function(data,address_field="addressString") {
  api_key="I1S6uQP2vb4dBPdwv80to9o2HQZQM5qa"
  url='http://delivery.apps.gov.bc.ca/pub/cpf/ws/apps/geocoder/multiple.json'
  url="https://geocoder.api.gov.bc.ca/addresses.json"
  headers = list('Accept'= 'application/json')
fields = list('inputDataContentType'= 'csv',
	'resultSrid'='3005',
	'interpolation'='adaptive',
	'locationDescriptor'='any',
	'resultDataContentType'='csv',
	'media'= 'application/json')
}
