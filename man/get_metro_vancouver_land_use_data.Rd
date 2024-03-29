% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/land_use_helpers.R
\name{get_metro_vancouver_land_use_data}
\alias{get_metro_vancouver_land_use_data}
\title{Metro Vancouver land use data (2011 version)}
\usage{
get_metro_vancouver_land_use_data(
  vintage = "2016",
  cache_path = getOption("custom_data_path"),
  refresh = FALSE
)
}
\arguments{
\item{vintage}{vintage of the data, valid parameters are 2016 and 2011}

\item{cache_path}{directory for caching the data}

\item{refresh}{if true, refresh the data}
}
\value{
an sf object with the metro vancouver land use data
}
\description{
Metro Vancouver land use data (2011 version)
}
