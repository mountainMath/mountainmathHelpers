% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/land_use_helpers.R
\name{get_2016_census_fsa_data}
\alias{get_2016_census_fsa_data}
\title{2016 census FSA data}
\usage{
get_2016_census_fsa_data(
  cache_path = getOption("custom_data_path"),
  refresh = FALSE
)
}
\arguments{
\item{cache_path}{directory for caching the data}

\item{refresh}{if true, refresh the data}
}
\value{
an tibble with the 2016 FSA data
}
\description{
2016 census FSA data
}
