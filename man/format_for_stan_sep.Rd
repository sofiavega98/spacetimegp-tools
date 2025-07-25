% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{format_for_stan_sep}
\alias{format_for_stan_sep}
\title{Format Data for Separable Gaussian Process Stan Model}
\usage{
format_for_stan_sep(simulated_data, trt_counties)
}
\arguments{
\item{simulated_data}{Data frame containing the simulated data with columns:
- county_id: Spatial location identifier
- time: Time point identifier
- lon: Longitude coordinate
- lat: Latitude coordinate
- count: Response variable}

\item{trt_counties}{Vector of county IDs that received treatment}
}
\value{
A list containing formatted data for Stan
}
\description{
This function prepares data for fitting a separable Gaussian Process model using Stan.
It handles the creation of spatial distance matrices and indices for the separable kernel structure.
}
\keyword{internal}
