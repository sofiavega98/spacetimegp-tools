% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{format_for_stan_ICM_normal}
\alias{format_for_stan_ICM_normal}
\title{Format Data for Normal Intrinsic Coregionalization Model (ICM) Stan Model}
\usage{
format_for_stan_ICM_normal(simulated_data, trt_counties)
}
\arguments{
\item{simulated_data}{Data frame containing the simulated data with columns:
- county_id: Spatial location identifier
- time: Time point identifier
- count: Response variable}

\item{trt_counties}{Vector of county IDs that received treatment}
}
\value{
A list containing formatted data for Stan
}
\description{
This function prepares data for fitting a Normal Intrinsic Coregionalization Model using Stan.
The ICM model uses latent factors to capture spatial dependencies, with a Normal likelihood.
}
\keyword{internal}
