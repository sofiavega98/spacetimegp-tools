#' Format Data for Separable Gaussian Process Stan Model
#'
#' This function prepares data for fitting a separable Gaussian Process model using Stan.
#' It handles the creation of spatial distance matrices and indices for the separable kernel structure.
#'
#' @param simulated_data Data frame containing the simulated data with columns:
#'   - county_id: Spatial location identifier
#'   - time: Time point identifier
#'   - lon: Longitude coordinate
#'   - lat: Latitude coordinate
#'   - count: Response variable
#' @param trt_counties Vector of county IDs that received treatment
#'
#' @return A list containing formatted data for Stan
#'
#' @keywords internal
format_for_stan_sep <- function(simulated_data, trt_counties) {
  # Order simulated_data by county_id
  simulated_data <- simulated_data[order(simulated_data$county_id), ]
  
  # Identify indices for treated counties and only include times after time = 7
  treated_indices <- which(simulated_data$county_id %in% trt_counties & simulated_data$time > 7)
  
  # Total number of observations
  N <- length(unique(simulated_data$time))
  D <- length(unique(simulated_data$county_id))
  
  # Covariate and target variable
  x_covariate <- unique(simulated_data$time)
  y_target_variable <- simulated_data$count
  
  # Determine control indices (non-treated counties)
  control_indices <- setdiff(seq_along(y_target_variable), treated_indices)
  
  # Compute spatial distance matrix H and its index matrix H_ind
  coords <- unique(simulated_data[, c("county_id", "lon", "lat")])
  H_matrix <- as.matrix(dist(coords[, c("lon", "lat")]))
  H <- as.vector(H_matrix)
  
  # Convert H into long vectors
  DN <- D * N
  H_ind <- matrix(0, nrow = D * N, ncol = D * N)
  
  for (i in 1:(D*N)) {
    for (j in 1:(D*N)) {
      unit_i <- ((i - 1) %/% N) + 1  # outer index is COUNTY
      unit_j <- ((j - 1) %/% N) + 1
      H_ind[i, j] <- (unit_i - 1) * D + unit_j
    }
  }
  
  list(
    N = N,
    D = D,
    n_k_f = 15,
    time_points = x_covariate,
    y = y_target_variable,
    num_treated = length(treated_indices),
    control_idx = control_indices,
    H = H,
    H_ind = H_ind,
    distance_matrix = H_matrix 
  )
}

#' Format Data for Non-separable Gaussian Process Stan Model
#'
#' This function prepares data for fitting a non-separable Gaussian Process model using Stan.
#' It handles the creation of distance matrices and indices for spatial and temporal components.
#'
#' @param simulated_data Data frame containing the simulated data with columns:
#'   - county_id: Spatial location identifier
#'   - time: Time point identifier
#'   - lon: Longitude coordinate
#'   - lat: Latitude coordinate
#'   - count: Response variable
#' @param trt_counties Vector of county IDs that received treatment
#'
#' @return A list containing formatted data for Stan
#'
#' @keywords internal
format_for_stan_nonsep <- function(simulated_data, trt_counties) {
  # Order simulated_data by county_id
  simulated_data <- simulated_data[order(simulated_data$county_id), ]
  
  # Identify indices for treated counties and only include times after time = 7
  treated_indices <- which(simulated_data$county_id %in% trt_counties & simulated_data$time > 7)
  
  # Total number of observations
  N <- length(unique(simulated_data$time))
  D <- length(unique(simulated_data$county_id))
  
  # Covariate and target variable
  x_covariate <- unique(simulated_data$time)
  y_target_variable <- simulated_data$count
  
  # Determine control indices (non-treated counties)
  control_indices <- setdiff(seq_along(y_target_variable), treated_indices)
  
  # Compute spatial distance matrix H and its index matrix H_ind
  coords <- unique(simulated_data[, c("county_id", "lon", "lat")])
  H_matrix <- as.matrix(dist(coords[, c("lon", "lat")]))
  H <- as.vector(H_matrix)
  
  # Convert H and U into long vectors
  DN <- D * N
  H_ind <- matrix(0, nrow = D * N, ncol = D * N)
  
  for (i in 1:(D*N)) {
    for (j in 1:(D*N)) {
      unit_i <- ((i - 1) %/% N) + 1  # outer index is COUNTY
      unit_j <- ((j - 1) %/% N) + 1
      H_ind[i, j] <- (unit_i - 1) * D + unit_j
    }
  }
  
  # Compute temporal distance matrix U and its index matrix U_ind
  temporal_coords <- unique(simulated_data$time)
  U_matrix <- as.matrix(dist(temporal_coords))
  U <- as.vector(U_matrix)
  
  U_ind <- matrix(0, nrow = D * N, ncol = D * N)
  
  for (i in 1:(D*N)) {
    for (j in 1:(D*N)) {
      time_i <- ((i - 1) %% N) + 1
      time_j <- ((j - 1) %% N) + 1
      U_ind[i, j] <- (time_i - 1) * N + time_j
    }
  }
  
  list(
    N = N,
    D = D,
    n_k_f = 15,
    time_points = x_covariate,
    y = y_target_variable,
    num_treated = length(treated_indices),
    control_idx = control_indices,
    H = H,
    U = U,
    H_ind = H_ind,
    U_ind = U_ind
  )
}

#' Format Data for Intrinsic Coregionalization Model (ICM) Stan Model
#'
#' This function prepares data for fitting an Intrinsic Coregionalization Model using Stan.
#' The ICM model uses latent factors to capture spatial dependencies.
#'
#' @param simulated_data Data frame containing the simulated data with columns:
#'   - county_id: Spatial location identifier
#'   - time: Time point identifier
#'   - count: Response variable
#' @param trt_counties Vector of county IDs that received treatment
#'
#' @return A list containing formatted data for Stan
#'
#' @keywords internal
format_for_stan_ICM <- function(simulated_data, trt_counties) {
  # Order simulated_data by county_id
  simulated_data <- simulated_data[order(simulated_data$county_id), ]
  
  # Identify indices for treated counties and only include times after time = 7
  treated_indices <- which(simulated_data$county_id %in% trt_counties & simulated_data$time > 7)
  
  # Total number of observations
  N <- length(unique(simulated_data$time))
  D <- length(unique(simulated_data$county_id))
  
  # Covariate and target variable
  x_covariate <- unique(simulated_data$time)
  y_target_variable <- simulated_data$count
  
  # Determine control indices (non-treated counties)
  control_indices <- setdiff(seq_along(y_target_variable), treated_indices)
  
  list(
    N = N,
    D = D,
    n_k_f = 15,
    x = x_covariate,
    y = y_target_variable,
    num_treated = length(treated_indices),
    control_idx = control_indices
  )
}

#' Format Data for Normal Intrinsic Coregionalization Model (ICM) Stan Model
#'
#' This function prepares data for fitting a Normal Intrinsic Coregionalization Model using Stan.
#' The ICM model uses latent factors to capture spatial dependencies, with a Normal likelihood.
#'
#' @param simulated_data Data frame containing the simulated data with columns:
#'   - county_id: Spatial location identifier
#'   - time: Time point identifier
#'   - count: Response variable
#' @param trt_counties Vector of county IDs that received treatment
#'
#' @return A list containing formatted data for Stan
#'
#' @keywords internal
format_for_stan_ICM_normal <- function(simulated_data, trt_counties) {
  # Order simulated_data by county_id
  simulated_data <- simulated_data[order(simulated_data$county_id), ]
  
  # Identify indices for treated counties and only include times after time = 7
  treated_indices <- which(simulated_data$county_id %in% trt_counties & simulated_data$time > 7)
  
  # Total number of observations
  N <- length(unique(simulated_data$time))
  D <- length(unique(simulated_data$county_id))
  
  # Covariate and target variable
  x_covariate <- unique(simulated_data$time)
  y_target_variable <- simulated_data$count
  
  # Determine control indices (non-treated counties)
  control_indices <- setdiff(seq_along(y_target_variable), treated_indices)
  
  list(
    N = N,
    D = D,
    n_k_f = 15,
    x = x_covariate,
    y = matrix(y_target_variable, nrow=N, ncol = D, byrow = T),
    num_treated = length(treated_indices),
    control_idx = control_indices
  )
} 