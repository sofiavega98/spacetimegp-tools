#' Simulate Spatiotemporal Data with Various Kernel Structures
#'
#' This function generates simulated spatiotemporal data using different kernel structures
#' (nonseparable, separable, ICM, or Gneiting) and distributions (Poisson or Normal).
#' The data is generated on a grid of spatial locations over multiple time points.
#'
#' @param kernel Character. Type of kernel to use:
#'   - "nonsep": Non-separable spatiotemporal kernel
#'   - "sep": Separable spatiotemporal kernel
#'   - "ICM": Intrinsic Coregionalization Model
#'   - "gneiting": Gneiting's non-separable covariance function
#' @param seed Integer. Random seed for reproducibility
#' @param n_counties Integer. Number of spatial locations (default: 49)
#' @param n_timepoints Integer. Number of time points (default: 15)
#' @param distribution Character. Type of distribution:
#'   - "poisson": Poisson distribution with log link
#'   - "normal": Normal distribution with identity link
#' @param spatial_lengthscale Numeric. Lengthscale parameter for spatial correlation (default: 0.3)
#' @param temporal_lengthscale Numeric. Lengthscale parameter for temporal correlation (default: 0.9)
#' @param J Integer. Number of latent factors for ICM kernel (default: 5)
#' 
#' @return A data frame containing:
#'   - kernel: Type of kernel used
#'   - county_id: Spatial location identifier
#'   - time: Time point identifier
#'   - lon: Longitude coordinate
#'   - lat: Latitude coordinate
#'   - gp_sample: Generated Gaussian process sample
#'   - mean: Mean of the distribution
#'   - count: Simulated response variable
#'
#' @examples
#' # Simulate Poisson data with separable kernel
#' data <- simulate_data(kernel = "sep", seed = 1, distribution = "poisson")
#' 
#' # Simulate Normal data with nonseparable kernel
#' data <- simulate_data(kernel = "nonsep", seed = 1, distribution = "normal")
#'
#' @export
simulate_data <- function(kernel, seed, n_counties = 49, n_timepoints = 15, distribution = "poisson",
                         spatial_lengthscale = 0.3, temporal_lengthscale = 0.9, J = 5) {
  
  # Check inputs
  if (!kernel %in% c("nonsep", "sep", "ICM", "gneiting")) {
    stop("Invalid kernel type. Choose from 'nonsep', 'sep', 'ICM', or 'gneiting'.")
  }
  
  if (!distribution %in% c("poisson", "normal")) {
    stop("Invalid distribution. Choose from 'poisson' or 'normal'.")
  }
  
  # Step 1: Create spatial grid
  # Generate a 7x7 grid of locations and subset to desired number of counties
  county_grid <- expand.grid(lon = seq(0, 1, length.out = 7), lat = seq(0, 1, length.out = 7))
  county_grid <- county_grid[1:n_counties, ]
  
  # Step 2: Calculate distance matrices
  # Spatial distance matrix between all pairs of locations
  dist_matrix <- as.matrix(dist(county_grid))
  
  # Create normalized timepoints and their distance matrix
  timepoints <- seq(0, 1, length.out = n_timepoints)
  temporal_dist_matrix <- as.matrix(dist(timepoints))
  
  # Step 3: Initialize and compute covariance matrix based on kernel type
  if (kernel == "nonsep") {
    # Non-separable kernel parameters
    beta <- 1      # Interaction parameter
    sigma <- 1     # Overall variance
    alpha <- 1     # Smoothness parameter
    a <- spatial_lengthscale    # Spatial lengthscale
    b <- temporal_lengthscale   # Temporal lengthscale
    
    # Initialize covariance matrix
    K <- matrix(0, nrow = n_counties * n_timepoints, ncol = n_counties * n_timepoints)
    
    # Compute covariance for each pair of space-time points
    for (i in seq_len(nrow(K))) {
      for (j in seq_len(ncol(K))) {
        # Extract spatial and temporal indices
        county_i <- (i - 1) %/% n_timepoints + 1
        time_i <- (i - 1) %% n_timepoints + 1
        county_j <- (j - 1) %/% n_timepoints + 1
        time_j <- (j - 1) %% n_timepoints + 1
        
        # Calculate spatial and temporal distances
        h <- dist_matrix[county_i, county_j]
        u <- temporal_dist_matrix[time_i, time_j]
        
        # Non-separable kernel formula:
        # K(s,t) = σ² * exp(-u/b) * exp(-exp(-u/b) * (h/a)²)
        K[i, j] <- sigma^2 * exp(-u / b) * exp(-exp(-u / b) * (h / a)^2)
      }
    }
    
  } else if (kernel == "sep") {
    # Separable kernel parameters
    sigma <- 1
    
    # Initialize covariance matrix
    K <- matrix(0, nrow = n_counties * n_timepoints, ncol = n_counties * n_timepoints)
    
    # Compute covariance for each pair of space-time points
    for (i in seq_len(nrow(K))) {
      for (j in seq_len(ncol(K))) {
        # Extract spatial and temporal indices
        county_i <- (i - 1) %/% n_timepoints + 1
        time_i <- (i - 1) %% n_timepoints + 1
        county_j <- (j - 1) %/% n_timepoints + 1
        time_j <- (j - 1) %% n_timepoints + 1
        
        # Separable kernel formula:
        # K(s,t) = σ² * exp(-0.5(h/ℓₛ)²) * exp(-0.5(u/ℓₜ)²)
        K[i, j] <- sigma^2 * exp(-0.5 * (dist_matrix[county_i, county_j] / spatial_lengthscale)^2) * 
          exp(-0.5 * (temporal_dist_matrix[time_i, time_j] / temporal_lengthscale)^2)
      }
    }
    
  } else if (kernel == "ICM") {
    # ICM kernel parameters
    sigma <- 0.4              # Overall variance
    
    # Generate random coefficients for latent processes
    set.seed(1)
    beta_matrix <- matrix(rnorm(n_counties * J), nrow = n_counties, ncol = J)
    
    # Compute spatial covariance matrix using latent factors
    K_unit <- beta_matrix %*% t(beta_matrix)
    
    # Initialize full covariance matrix
    K <- matrix(0, nrow = n_counties * n_timepoints, ncol = n_counties * n_timepoints)
    
    # Compute covariance for each pair of space-time points
    for (i in seq_len(nrow(K))) {
      for (j in seq_len(ncol(K))) {
        county_i <- (i - 1) %/% n_timepoints + 1
        time_i <- (i - 1) %% n_timepoints + 1
        county_j <- (j - 1) %/% n_timepoints + 1
        time_j <- (j - 1) %% n_timepoints + 1
        
        # ICM kernel formula:
        # K(s,t) = σ² * K_unit(s,s') * exp(-0.5(u/ℓₜ)²)
        K[i, j] <- sigma^2 * K_unit[county_i, county_j] * 
          exp(-0.5 * (temporal_dist_matrix[time_i, time_j] / temporal_lengthscale)^2)
      }
    }
    
  } else if (kernel == "gneiting") {
    # Gneiting kernel parameters
    sigma <- 1
    a <- temporal_lengthscale
    c <- spatial_lengthscale
    alpha <- 1
    beta <- 1
    gamma <- 1
    d <- 2
    
    # Initialize covariance matrix
    K <- matrix(0, nrow = n_counties * n_timepoints, ncol = n_counties * n_timepoints)
    
    # Compute covariance for each pair of space-time points
    for (i in seq_len(nrow(K))) {
      for (j in seq_len(ncol(K))) {
        county_i <- (i - 1) %/% n_timepoints + 1
        time_i <- (i - 1) %% n_timepoints + 1
        county_j <- (j - 1) %/% n_timepoints + 1
        time_j <- (j - 1) %% n_timepoints + 1
        
        # Calculate distances
        h <- dist_matrix[county_i, county_j]
        u <- abs(timepoints[time_i] - timepoints[time_j])
        
        # Gneiting kernel formula:
        # K(s,t) = σ² * (1/ψ(u)^(βd/2)) * exp(-c*h^(2γ)/ψ(u)^(βγ))
        # where ψ(u) = (a|u|^(2α) + 1)
        psi_u <- (a * u^(2 * alpha) + 1)
        space_term <- exp(-c * h^(2 * gamma) / psi_u^(beta * gamma))
        time_term <- 1 / psi_u^(beta * d / 2)
        
        K[i, j] <- sigma^2 * time_term * space_term
      }
    }
  }
  
  # Step 4: Generate GP sample
  set.seed(1)  # Fixed seed for consistent mean across simulations
  gp_sample <- MASS::mvrnorm(1, mu = rep(0, nrow(K)), Sigma = K)
  
  # Step 5: Generate response variable based on distribution
  if (distribution == "poisson") {
    # Poisson: mean = exp(intercept + GP)
    mean <- exp(4 + gp_sample)
    set.seed(seed)  # User-specified seed for response generation
    count <- rpois(length(mean), lambda = mean)
  } else if (distribution == "normal") {
    # Normal: mean = intercept + GP, with unit variance
    mean <- 4 + gp_sample
    set.seed(seed)
    count <- rnorm(length(gp_sample), mean = mean, sd = 1)
  }
  
  # Step 6: Return formatted data frame
  return(data.frame(
    kernel = kernel,
    county_id = rep(seq_len(n_counties), each = n_timepoints),
    time = rep(seq_len(n_timepoints), times = n_counties),
    lon = rep(county_grid$lon, each = n_timepoints),
    lat = rep(county_grid$lat, each = n_timepoints),
    gp_sample = gp_sample,
    mean = mean,
    count = count
  ))
} 