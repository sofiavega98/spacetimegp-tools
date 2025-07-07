#' Fit Spatiotemporal Gaussian Process Model
#'
#' This function fits a spatiotemporal Gaussian Process model using Stan based on the specified
#' kernel type and outcome distribution. It automatically selects the appropriate Stan model
#' and data formatting function.
#'
#' @param data Data frame containing the simulated data (output from simulate_data)
#' @param kernel Character. Type of kernel to use for modeling:
#'   - "ICM": Intrinsic Coregionalization Model
#'   - "sep": Separable spatiotemporal kernel
#'   - "nonsep": Non-separable spatiotemporal kernel
#' @param outcome_type Character. Type of outcome distribution:
#'   - "poisson": Poisson distribution with log link
#'   - "normal": Normal distribution with identity link
#' @param trt_counties Vector of county IDs that received treatment (default: c(1, 2, 3))
#' @param chains Integer. Number of MCMC chains (default: 1)
#' @param iter Integer. Number of iterations per chain (default: 1000)
#' @param warmup Integer. Number of warmup iterations (default: 500)
#' @param seed Integer. Random seed for Stan sampling (default: 300)
#' @param init_r Numeric. Initialization parameter for Stan (default: 0.1)
#'
#' @return A fitted Stan model object
#'
#' @examples
#' # Simulate data first
#' sim_data <- simulate_data(kernel = "sep", seed = 1, distribution = "poisson")
#' 
#' # Fit separable Poisson model
#' fit <- fit_model(data = sim_data, kernel = "sep", outcome_type = "poisson")
#' 
#' # Fit nonseparable Normal model
#' fit <- fit_model(data = sim_data, kernel = "nonsep", outcome_type = "normal")
#'
#' @export
fit_model <- function(data, kernel, outcome_type, trt_counties = c(1, 2, 3), 
                     chains = 1, iter = 1000, warmup = 500, seed = 300, init_r = 0.1) {
  
  # Check inputs
  if (!kernel %in% c("ICM", "sep", "nonsep")) {
    stop("Invalid kernel type. Choose from 'ICM', 'sep', or 'nonsep'.")
  }
  
  if (!outcome_type %in% c("poisson", "normal")) {
    stop("Invalid outcome type. Choose from 'poisson' or 'normal'.")
  }
  
  # Check if rstan is available
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' is required but not installed. Please install it first.")
  }
  
  # Set up Stan options
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = FALSE)
  
  # Format data based on kernel type
  if (kernel == "ICM") {
    if (outcome_type == "poisson") {
      stan_data_list <- format_for_stan_ICM(data, trt_counties)
      stan_file <- system.file("stan", "poisson_ICM.stan", package = "spacetimegp")
    } else {
      stan_data_list <- format_for_stan_ICM_normal(data, trt_counties)
      stan_file <- system.file("stan", "normal_ICM.stan", package = "spacetimegp")
    }
  } else if (kernel == "sep") {
    stan_data_list <- format_for_stan_sep(data, trt_counties)
    if (outcome_type == "poisson") {
      stan_file <- system.file("stan", "poisson_RBF.stan", package = "spacetimegp")
    } else {
      stan_file <- system.file("stan", "normal_RBF.stan", package = "spacetimegp")
    }
  } else if (kernel == "nonsep") {
    stan_data_list <- format_for_stan_nonsep(data, trt_counties)
    if (outcome_type == "poisson") {
      stan_file <- system.file("stan", "poisson_nonsep.stan", package = "spacetimegp")
    } else {
      stan_file <- system.file("stan", "normal_nonsep.stan", package = "spacetimegp")
    }
  }
  
  # Check if Stan file exists
  if (!file.exists(stan_file)) {
    stop("Stan model file not found. Please ensure the package is properly installed.")
  }
  
  # Load and compile Stan model
  cat("Compiling Stan model...\n")
  stan_model <- rstan::stan_model(stan_file, auto_write = FALSE)
  
  # Set parameters to extract based on model type
  if (kernel == "ICM" && outcome_type == "poisson") {
    pars <- c('f', 'f_samples')
  } else if (kernel == "nonsep" && outcome_type == "normal") {
    pars <- c('f_samples', 'f_draw', 'export_sigma_global',
              'export_beta', 'export_alpha', 'export_gamma',
              'export_lengthscale_time', 'export_lengthscale_space')
  } else {
    pars <- c('f_samples')
  }
  
  # Run Stan model
  cat("Begin sampling...\n")
  stan_fit <- rstan::sampling(
    stan_model, 
    data = stan_data_list, 
    chains = chains, 
    iter = iter, 
    warmup = warmup, 
    pars = pars, 
    init_r = init_r, 
    seed = seed, 
    save_warmup = FALSE
  )
  cat("Finished sampling.\n")
  
  return(stan_fit)
} 