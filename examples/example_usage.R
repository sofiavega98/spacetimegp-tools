# Example usage of spacetimegp package
# This script demonstrates the basic workflow

library(spacetimegp)

# Example 1: Simulate and fit Poisson data with separable kernel
cat("Example 1: Poisson data with separable kernel\n")
cat("==============================================\n")

# Simulate data
sim_data_pois <- simulate_data(
  kernel = "sep",
  seed = 123,
  distribution = "poisson",
  n_counties = 25,  # Smaller for faster example
  n_timepoints = 10
)

cat("Data simulated successfully!\n")
cat("Number of observations:", nrow(sim_data_pois), "\n")
cat("Number of counties:", length(unique(sim_data_pois$county_id)), "\n")
cat("Number of time points:", length(unique(sim_data_pois$time)), "\n\n")

# Fit model (with reduced iterations for faster example)
fit_pois <- fit_model(
  data = sim_data_pois,
  kernel = "sep",
  outcome_type = "poisson",
  chains = 1,
  iter = 500,  # Reduced for example
  warmup = 250
)

cat("Model fitting completed!\n\n")

# Example 2: Simulate and fit Normal data with nonseparable kernel
cat("Example 2: Normal data with nonseparable kernel\n")
cat("===============================================\n")

# Simulate data
sim_data_norm <- simulate_data(
  kernel = "nonsep",
  seed = 456,
  distribution = "normal",
  n_counties = 25,
  n_timepoints = 10
)

cat("Data simulated successfully!\n")
cat("Number of observations:", nrow(sim_data_norm), "\n\n")

# Fit model
fit_norm <- fit_model(
  data = sim_data_norm,
  kernel = "nonsep",
  outcome_type = "normal",
  chains = 1,
  iter = 500,
  warmup = 250
)

cat("Model fitting completed!\n\n")

# Example 3: Simulate data with ICM kernel
cat("Example 3: ICM kernel simulation\n")
cat("===============================\n")

sim_data_icm <- simulate_data(
  kernel = "ICM",
  seed = 789,
  distribution = "poisson",
  n_counties = 25,
  n_timepoints = 10,
  J = 3  # Number of latent factors
)

cat("ICM data simulated successfully!\n")
cat("Number of observations:", nrow(sim_data_icm), "\n\n")

# Example 4: Simulate data with Gneiting kernel
cat("Example 4: Gneiting kernel simulation\n")
cat("=====================================\n")

sim_data_gneiting <- simulate_data(
  kernel = "gneiting",
  seed = 101,
  distribution = "normal",
  n_counties = 25,
  n_timepoints = 10
)

cat("Gneiting data simulated successfully!\n")
cat("Number of observations:", nrow(sim_data_gneiting), "\n\n")

# Summary
cat("Summary\n")
cat("=======\n")
cat("All examples completed successfully!\n")
cat("The package provides:\n")
cat("- Data simulation with 4 kernel types (sep, nonsep, ICM, gneiting)\n")
cat("- Model fitting with 3 kernel types (sep, nonsep, ICM)\n")
cat("- Support for both Poisson and Normal outcomes\n")
cat("- Automatic Stan model selection and data formatting\n") 