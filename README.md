# spacetimegp-tools: Spatiotemporal Gaussian Process Models

A simple R package for simulating and fitting spatiotemporal Gaussian Process models with various kernel structures and outcome distributions using Stan.

## Installation

```r
# Install from GitHub
devtools::install_github("sofiavega98/spacetimegp-tools")

# Or install from local directory
devtools::install(".")
```

## Quick Start

```r
library(spacetimegptools)

# Simulate data
data <- simulate_data(
  kernel = "sep", 
  seed = 123, 
  distribution = "poisson"
)

# Fit model
fit <- fit_model(
  data = data, 
  kernel = "sep", 
  outcome_type = "poisson"
)
```

## Functions

### `simulate_data()`
Simulates spatiotemporal data with various kernel structures:

- **Kernel types**: `"sep"` (separable), `"nonsep"` (non-separable), `"ICM"` (Intrinsic Coregionalization Model), `"gneiting"`
- **Distributions**: `"poisson"` or `"normal"`
- **Parameters**: spatial/temporal lengthscales, number of locations/timepoints, etc.

### `fit_model()`
Fits spatiotemporal Gaussian Process models using Stan:

- **Kernel types**: `"sep"`, `"nonsep"`, `"ICM"`
- **Outcome types**: `"poisson"` or `"normal"`
- **MCMC options**: chains, iterations, warmup, etc.

## Example Workflow

```r
library(spacetimegptools)

# 1. Simulate data
sim_data <- simulate_data(
  kernel = "sep",
  seed = 123,
  distribution = "poisson",
  n_counties = 25,
  n_timepoints = 10
)

# 2. Fit model
fit <- fit_model(
  data = sim_data,
  kernel = "sep", 
  outcome_type = "poisson",
  chains = 2,
  iter = 1000
)

# 3. Examine results
print(fit)
plot(fit)
```

## Dependencies

- `rstan` (>= 2.21.0): For Stan model fitting
- `MASS`: For multivariate normal sampling
- `parallel`: For parallel processing

## Related Repository

This package provides the core simulation and modeling functions. For the complete research project including simulation studies, case studies, and paper reproducibility, see the main [spacetimegp repository](https://github.com/sofiavega98/spacetimegp). 