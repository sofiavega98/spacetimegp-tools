# spacetimegp-tools: Spatiotemporal Gaussian Process Models

A simple R package for simulating and fitting spatiotemporal Gaussian Process models with various kernel structures and outcome distributions using Stan.

**Note**: This package repository is separate from the main `spacetimegp` repository which contains the full research code and analysis for the paper. This package provides a clean, reusable interface for the core simulation and modeling functions.

## Installation

```r
# Install from GitHub
devtools::install_github("yourusername/spacetimegp-tools")

# Or install from local directory
devtools::install(".")
```

## Usage

The package provides two main functions:

### 1. Simulate Data

```r
# Simulate Poisson data with separable kernel
data <- simulate_data(
  kernel = "sep", 
  seed = 1, 
  distribution = "poisson"
)

# Simulate Normal data with nonseparable kernel
data <- simulate_data(
  kernel = "nonsep", 
  seed = 1, 
  distribution = "normal"
)
```

### 2. Fit Models

```r
# Fit separable Poisson model
fit <- fit_model(
  data = data, 
  kernel = "sep", 
  outcome_type = "poisson"
)

# Fit nonseparable Normal model
fit <- fit_model(
  data = data, 
  kernel = "nonsep", 
  outcome_type = "normal"
)
```

## Available Options

### Kernel Types
- `"sep"`: Separable spatiotemporal kernel
- `"nonsep"`: Non-separable spatiotemporal kernel  
- `"ICM"`: Intrinsic Coregionalization Model
- `"gneiting"`: Gneiting's non-separable covariance function (simulation only)

### Outcome Types
- `"poisson"`: Poisson distribution with log link
- `"normal"`: Normal distribution with identity link

## Dependencies

- `rstan` (>= 2.21.0): For Stan model fitting
- `MASS`: For multivariate normal sampling
- `parallel`: For parallel processing

## Example Workflow

```r
library(spacetimegp)

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

## Relationship to Main Repository

This package extracts the core simulation and modeling functionality from the main `spacetimegp` research repository. The main repository contains:

- Complete simulation studies and analysis
- Case study applications
- Paper figures and tables
- Full reproducibility code for research results

This tools package provides:
- Clean, documented functions for data simulation
- Easy-to-use interface for model fitting
- Reusable components for other projects
- Standalone package that can be installed independently

## Citation

If you use this package in your research, please cite the main paper:

```bibtex
@article{vega2023spacetimegp,
  title={Practical considerations for Gaussian Process modeling for causal inference quasi-experimental studies with panel data},
  author={Vega, Sofia and co-authors},
  journal={Journal Name},
  year={2023}
}
```

## Project Structure

### `kernel_demo/`: Kernel Function Demonstrations
This directory contains code for demonstrating and analyzing kernel functions and their properties:
- `0_functions.R`: Core functions for kernel operations and calculations
- `1_create_sim_data.R`: Data generation for kernel demonstrations
- `2_calculate_weights.R`: Weight calculations for kernel functions
- `3_analyze_unit6_weights.R`: Analysis of unit weights
- `view_kernel_3d.R`: 3D visualization of kernel functions
- `est_functional_boxplots.R`: Functional boxplot estimation

### `simulations/`: Simulation Code
This directory contains code for running various simulations:
- `0_functions.R`: Core simulation functions
- `1_create_sim_data.R`: Data generation for simulations
- `2_run_simulations_normal.R`: Normal distribution simulations
- `2_run_simulations_poisson.R`: Poisson distribution simulations
- `3_export_results_normal.R`: Export normal simulation results
- `3_export_results_poisson.R`: Export Poisson simulation results
- `plot_counts_paper.R`: Visualization of simulation results
- `plot_DGP.R`: Data generating process visualization

### `STAN/`: Stan Model Implementations
This directory contains various Stan model implementations:
- Normal models:
  - `normal_nonsep.stan`: Non-separable normal model
  - `normal_RBF.stan`: Radial Basis Function normal model
  - `normal_ICM.stan`: Intrinsic Coregionalization Model
  - `normal_nonsep_NNGP.stan`: Non-separable NNGP normal model
  - `normal_RBF_NNGP.stan`: RBF NNGP normal model
  - `normal_ICM_NNGP.stan`: ICM NNGP normal model
- Poisson models:
  - `poisson_nonsep.stan`: Non-separable Poisson model
  - `poisson_RBF.stan`: RBF Poisson model
  - `poisson_ICM.stan`: ICM Poisson model
  - `poisson_nonsep_NNGP.stan`: Non-separable NNGP Poisson model
  - `poisson_RBF_NNGP.stan`: RBF NNGP Poisson model
  - `poisson_ICM_NNGP.stan`: ICM NNGP Poisson model
- Other:
  - `SpaceTimeAR.stan`: Space-time autoregressive model

## Setup Instructions

1. Clone this repository:
```bash
git clone [repository-url]
cd [repository-name]
```

2. Install R (version 4.0.0 or later) from [CRAN](https://cran.r-project.org/)

3. Install required R packages:
```R
install.packages(c("tidyverse", "rstan", "ggplot2", "dplyr", "tidyr", "purrr", "magrittr", "knitr", "rmarkdown"))
```

4. Install Stan:
```R
install.packages("rstan")
```

## Usage Instructions

### Kernel Demonstrations
1. Start with `0_functions.R` to understand the core kernel functions
2. Run `1_create_sim_data.R` to generate demonstration data
3. Use `2_calculate_weights.R` to compute kernel weights
4. Analyze results with `3_analyze_unit6_weights.R`
5. Visualize kernels in 3D using `view_kernel_3d.R`

### Running Simulations
1. Begin with `0_functions.R` to set up simulation functions
2. Generate simulation data using `1_create_sim_data.R`
3. Run simulations:
   - For normal distributions: `2_run_simulations_normal.R`
   - For Poisson distributions: `2_run_simulations_poisson.R`
4. Export results:
   - Normal results: `3_export_results_normal.R`
   - Poisson results: `3_export_results_poisson.R`
5. Visualize results using `plot_counts_paper.R`

### Using Stan Models
1. Choose the appropriate model from the STAN directory based on your needs:
   - Normal models for continuous data
   - Poisson models for count data
   - NNGP variants for large datasets
2. Compile the model in R:
```R
library(rstan)
model <- stan_model("STAN/your_model.stan")
```
3. Prepare your data according to the model's requirements
4. Run the model:
```R
fit <- sampling(model, data = your_data, chains = 4, iter = 2000)
```

## Dependencies

The project requires the following main dependencies:
- R 4.0.0+
- rstan
- tidyverse
- ggplot2
- dplyr
- tidyr
- purrr
- magrittr
- knitr
- rmarkdown

## Contributing

[Guidelines for contributing to the project]

## License

[License information]

## Contact

[Contact information] 