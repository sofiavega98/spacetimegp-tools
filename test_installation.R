# Test script for spacetimegp_tools package installation
# Run this script to verify everything works correctly

cat("Testing spacetimegp_tools package installation...\n")
cat("===============================================\n\n")

# Test 1: Install package
cat("1. Installing package from GitHub...\n")
tryCatch({
  devtools::install_github("sofiavega98/spacetimegp-tools", quiet = TRUE)
  cat("   ✓ Package installed successfully\n")
}, error = function(e) {
  cat("   ✗ Installation failed:", e$message, "\n")
  stop("Installation test failed")
})

# Test 2: Load package
cat("\n2. Loading package...\n")
tryCatch({
  library(spacetimegp_tools)
  cat("   ✓ Package loaded successfully\n")
}, error = function(e) {
  cat("   ✗ Loading failed:", e$message, "\n")
  stop("Loading test failed")
})

# Test 3: Check if functions are available
cat("\n3. Checking function availability...\n")
functions_to_check <- c("simulate_data", "fit_model")
for (func in functions_to_check) {
  if (exists(func)) {
    cat("   ✓ Function '", func, "' is available\n")
  } else {
    cat("   ✗ Function '", func, "' is missing\n")
  }
}

# Test 4: Test data simulation (small test)
cat("\n4. Testing data simulation...\n")
tryCatch({
  test_data <- simulate_data(
    kernel = "sep",
    seed = 123,
    distribution = "poisson",
    n_counties = 9,  # Very small for quick test
    n_timepoints = 5
  )
  cat("   ✓ Data simulation successful\n")
  cat("   ✓ Generated", nrow(test_data), "observations\n")
}, error = function(e) {
  cat("   ✗ Data simulation failed:", e$message, "\n")
})

# Test 5: Check Stan models are available
cat("\n5. Checking Stan model files...\n")
stan_files <- c(
  "poisson_ICM.stan",
  "poisson_RBF.stan", 
  "poisson_nonsep.stan",
  "normal_ICM.stan",
  "normal_RBF.stan",
  "normal_nonsep.stan"
)

stan_path <- system.file("stan", package = "spacetimegp_tools")
if (stan_path != "") {
  cat("   ✓ Stan models directory found at:", stan_path, "\n")
  for (file in stan_files) {
    full_path <- file.path(stan_path, file)
    if (file.exists(full_path)) {
      cat("   ✓ Stan file '", file, "' found\n")
    } else {
      cat("   ✗ Stan file '", file, "' missing\n")
    }
  }
} else {
  cat("   ✗ Stan models directory not found\n")
}

cat("\n===============================================\n")
cat("Installation test completed!\n")
cat("If all tests passed (✓), your package is working correctly.\n")
cat("You can now use: library(spacetimegp_tools)\n") 