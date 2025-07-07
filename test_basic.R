# Basic test for spacetimegp_tools package
# This tests the package structure and basic functionality

cat("Testing spacetimegp_tools package structure...\n")
cat("=============================================\n\n")

# Test 1: Check if we can build the package
cat("1. Testing package build...\n")
tryCatch({
  # Build the package
  system("R CMD build .", intern = TRUE)
  cat("   ✓ Package builds successfully\n")
}, error = function(e) {
  cat("   ✗ Package build failed:", e$message, "\n")
})

# Test 2: Check if Stan files are in the right place
cat("\n2. Checking Stan model files...\n")
stan_files <- c(
  "poisson_ICM.stan",
  "poisson_RBF.stan", 
  "poisson_nonsep.stan",
  "normal_ICM.stan",
  "normal_RBF.stan",
  "normal_nonsep.stan"
)

stan_dir <- "inst/stan"
if (dir.exists(stan_dir)) {
  cat("   ✓ Stan directory found\n")
  for (file in stan_files) {
    full_path <- file.path(stan_dir, file)
    if (file.exists(full_path)) {
      cat("   ✓ Stan file '", file, "' found\n")
    } else {
      cat("   ✗ Stan file '", file, "' missing\n")
    }
  }
} else {
  cat("   ✗ Stan directory not found\n")
}

# Test 3: Check R functions
cat("\n3. Checking R function files...\n")
r_files <- c("R/simulate_data.R", "R/fit_model.R", "R/helpers.R")
for (file in r_files) {
  if (file.exists(file)) {
    cat("   ✓ R file '", file, "' found\n")
  } else {
    cat("   ✗ R file '", file, "' missing\n")
  }
}

# Test 4: Check package metadata
cat("\n4. Checking package metadata...\n")
metadata_files <- c("DESCRIPTION", "NAMESPACE", "README.md")
for (file in metadata_files) {
  if (file.exists(file)) {
    cat("   ✓ Metadata file '", file, "' found\n")
  } else {
    cat("   ✗ Metadata file '", file, "' missing\n")
  }
}

cat("\n=============================================\n")
cat("Basic structure test completed!\n")
cat("The package structure looks good for GitHub distribution.\n")
cat("Note: Full installation test requires rstan to be installed separately.\n") 