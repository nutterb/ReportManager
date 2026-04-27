# Rebuild package documentation
devtools::document(".")

# Run the test suite
devtools::test(".")

# Run R CMD Check
# Looks for errors and omissions in documentation
devtools::check(".", build_args = "--compact-vignettes=both")

# Load all package objects
devtools::load_all(helpers = FALSE)

# Install the package locally
devtools::install_local(".", 
                        dependencies = FALSE, 
                        upgrade = "never", 
                        force = TRUE, 
                        build_vignettes = TRUE)