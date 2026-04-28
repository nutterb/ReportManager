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


library(ReportManager)

configureReportManager(flavor = "sqlite", 
                       database_file = "/home/l102036/Temp/report-example.sqlite")

# initializeReportManagerDatabase(
#   filename = system.file("Sql/SQLite.sql",
#                          package = "ReportManager"), 
#   last_name = "Nutter", 
#   first_name = "Benjamin", 
#   login_id = "l102036", 
#   email = "benjamin.nutter@lilly.com")

startReportManager("benjamin.nutter@lilly.com")
