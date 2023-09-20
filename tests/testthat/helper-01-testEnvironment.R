options(width = 200)

library(checkmate)
library(DBI)
library(keyring)

# keyring_create("ReportManager", "[PASSWORD HERE]")
# key_set_with_value(service = "ReportManager", 
#                    username = "SqlServerDriver", 
#                    password = "[DRVER NAME HERE]", 
#                    keyring = "ReportManager")
# key_set_with_value(service = "ReportManager", 
#                    username = "SqlServerServer",
#                    password = "[SERVER NAME HERE]", 
#                    keyring = "ReportManager")
# key_set_with_value(service = "ReportManager", 
#                    username = "SqlServerDatabase", 
#                    password = "[DATABASE NAME HERE]", 
#                    keyring = "ReportManager")

temp_sqlite <- tempfile(fileext = ".sqlite")

options(
  RM_sqlite_file = temp_sqlite, 
  RM_sqlServer_driver = keyring::key_get("ReportManager", "SqlServerDriver", "ReportManager"), 
  RM_sqlServer_server = keyring::key_get("ReportManager", "SqlServerServer", "ReportManager"), 
  RM_sqlServer_database = keyring::key_get("ReportManager", "SqlServerDatabase", "ReportManager")
)

SQL_SERVER_READY <- TRUE
invisible(tryCatch(connectToReportManager_sqlServer(), 
                   error = function(cond) SQL_SERVER_READY <<- FALSE))
SQL_SERVER_READY_MESSAGE <- "The SQL Server Database connection could not be established for testing."


SQLITE_READY <- requireNamespace("RSQLite", quietly=TRUE)
SQLITE_READY_MESSAGE <- "`RSQLite` is required to test SQLite connections."