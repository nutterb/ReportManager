#' @name connectToReportManager
#' @title Connect to the Report Manager Database
#' 
#' @description Establish a Connection to the Report Manager Database. 
#' 
#' @inheritParams configureReportManager
#' @param ... Arguments for establishing the connection via the `DBI` package.
#' 
#' @export

connectToReportManager <- function(flavor = getOption("RM_sql_flavor")){
  coll <- checkmate::makeAssertCollection()
  
  flavor <- checkmate::matchArg(x = flavor, 
                                choices = c("sql_server", "sqlite"), 
                                add = coll)
  
  checkmate::reportAssertions(coll)
  
  switch(tolower(flavor), 
         "sqlite" = connectToReportManager_sqlite(), 
         "sql_server" = connectToReportManager_sqlServer())
}
  
  
  
# Unexported --------------------------------------------------------

connectToReportManager_sqlite <- function(database_file = getOption("RM_sqlite_file")){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = database_file, 
                             len = 1, 
                             any.missing = FALSE, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  DBI::dbConnect(RSQLite::SQLite(), 
                 database_file)
}


connectToReportManager_sqlServer <- function(driver = getOption("RM_sqlServer_driver"), 
                                             server = getOption("RM_sqlServer_server"), 
                                             database = getOption("RM_sqlServer_database"), 
                                             ...){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = driver, 
                             len = 1, 
                             any.missing = FALSE, 
                             add = coll)
  
  checkmate::assertCharacter(x = server, 
                             len = 1, 
                             any.missing = FALSE, 
                             add = coll)
  
  checkmate::assertCharacter(x = database, 
                             len = 1, 
                             any.missing = FALSE, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  DBI::dbConnect(odbc::odbc(), 
                 Driver = driver, 
                 server = server, 
                 database = database, 
                 Trusted_Connection = "yes", 
                 Encrypt = "yes", 
                 TrustServerCertificate = "yes")
  
}
