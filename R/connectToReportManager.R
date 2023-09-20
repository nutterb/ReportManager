#' @name connectToReportManager
#' @title Connect to the Report Manager Database
#' 
#' @description Establish a Connection to the Report Manager Database. 
#' 
#' @param flavor `character(1)`. The flavor of SQL used. This will determine
#'   the SQL engine used. One of `c("sql_server", "sqlite")`.
#' @param database_file `character(1)`. For SQLite connections, the file 
#'   containing the database.
#' @param driver `character(1)`. For SQL Server connections, the driver to 
#'   use when connecting to the database.
#' @param server `character(1)`. For SQL Server connections, the server
#'   to connect to.
#' @param database `character(1)`. For SQL Server connections, the database 
#'   to connect to.
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
