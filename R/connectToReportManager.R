#' @name connectToReportManager
#' @title Connect to the Report Manager Database
#' 
#' @description Establish a Connection to the Report Manager Database. 
#' 
#' @param sql_flavor `character(1)`. The flavor of SQL used in the backend 
#'   database. One of `c("sql_server", "sqlite")`.
#' @param ... Arguments for establishing the connection via the `DBI` package.

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

#' @rdname connectToReportManager
#' @export

connectToReportManager <- connectToReportManager_sqlite
