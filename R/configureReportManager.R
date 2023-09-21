#' @name configureReportManager
#' @title Configure the Report Manager
#' 
#' @description Assists the user is setting options for the report manager.
#'   This function is called for its side effects in setting options 
#'   for the ReportManager application.  
#' 
#' @param flavor `character(1)`. The flavor of SQL used. This will 
#'   determine the SQL engine used. One of `c("sql_server", "sqlite")`.
#' @param database_file `character(1)`. For SQLite connections, the file 
#'   containing the database.
#' @param driver `character(1)`. For SQL Server connections, the driver to 
#'   use when connecting to the database.
#' @param server `character(1)`. For SQL Server connections, the server
#'   to connect to.
#' @param database `character(1)`. For SQL Server connections, the database 
#'   to connect to.
#'   
#' @details For all arguments, `NULL` is an acceptable value. When `NULL` 
#' is provided, the option associated with that argument will not be altered.
#'   
#' @return
#' Invisibly returns `NULL`
#' 
#' @export


configureReportManager <- function(sql_flavor    = NULL,
                                   database_file = NULL, 
                                   driver        = NULL, 
                                   server        = NULL, 
                                   database      = NULL){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  if (!is.null(sql_flavor)){
    sql_flavor <- checkmate::matchArg(x = sql_flavor, 
                                      choices = SUPPORTED_SQL_FLAVOR, 
                                      .var.name = "sql_flavor",
                                      add = coll)
  }
  
  checkmate::assertCharacter(x = database_file, 
                             len = 1, 
                             any.missing = FALSE, 
                             null.ok = TRUE, 
                             add = coll)
  
  checkmate::assertCharacter(x = driver, 
                             len = 1, 
                             any.missing = FALSE,  
                             null.ok = TRUE,
                             add = coll)
  
  checkmate::assertCharacter(x = server, 
                             len = 1, 
                             any.missing = FALSE,  
                             null.ok = TRUE,
                             add = coll)
  
  checkmate::assertCharacter(x = database, 
                             len = 1, 
                             any.missing = FALSE,  
                             null.ok = TRUE,
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Set the options -------------------------------------------------
  
  if (!is.null(sql_flavor)){
    options(RM_sql_flavor = sql_flavor)
  }
  
  if (!is.null(database_file)){
    options(RM_sqlite_file = database_file)
  }
  
  if (!is.null(driver)){
    options(RM_sqlServer_driver = driver)
  }
  
  if (!is.null(driver)){
    options(RM_sqlServer_server = server)
  }
  
  if (!is.null(driver)){
    options(RM_sqlServer_database = database)
  }
}
