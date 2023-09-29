#' @name queryFooter
#' @title Retrieve Footer Objects from the Database
#' 
#' @description Retrieves Footer objects from the database. 
#' 
#' @param oid `integerish(0/1)`. The OID of the Footer object to retrieve.
#'   When length is 0, all objects are returned.
#'
#' @export

queryFooter <- function(oid = numeric(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .queryFooter_sqlite, 
                      "sql_server" = .queryFooter_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  if (length(oid) > 0){
    statement <- paste0(statement, " WHERE [OID] = ?")
  }
  
  param_list <- list(oid)
  param_list <- param_list[lengths(param_list) > 0]
  
  Footer <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    Footer$IsActive <- as.logical(Footer$IsActive)
  }
  
  Footer
}

# Unexported --------------------------------------------------------

.queryFooter_sqlite <- 
  "SELECT [OID], 
      [Footer],
      [IsActive]
    FROM [Footer]"

.queryFooter_sqlServer <- 
  "SELECT [OID], 
      [Footer],
      [IsActive]
    FROM dbo.[Footer]"
