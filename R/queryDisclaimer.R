#' @name queryDisclaimer
#' @title Retrieve Disclaimer Objects from the Database
#' 
#' @description Retrieves Disclaimer objects from the database. 
#' 
#' @param oid `integerish(0/1)`. The OID of the Disclaimer object to retrieve.
#'   When length is 0, all objects are returned.
#'
#' @export

queryDisclaimer <- function(oid = numeric(0)){
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
                      "sqlite" = .queryDisclaimer_sqlite, 
                      "sql_server" = .queryDisclaimer_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  if (length(oid) > 0){
    statement <- paste0(statement, " WHERE [OID] = ?")
  }
  
  param_list <- list(oid)
  param_list <- param_list[lengths(param_list) > 0]

  Disclaimer <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    Disclaimer$IsActive <- as.logical(Disclaimer$IsActive)
  }
  
  Disclaimer
}

# Unexported --------------------------------------------------------

.queryDisclaimer_sqlite <- 
  "SELECT [OID], 
      [Title],
      [Disclaimer],
      [IsActive]
    FROM [Disclaimer]"

.queryDisclaimer_sqlServer <- 
  "SELECT [OID], 
      [Title],
      [Disclaimer],
      [IsActive]
    FROM dbo.[Disclaimer]"
