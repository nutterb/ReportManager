#' @name queryReportUser
#' @title Fetch the ReportUser Table
#' 
#' @description Fetch the ReportUser table from the database.
#' 
#' @param oid `integerish(0/1)`. The OID of the Report User to fetch
#'   from the database. By default (`character(0)`), all records are 
#'   returned.  
#' 
#' @export

queryReportUser <- function(oid = numeric(0)){
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
                      "sqlite" = .queryReportUser_sqlite, 
                      "sql_server" = .queryReportUser_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  if (length(oid) > 0){
    statement <- paste0(statement, " WHERE OID = ", oid)
  }
  
  ReportUser <- DBI::dbGetQuery(conn, statement)
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    ReportUser$IsInternal <- as.logical(ReportUser$IsInternal)
    ReportUser$IsActive <- as.logical(ReportUser$IsActive)
  }
  
  ReportUser
}

# Unexported --------------------------------------------------------

.queryReportUser_sqlite <- 
  "SELECT OID, 
      LastName,
      FirstName,
      LoginId,
      EmailAddress, 
      IsInternal, 
      IsActive
    FROM ReportUser"

.queryReportUser_sqlServer <- 
  "SELECT OID, 
      LastName, 
      FirstName, 
      LoginId,
      EmailAddress, 
      IsInternal, 
      IsActive
    FROM dbo.ReportUser"