#' @name queryReportUser
#' @title Fetch the ReportUser Table
#' 
#' @description Fetch the ReportUser table from the database. 
#' 
#' @export

queryReportUser <- function(){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  query <- switch(getOption("RM_sql_flavor"), 
                  "sqlite" = .queryReportUser_sqlite, 
                  "sql_server" = .queryReportUser_sqlServer,
                  stop(sprintf("Query not defined for SQL flavor '%s'", 
                               getOption("RM_sql_flavor"))))
  
  DBI::dbGetQuery(conn, query) 
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