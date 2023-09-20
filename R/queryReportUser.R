#' @name queryReportUser
#' @title Fetch the ReportUser Table
#' 
#' @description Fetch the ReportUser table from the database. 
#' 
#' @export

queryReportUser <- function(){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  query <- switch(class(conn), 
                  "SQLiteConnection" = .queryReportUser_sqlite, 
                  stop(sprintf("Query not defined for connection type %s", 
                               paste0(class(conn), collapse = ", "))))
  
  DBI::dbGetQuery(conn, query) 
}

# Unexported --------------------------------------------------------

.queryReportUser_sqlite <- 
  "SELECT OID, 
      UserName, 
      LoginId,
      EmailAddress, 
      IsInternal, 
      IsActive
    FROM ReportUser"

.queryReportUser_sqlServer <- 
  "SELECT OID, 
      UserName, 
      LoginId,
      EmailAddress, 
      IsInternal, 
      IsActive
    FROM dbo.ReportUser"