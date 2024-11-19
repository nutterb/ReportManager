#' @name queryReportInstanceNote
#' @title Query Database for Report Instance Notes
#' 
#' @description Queries the database for notes associated with a 
#' report instance.
#' 
#' @param report_instance_oid `integerish(1)`. The OID of the report instance.
#' 
#' @export

queryReportInstanceNote <- function(report_instance_oid){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .queryReportInstanceNote_sqlite, 
                      "sql_server" = .queryReportInstanceNote_sqlServer, 
                      stop(sprintf("Query not defined for SQL flavor '%s'",
                                   getOption("RM_sql_flavor"))))
  
  DBI::dbGetQuery(
    conn, 
    DBI::sqlInterpolate(
      conn, 
      statement, 
      oid = report_instance_oid
    )
  )
}

# Unexported --------------------------------------------------------

.queryReportInstanceNote_sqlServer <- "
  SELECT U.LoginId AS [User], 
  	RIN.NoteDateTime, 
  	RIN.Note
  FROM dbo.ReportInstanceNote RIN
    LEFT JOIN dbo.[User] U
      ON RIN.ParentUser = U.OID
  WHERE RIN.ParentReportInstance = ?oid
  ORDER BY RIN.NoteDateTime DESC
"

.queryReportInstanceNote_sqlite <- "
  SELECT U.LoginId AS [User], 
  	RIN.ParentUser, 
  	RIN.NoteDateTime, 
  	RIN.Note
  FROM ReportInstanceNote RIN
    LEFT JOIN [User] U
      ON RIN.ParentUser = U.OID
  WHERE RIN.OID = ?oid
  ORDER BY RIN.NoteDateTime DESC
"