#' @name queryReportInstanceNarrativeEvent
#' @title Retrieve the History of Narrative Editing Events for a Report Instance
#' 
#' @description Enables the user to query the database for the history of 
#'   events where the narrative associated with a report instance was edited.
#'   
#' @param report_instance_oid `integerish(1)`. The OID of the report instance
#'   for which the narrative event history is to be retrieved.
#'   
#' @export

queryReportInstanceNarrativeEvent <- function(report_instance_oid){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sqlite" = .queryReportInstanceNarrativeEvent_sqlite, 
           "sql_server" = .queryReportInstanceNarrativeEvent_sqlServer, 
           stop(sprintf("Query not defined for SQL flavor '%s'", 
                        getOption("RM_sql_flavor"))))
  
  Event <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn,
        statement, 
        report_instance_oid = report_instance_oid
      )
    )
  
  Event
}

# Unexported --------------------------------------------------------

.queryReportInstanceNarrativeEvent_sqlite <- "
SELECT 
  U.[LastName] + ', ' + U.[FirstName] + ' (' + U.[LoginId] + ')' AS UserName,
  RINE.[EventDateTime], 
  RINE.[NewValue] AS Narrative
FROM ReportInstanceNarrativeEvent RINE
	LEFT JOIN [ReportInstanceNarrative] RIN
		ON RINE.[ParentReportInstanceNarrative] = RIN.OID
	LEFT JOIN [User] U
		ON RINE.[EventUser] = U.OID
WHERE RIN.[ParentReportInstance] = ?report_instance_oid
ORDER BY EventDateTime DESC
"

.queryReportInstanceNarrativeEvent_sqlServer <- "
SELECT 
  U.[LastName] + ', ' + U.[FirstName] + ' (' + U.[LoginId] + ')' AS UserName,
  RINE.[EventDateTime], 
  RINE.[NewValue] AS Narrative
FROM dbo.ReportInstanceNarrativeEvent RINE
	LEFT JOIN dbo.[ReportInstanceNarrative] RIN
		ON RINE.[ParentReportInstanceNarrative] = RIN.OID
	LEFT JOIN dbo.[User] U
		ON RINE.[EventUser] = U.OID
WHERE RIN.[ParentReportInstance] = ?report_instance_oid
ORDER BY EventDateTime DESC
"