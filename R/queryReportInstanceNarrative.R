#' @name queryReportInstanceNarrative
#' @title Retrieve the Narrative for a Report Instance
#' 
#' @description Enables the user to query the database for the current  
#'   associated with a report instance.
#'   
#' @param report_instance_oid `integerish(1)`. The OID of the report instance
#'   for which the narrative is to be retrieved.
#'   
#' @export

queryReportInstanceNarrative <- function(report_instance_oid){
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
           "sqlite" = .queryReportInstanceNarrative_sqlite, 
           "sql_server" = .queryReportInstanceNarrative_sqlServer, 
           stop(sprintf("Query not defined for SQL flavor '%s'", 
                        getOption("RM_sql_flavor"))))
  
  Narrative <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn,
        statement, 
        report_instance_oid = report_instance_oid
      )
    )
  
  Narrative
}

# Unexported --------------------------------------------------------

.queryReportInstanceNarrative_sqlite <- "
SELECT 
  OID, 
  ParentReportInstance,
  Narrative
FROM ReportInstanceNarrative
WHERE ParentReportInstance = ?report_instance_oid
"

.queryReportInstanceNarrative_sqlServer <- "
SELECT 
  OID, 
  ParentReportInstance,
  Narrative
FROM dbo.ReportInstanceNarrative
WHERE ParentReportInstance = ?report_instance_oid
"
