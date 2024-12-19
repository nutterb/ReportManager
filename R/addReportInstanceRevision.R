#' @name addReportInstanceRevision
#' @title Add a Revision Record for a Submitted Report
#' 
#' @description Adds a record to the ReportInstanceRevision table. These 
#' records indicate the start of a revision of a previously submitted report.
#' 
#' @param report_instance_oid `integerish(1)`. The OID of the report instance.
#' @param parent_user `integerish(1)`. The OID of the user initiating the 
#'   revision. 
#' @param revision_date_time `POSIXct(1)`. The time stamp the revision is 
#'   initiated.
#' @param reason `character(1)`. The reason for the revision. 
#' 
#' @export

addReportInstanceRevision <- function(report_instance_oid, 
                                      parent_user, 
                                      revision_date_time, 
                                      reason){
  # Argument validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertPOSIXct(x = revision_date_time, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertCharacter(x = reason, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  reason <- trimws(reason)
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .addReportInstanceRevision_sqlite, 
                      "sql_server" = .addReportInstanceRevision_sqlServer, 
                      stop(sprintf("Query not defined for SQL flavor '%s'",
                                   getOption("RM_sql_flavor"))))
  
  result <- 
    DBI::dbSendStatement(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        parent_report_instance = report_instance_oid, 
        parent_user = parent_user, 
        event_date_time = format(revision_date_time, 
                                 format = "%Y-%m-%d %H:%M:%S"), 
        reason = reason
      )
    )
  
  DBI::dbClearResult(result)
}

.addReportInstanceRevision_sqlite <- "
  INSERT INTO ReportInstanceRevision
  (ParentReportInstance, ParentUser, EventDateTime, Reason)
  VALUES
  (?parent_report_instance, ?parent_user, ?event_date_time, ?reason)
"

.addReportInstanceRevision_sqlServer <- "
  INSERT INTO dbo.ReportInstanceRevision
  (ParentReportInstance, ParentUser, EventDateTime, Reason)
  VALUES
  (?parent_report_instance, ?parent_user, ?event_date_time, ?reason)
"
