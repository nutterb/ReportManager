#' @name activateReportUser
#' @title Activate or Deactivate Report Users
#' 
#' @description Changes the value of the ReportUser.IsActive property.
#' 
#' @param oid `integerish(1)`. The OID of the ReportUser to modify. 
#' @param active `logical(1)`. The value to set in the database.
#' @param event_user `integerish(1)`. The OID of the ReportUser performing the action. 
#' 
#' @details When the value of `active` matches what is already in the database, 
#'   no action will be taken. Otherwise, the value is updated and an event
#'   is recorded.
#' 
#' @export

activateReportUser <- function(oid, 
                               active, 
                               event_user){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = active, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  current_value <- queryReportUser(oid)$IsActive
  
  if (current_value == active){
    return(NULL)
  }
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sql_server" = .activateReportUser_statement_sqlServer, 
                      "sqlite" = .activateReportUser_statement_sqlite)
  
  event_statement <- switch(getOption("RM_sql_flavor"), 
                            "sql_server" = .activateReportUser_eventStatement_sqlServer, 
                            "sqlite" = .activateReportUser_eventStatement_sqlite)
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  result <- DBI::dbSendStatement(conn, 
                                 statement, 
                                 list(as.numeric(active), 
                                      oid))
  DBI::dbClearResult(result)
  
  result <- DBI::dbSendStatement(conn, 
                                 event_statement, 
                                 list(oid, 
                                      event_user, 
                                      if (active) "Activate" else "Deactivate", 
                                      format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), 
                                      as.character(active)))
  DBI::dbClearResult(result)
}

# Unexported --------------------------------------------------------

.activateReportUser_statement_sqlServer <- 
  "UPDATE dbo.ReportUser 
   SET IsActive = ?
   WHERE OID = ?"

.activateReportUser_statement_sqlite <- 
  "UPDATE ReportUser
   SET IsActive = ?
   WHERE OID = ?"

.activateReportUser_eventStatement_sqlServer <- 
  "INSERT INTO dbo.ReportUserEvent
   (ParentReportUser, EventReportUser, EventType, EventDateTime, NewValue) 
   VALUES
   (?,                ?,               ?,         ?,             ?       )"

.activateReportUser_eventStatement_sqlite <- 
  "INSERT INTO ReportUserEvent
   (ParentReportUser, EventReportUser, EventType, EventDateTime, NewValue)
   VALUES
   (?,                ?,               ?,         ?,             ?       )"
