#' @name updateInstanceIsSubmitted
#' @title Update IsSubmitted Property on a Report Instance
#' 
#' @description Changes the IsSubmitted property on a Report Instance record.
#' 
#' @param report_instance_oid `integerish(1)`. The OID of the report instance
#' @param is_submitted `logical(1)`. The value to which the IsSubmitted
#'   property will be set. 
#' @param current_user_oid `integerish(1)`. The OID of the user performing
#'   the action.
#' @param event_date_time `POSIXct(1)`. The date/time at which the action
#'   was performed
#' 
#' @export

updateInstanceIsSubmitted <- function(report_instance_oid, 
                                      is_submitted, 
                                      current_user_oid, 
                                      event_date_time = Sys.time()){
  
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = is_submitted, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = current_user_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertPOSIXct(x = event_date_time, 
                           len = 1, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  dataStatement <- .updateInstanceIsSubmitted_dataStatement()
  eventStatement <- .updateInstanceIsSubmitted_eventStatement()

  res <- DBI::dbSendStatement(
    conn, 
    DBI::sqlInterpolate(
      conn, 
      dataStatement, 
      is_submitted = as.numeric(is_submitted), 
      oid = report_instance_oid
    )
  )
  
  DBI::dbClearResult(res)
  
  res <- DBI::dbSendStatement(
    conn, 
    DBI::sqlInterpolate(
      conn, 
      eventStatement, 
      ParentReportInstance = report_instance_oid, 
      EventUser = current_user_oid, 
      EventDateTime = format(event_date_time, 
                             format = "%Y-%m-%d %H:%M:%S"),
      NewValue = as.character(is_submitted)
    )
  )
  
  DBI::dbClearResult(res)
}

.updateInstanceIsSubmitted_dataStatement <- function(flavor = getOption("RM_sql_flavor")){
  schema <- if (flavor == "sql_server") "dbo." else ""
  
  sprintf("UPDATE %sReportInstance 
          SET IsSubmitted = ?is_submitted 
          WHERE OID = ?oid", 
          schema)
}

.updateInstanceIsSubmitted_eventStatement <- function(flavor = getOption("RM_sql_flavor")){
  schema <- if (flavor == "sql_server") "dbo." else ""
  
  sprintf("INSERT INTO %sReportInstanceEvent
          (ParentReportInstance, EventUser, EventType, EventDateTime, NewValue)
          VALUES
          (?ParentReportInstance, ?EventUser, 'EditIsSubmitted', ?EventDateTime, ?NewValue)",
          schema)
}
