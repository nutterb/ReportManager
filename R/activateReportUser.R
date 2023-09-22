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
  
  checkmate::assertIntegerish(x   = oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x   = active, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x   = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  current_value <- queryReportUser(oid)$IsActive
  
  if (current_value == active){
    return(invisible())
  }
  
  updateRecord(data       = data.frame(IsActive = as.numeric(active)), 
               where_data = data.frame(OID = oid), 
               table_name = "ReportUser")
  
  EventData <- data.frame(ParentReportUser = oid, 
                          EventReportUser  = event_user, 
                          EventType        = if (active) "Activate" else "Deactivate", 
                          EventDateTime    = format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), 
                          NewValue         = as.character(active))
  
  insertRecord(data       = EventData, 
               table_name = "ReportUserEvent", 
               return_oid = FALSE)
}
