#' @name activateReportUser
#' @title Activate or Deactivate Report Users
#' 
#' @description Changes the value of a table's IsActive property.
#' 
#' @param oid `integerish(1)`. The OID of the record to modify. 
#' @param active `logical(1)`. The value to set in the database.
#' @param event_user `integerish(1)`. The OID of the ReportUser performing the action. 
#' 
#' @details When the value of `active` matches what is already in the database, 
#'   no action will be taken. Otherwise, the value is updated and an event
#'   is recorded.
#' 
#' @export

activateRecord <- function(oid, 
                           active, 
                           event_user, 
                           table_name, 
                           event_table_name, 
                           parent_field_name){
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
  
  checkmate::assertCharacter(x   = table_name, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x   = event_table_name, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x   = parent_field_name, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  current_value <- 
    switch(table_name, 
           "User" = queryUser(oid)$IsActive, 
           "Role" = queryRole(oid)$IsActive, 
           stop(sprintf("Activation for table %s is not supported.", 
                        table_name)))
  
  if (current_value == active){
    return(invisible())
  }
  
  updateRecord(data       = data.frame(IsActive = as.numeric(active)), 
               where_data = data.frame(OID = oid), 
               table_name = table_name)
  
  EventData <- data.frame(Parent          = oid, 
                          EventUser       = event_user, 
                          EventType       = if (active) "Activate" else "Deactivate", 
                          EventDateTime   = format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), 
                          NewValue        = as.character(active))
  
  names(EventData)[1] <- parent_field_name
  
  insertRecord(data       = EventData, 
               table_name = event_table_name, 
               return_oid = FALSE)
}
