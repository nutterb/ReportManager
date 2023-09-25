#' @name addEditUserRole 
#' @title Add and Edit User Role Assignments
#' 
#' @description Add UserRole assignments or edit existing UserRole assignments.
#' 
#' @param oid `integerish(0/1)`. The OID of the UserRole object to modify. 
#'   Use `numeric(0)` to add a new record. 
#' @param parent_user `integerish(1)`. The OID of the User in the UserRole
#'   assignment. 
#' @param parent_role `integerish(1)`. the OID of the Role in the UserRole
#'   assignment. 
#' @param is_active `logical(1)`. When `TRUE`, the assignment is active. 
#' @param event_user `integerish(1)`. The OID of the User performing the action.
#' 
#' @export

addEditUserRole <- function(oid = numeric(0), 
                            parent_user, 
                            parent_role, 
                            is_active = TRUE, 
                            event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_user,
                              len = 1,
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_role, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = is_active, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  AddEditData <- data.frame(ParentUser = parent_user, 
                            ParentRole = parent_role, 
                            IsActive   = is_active)
  
  event_time <- Sys.time()
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 2), 
               EventType = c("Add", 
                             if (is_active) "Activate" else "Deactivate"), 
               EventDateTime = rep(event_time, 2), 
               NewValue = c("", 
                            is_active), 
               stringsAsFactors = FALSE)
  
  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "UserRole", 
                        return_oid = TRUE)
    
    EventList$ParentUserRole <- rep(OID$OID, 
                                    nrow(EventList))
  } else {
    EventList <- .addEditRole_editedEventList(EventList = EventList,
                                              oid       = oid,
                                              conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "UserRole")      
    }
  }

  insertRecord(EventList, 
               table_name = "UserRoleEvent", 
               return_oid = FALSE)
}

# Unexported --------------------------------------------------------

.addEditUserRole_editedEventList <- function(EventList, 
                                             oid,
                                             conn){
  EventList$ParentRole <- rep(oid, 
                              nrow(EventList))
  
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisRole <- queryUserRole(oid)
  
  CurrentValue <- c(ThisRole$IsActive)
  
  EventList[vapply(CurrentValue != EventList$NewValue, 
                   isTRUE, 
                   logical(1)), ]
}
