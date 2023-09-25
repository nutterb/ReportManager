#' @name addEditRole
#' @title Add or Edit a Role
#' 
#' @description Adds a new role or edits an existing role in the database. 
#' 
#' @param oid `integerish(0/1)`. The OID of the role to be edited. When 
#'   length is 0, a new role will be added. 
#' @param role_name `character(1)`. The name of the role. 
#' @param role_description `character(1)`. The description of the role. 
#' @param is_active `logical(1)`. When `TRUE`, the role will be made 
#'   active. Otherwise, it is inactive.
#' @param event_user `integerish(1)`. The OID of the user performing the 
#' 
#'   
#' @export

addEditRole <- function(oid = numeric(0), 
                        role_name, 
                        role_description = NA_character_, 
                        is_active = TRUE, 
                        event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertCharacter(x = role_name, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = role_description, 
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
  
  conn <- connectToReportManager()
  
  on.exit({ DBI::dbDisconnect(conn) })
  
  role_name <- trimws(role_name)
  role_description <- trimws(role_description)
  
  event_time <- Sys.time()
  
  AddEditData <- data.frame(RoleName = role_name, 
                            RoleDescription = role_description, 
                            IsActive = is_active)
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 4), 
               EventType = c("Add", 
                             if (is_active) "Activate" else "Deactivate", 
                             "EditRoleName", 
                             "EditRoleDescription"), 
               EventDateTime = rep(event_time, 4), 
               NewValue = c("", 
                            is_active, 
                            role_name, 
                            role_description))
  
  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "Role", 
                        return_oid = TRUE)
    
    EventList$ParentRole <- rep(OID$OID, 
                                nrow(EventList))
  } else {
    EventList <- .addEditRole_editedEventList(EventList = EventList,
                                              oid       = oid,
                                              conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "Role")      
    }
  }

  insertRecord(EventList, 
               table_name = "RoleEvent", 
               return_oid = FALSE)
}

# Unexported --------------------------------------------------------

.addEditRole_editedEventList <- function(EventList, 
                                         oid,
                                         conn){
  EventList$ParentRole <- rep(oid, 
                              nrow(EventList))

  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisRole <- queryRole(oid)
  
  CurrentValue <- c(ThisRole$IsActive, 
                    ThisRole$RoleName, 
                    ThisRole$RoleDescription)
  
  EventList[vapply(CurrentValue != EventList$NewValue, 
                   isTRUE, 
                   logical(1)), ]
}
