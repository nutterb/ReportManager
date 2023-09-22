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
  
  event_time <- Sys.time()
  
  EventList <- 
    data.frame(EventReportUser = rep(event_user, 4), 
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
    OID <- .addEditRole_addRole(role_name        = role_name, 
                                role_description = role_description, 
                                is_active        = is_active, 
                                conn             = conn)
    
    EventList$ParentRole <- rep(OID$OID, 
                                nrow(EventList))
  } else {
    EventList <- .addEditRole_editedEventList(EventList = EventList,
                                              oid       = oid,
                                              conn      = conn)
    
    .addEditRole_editRole(oid              = oid, 
                          role_name        = role_name, 
                          role_description = role_description,
                          is_active        = is_active, 
                          conn             = conn)
  }
  
  .addEditRole_addRoleEvents(EventList, 
                             conn)
}

# Unexported --------------------------------------------------------

.addEditRole_addRole <- function(role_name, 
                                 role_description, 
                                 is_active,
                                 conn){
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .addEditRole_addRoleStatement_sqlite, 
                      "sql_server" = .addEditRole_addRoleStatement_sqlServer)
  
  addAndReturnOid(conn, 
                  statement, 
                  list(role_name, 
                       role_description, 
                       is_active))
}

.addEditRole_editRole <- function(oid, 
                                  role_name, 
                                  role_description,
                                  is_active, 
                                  conn){
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sqlite" = .addEditRole_editRoleStatement_sqlite, 
           "sql_server" = .addEditRole_editRoleStatement_sqlServer)
  
  result <- 
    DBI::dbSendStatement(
      conn, 
      statement, 
      list(role_name, 
           role_description,
           as.numeric(is_active), 
           oid)
    )
  
  DBI::dbClearResult(result)
}

.addEditRole_addRoleEvents <- function(EventList, 
                                       conn){
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sqlite" = .addEditRole_addEventStatement_sqlite, 
           "sql_server" = .addEditRole_addEventStatement_sqlServer)
  
  for (i in seq_len(nrow(EventList))){
    result <- 
      DBI::dbSendStatement(
        conn, 
        statement, 
        list(EventList$ParentRole[i], 
             EventList$EventReportUser[i], 
             EventList$EventType[i], 
             format(EventList$EventDate[i], 
                    format = "%Y-%m-%d %H:%M:%S"), 
             EventList$NewValue[i])
      )
    DBI::dbClearResult(result)
  }
}

.addEditRole_editedEventList <- function(EventList, 
                                         oid,
                                         conn){
  EventList$ParentRole <- rep(oid, 
                              nrow(EventList))
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisRole <- queryRole(oid)
  
  EventList$CurrentValue <- c(ThisRole$IsActive, 
                              ThisRole$RoleName, 
                              ThisRole$RoleDescription)
  
  EventList[EventList$CurrentValue != EventList$NewValue, ]
}

# Add Role Statements -----------------------------------------------

.addEditRole_addRoleStatement_sqlServer <- 
  "INSERT INTO dbo.[Role]
   (RoleName, RoleDescription, IsActive)
   OUTPUT INSERTED.OID
   VALUES
   (?,        ?,               ?);"

.addEditRole_addRoleStatement_sqlite <- 
  "INSERT INTO [Role]
   (RoleName, RoleDescription, IsActive)
   VALUES
   (?,        ?,               ?);"

# Edit Role Statements ----------------------------------------------

.addEditRole_editRoleStatement_sqlServer <- 
  "UPDATE dbo.[Role]
   SET [RoleName] = ?, 
       [RoleDescription] = ?, 
       [IsActive] = ?
   WHERE [OID] = ?"

.addEditRole_editRoleStatement_sqlite <- 
  "UPDATE [Role]
   SET [RoleName] = ?, 
       [RoleDescription] = ?, 
       [IsActive] = ?
   WHERE [OID] = ?"

# Add Event Statements ----------------------------------------------

.addEditRole_addEventStatement_sqlServer <- 
  "INSERT INTO dbo.[RoleEvent]
   (ParentRole, EventReportUser, EventType, EventDateTime, NewValue)
   VALUES
   (?,                ?,               ?,         ?,             ?)"

.addEditRole_addEventStatement_sqlite <- 
  "INSERT INTO [RoleEvent]
   (ParentRole, EventReportUser, EventType, EventDateTime, NewValue)
   VALUES
   (?,                ?,               ?,         ?,             ?)"
