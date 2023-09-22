#' @name addEditReportUser
#' @title Add or Edit a ReportUser Entry
#' 
#' @description Executes the SQL statement to add or edit an existing 
#'   ReportUser while adding the appropriate events. 
#'   
#'   When editing, values are compared against what is currently in the 
#'   database and only changed values are edited.
#'   
#' @param oid `integerish(0/1)`. The OID of the ReortUser entry to be 
#'   edited. Use `numeric(0)` to add a new entry.
#' @param last_name `character(1)`. The last name of the user.
#' @param first_name `character(1)`. The first name of the user. 
#' @param login_id `character(1)`. The login ID of the user. 
#' @param email `character(0/1)`. The e-mail address of the user. If not
#'   provided, the user will not receive reports distributed by e-mail but
#'   may use the application otherwise.
#' @param is_internal `logical(1)`. When `TRUE`, the user is considered 
#'   internal and may receive incomplete reports. When `FALSE`, only reports
#'   that have completed signatures will be distributed.
#' @param is_active `logical(1)`. When `TRUE`, the user has access to the 
#'   application and may receive e-mails with distributed reports. When 
#'   `FALSE`, access is denied and no e-mails may be sent to the user.
#' @param event_user `integerish(1)`. The ReportUser.OID of the user performing
#'   the action.
#'   
#' @export

addEditReportUser <- function(oid         = numeric(0), 
                              last_name, 
                              first_name, 
                              login_id, 
                              email, 
                              is_internal = FALSE, 
                              is_active   = TRUE, 
                              event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertCharacter(x = last_name, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = first_name, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = login_id, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = email, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertLogical(x = is_internal, 
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
    data.frame(EventReportUser = rep(event_user, 7), 
               EventType = c("Add", 
                             "EditLastName", 
                             "EditFirstName", 
                             "EditLoginId", 
                             "EditEmailAddress", 
                             if (is_internal) "SetInternalTrue" else "SetInternalFalse", 
                             if (is_active) "Activate" else "Deactivate"), 
               EventDateTime = rep(event_time, 7), 
               NewValue = c("", 
                            last_name, 
                            first_name, 
                            login_id, 
                            email, 
                            is_internal, 
                            is_active))
  
  if (length(oid) == 0){
    OID <- .addEditReportUser_addUser(last_name   = last_name, 
                                      first_name  = first_name, 
                                      login_id    = login_id, 
                                      email       = email, 
                                      is_internal = is_internal, 
                                      is_active   = is_active, 
                                      conn        = conn)
    
    EventList$ParentReportUser <- rep(OID$OID, 
                                      nrow(EventList))
  } else {
    EventList <- .addEditReportUser_editedEventList(EventList = EventList,
                                                    oid       = oid,
                                                    conn      = conn)
    
    .addEditReportUser_editUser(oid         = oid, 
                                last_name   = last_name, 
                                first_name  = first_name, 
                                login_id    = login_id, 
                                email       = email, 
                                is_internal = is_internal, 
                                is_active   = is_active, 
                                conn        = conn)
  }

  .addEditReportUser_addUserEvents(EventList, 
                                   conn)
}

# Unexported --------------------------------------------------------

.addEditReportUser_addUser <- function(last_name, 
                                       first_name, 
                                       login_id, 
                                       email, 
                                       is_internal, 
                                       is_active, 
                                       conn){
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sqlite" = .addEditReportUser_addReportUserStatement_sqlite, 
           "sql_server" = .addEditReportUser_addReportUserStatement_sqlServer)
  
  addAndReturnOid(conn, 
                  statement, 
                  list(last_name, 
                       first_name, 
                       login_id, 
                       email, 
                       as.numeric(is_internal), 
                       as.numeric(is_active)))
}

.addEditReportUser_editUser <- function(oid, 
                                        last_name, 
                                        first_name, 
                                        login_id, 
                                        email, 
                                        is_internal, 
                                        is_active, 
                                        conn){
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sqlite" = .addEditReportUser_editReportUserStatement_sqlite, 
           "sql_server" = .addEditReportUser_editReportUserStatement_sqlServer)
  
  result <- 
    DBI::dbSendStatement(
      conn, 
      statement, 
      list(last_name, 
           first_name, 
           login_id, 
           email, 
           as.numeric(is_internal), 
           as.numeric(is_active), 
           oid)
    )
  
  DBI::dbClearResult(result)
}

.addEditReportUser_addUserEvents <- function(EventList, 
                                             conn){
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sqlite" = .addEditReportUser_addEventStatement_sqlite, 
           "sql_server" = .addEditReportUser_addEventStatement_sqlServer)

  for (i in seq_len(nrow(EventList))){
    result <- 
      DBI::dbSendStatement(
        conn, 
        statement, 
        list(EventList$ParentReportUser[i], 
             EventList$EventReportUser[i], 
             EventList$EventType[i], 
             format(EventList$EventDate[i], 
                    format = "%Y-%m-%d %H:%M:%S"), 
             EventList$NewValue[i])
      )
    DBI::dbClearResult(result)
  }
}

.addEditReportUser_editedEventList <- function(EventList, 
                                               oid,
                                               conn){
  EventList$ParentReportUser <- rep(oid, 
                                    nrow(EventList))
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisReportUser <- queryReportUser(oid)
  
  EventList$CurrentValue <- c(ThisReportUser$LastName, 
                              ThisReportUser$FirstName, 
                              ThisReportUser$LoginId, 
                              ThisReportUser$EmailAddress, 
                              ThisReportUser$IsInternal, 
                              ThisReportUser$IsActive)
  
  EventList[EventList$CurrentValue != EventList$NewValue, ]
}

# Add ReportUser Statements -----------------------------------------

.addEditReportUser_addReportUserStatement_sqlServer <- 
  "INSERT INTO dbo.ReportUser
   (LastName, FirstName, LoginId, EmailAddress, IsInternal, IsActive)
   OUTPUT INSERTED.OID
   VALUES
   (?,        ?,         ?,       ?,            ?,          ?)"

.addEditReportUser_addReportUserStatement_sqlite <- 
  "INSERT INTO ReportUser
   (LastName, FirstName, LoginId, EmailAddress, IsInternal, IsActive)
   VALUES
   (?,        ?,         ?,       ?,            ?,          ?)"

# Update ReportUser Statement ---------------------------------------

.addEditReportUser_editReportUserStatement_sqlServer <- 
  "UPDATE dbo.ReportUser
   SET LastName = ?, 
       FirstName = ?, 
       LoginId = ?, 
       EmailAddress = ?, 
       IsInternal = ?, 
       IsActive = ?
   WHERE OID = ?"

.addEditReportUser_editReportUserStatement_sqlite <- 
  "UPDATE ReportUser
   SET LastName = ?, 
       FirstName = ?, 
       LoginId = ?, 
       EmailAddress = ?, 
       IsInternal = ?, 
       IsActive = ?
   WHERE OID = ?"

# Add Event Statements ----------------------------------------------

.addEditReportUser_addEventStatement_sqlServer <- 
  "INSERT INTO dbo.ReportUserEvent
   (ParentReportUser, EventReportUser, EventType, EventDateTime, NewValue)
   VALUES
   (?,                ?,               ?,         ?,             ?)"

.addEditReportUser_addEventStatement_sqlite <- 
  "INSERT INTO ReportUserEvent
   (ParentReportUser, EventReportUser, EventType, EventDateTime, NewValue)
   VALUES
   (?,                ?,               ?,         ?,             ?)"
