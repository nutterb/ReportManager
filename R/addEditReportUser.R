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
  
  last_name  <- trimws(last_name)
  first_name <- trimws(first_name)
  login_id   <- trimws(login_id)
  email      <- trimws(email)
  
  event_time <- Sys.time()
  
  AddEditData <- data.frame(LastName = last_name, 
                            FirstName = first_name, 
                            LoginId = login_id, 
                            EmailAddress = email, 
                            IsInternal = as.numeric(is_internal))
  
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
    
    OID <- insertRecord(AddEditData, 
                        table_name = "ReportUser")
    
    EventList$ParentReportUser <- rep(OID$OID, 
                                      nrow(EventList))
  } else {
    EventList <- .addEditReportUser_editedEventList(EventList = EventList,
                                                    oid       = oid,
                                                    conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "ReportUser")
    }
  }

  insertRecord(EventList, 
               table_name = "ReportUserEvent", 
               return_oid = FALSE)
}

# Unexported --------------------------------------------------------

.addEditReportUser_editedEventList <- function(EventList, 
                                               oid,
                                               conn){
  EventList$ParentReportUser <- rep(oid, 
                                    nrow(EventList))
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisReportUser <- queryReportUser(oid)
  
  CurrentValue <- c(ThisReportUser$LastName, 
                    ThisReportUser$FirstName, 
                    ThisReportUser$LoginId, 
                    ThisReportUser$EmailAddress, 
                    ThisReportUser$IsInternal, 
                    ThisReportUser$IsActive)
  
  EventList[CurrentValue != EventList$NewValue, ]
}
