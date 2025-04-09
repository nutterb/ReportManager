#' @name addEditUser
#' @title Add or Edit a User Entry
#' 
#' @description Executes the SQL statement to add or edit an existing 
#'   User while adding the appropriate events. 
#'   
#'   When editing, values are compared against what is currently in the 
#'   database and only changed values are edited.
#'   
#' @param oid `integerish(0/1)`. The OID of the User entry to be 
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
#' @param event_user `integerish(1)`. The User.OID of the user performing
#'   the action.
#'   
#' @export

addEditUser <- function(oid = numeric(0), 
                        last_name, 
                        first_name, 
                        login_id, 
                        email, 
                        is_internal = FALSE, 
                        is_active   = TRUE, 
                        event_user, 
                        signature_file = data.frame()){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertString(x = last_name, 
                          max.chars = 50, 
                          add = coll)
  
  checkmate::assertString(x = first_name, 
                          max.chars = 50, 
                          add = coll)
  
  checkmate::assertString(x = login_id, 
                          max.chars = 50, 
                          add = coll)
  
  checkmate::assertString(x = email, 
                          max.chars = 100, 
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
  
  checkmate::assertDataFrame(x = signature_file, 
                             max.rows = 1, 
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
                            IsInternal = as.numeric(is_internal), 
                            IsActive = as.numeric(is_active))
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 7), 
               EventType = c("Add", 
                             "EditLastName", 
                             "EditFirstName", 
                             "EditLoginId", 
                             "EditEmailAddress", 
                             if (is_internal) "SetInternalTrue" else "SetInternalFalse", 
                             if (is_active) "Activate" else "Deactivate"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 7), 
               NewValue = c("", 
                            last_name, 
                            first_name, 
                            login_id, 
                            email, 
                            is_internal, 
                            is_active))
  
  if (length(oid) == 0){
    
    OID <- insertRecord(AddEditData, 
                        table_name = "User")
    
    EventList$ParentUser <- rep(OID$OID, 
                                      nrow(EventList))
  } else {
    EventList <- .addEditUser_editedEventList(EventList = EventList,
                                                    oid       = oid,
                                                    conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "User")
    }
  }

  if (nrow(EventList)){
    insertRecord(EventList, 
                 table_name = "UserEvent", 
                 return_oid = FALSE)
  }
  
  # Add/Replace the Signature Image ---------------------------------
  if (nrow(signature_file) == 1){
    if (length(oid) == 0) oid <- OID$OID
    SignatureData <- data.frame(ParentUser = oid, 
                                FileName = tools::file_path_sans_ext(signature_file$name), 
                                FileExtension = tools::file_ext(signature_file$name), 
                                FileSize = signature_file$size, 
                                FileContent = I(list(readBin(signature_file$datapath, 
                                                             what = "raw", 
                                                             n = signature_file$size))))
    
    ThisUserSignature <- queryUserSignature(user_oid = oid)
    
    if (nrow(ThisUserSignature) > 0){
      updateRecord(data = SignatureData, 
                   table_name = "UserSignature", 
                   where_data = data.frame(OID = oid))
    } else {
      insertRecord(SignatureData, 
                   table_name = "UserSignature", 
                   return_oid = FALSE)
    }
  }
}

# Unexported --------------------------------------------------------

.addEditUser_editedEventList <- function(EventList, 
                                               oid,
                                               conn){
  EventList$ParentUser <- rep(oid, 
                                    nrow(EventList))
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisUser <- queryUser(oid)
  
  CurrentValue <- c(ThisUser$LastName, 
                    ThisUser$FirstName, 
                    ThisUser$LoginId, 
                    ThisUser$EmailAddress, 
                    ThisUser$IsInternal, 
                    ThisUser$IsActive)
  
  EventList[compareValues(CurrentValue, EventList$NewValue), ]
}
