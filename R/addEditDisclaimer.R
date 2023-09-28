#' @name addEditDisclaimer
#' @title Add or Edit Disclaimers
#' 
#' @description Add a new disclaimer to the database, or edit an existing 
#'   disclaimer.
#'   
#' @param oid `integerish(0/1)`. The OID of the Disclaimer object to be 
#'   edited. When length is 0, a new Disclaimer object is added.
#' @param disclaimer `character(1)`. The text of the disclaimer. Limited to
#'   2000 characters.
#' @param is_active `logical(1)`. When `TRUE`, the disclaimer is marked
#'   as active. 
#' @param event_user `integerish(1)`. The OID of the user performing the 
#'   action.
#'   
#' @export

addEditDisclaimer <- function(oid = numeric(0), 
                              disclaimer, 
                              is_active = TRUE, 
                              event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertString(x = disclaimer, 
                          max.chars = 2000, 
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
  
  disclaimer <- trimws(disclaimer)
  
  event_time <- Sys.time()
  
  AddEditData <- data.frame(Disclaimer = disclaimer, 
                            IsActive = as.numeric(is_active))
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 3), 
               EventType = c("Add", 
                             "EditDisclaimer", 
                             if (is_active) "Activate" else "Deactivate"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 3), 
               NewValue = c("", 
                            disclaimer,
                            is_active))
  
  if (length(oid) == 0){
    
    OID <- insertRecord(AddEditData, 
                        table_name = "Disclaimer")
    
    EventList$ParentDisclaimer <- rep(OID$OID, 
                                      nrow(EventList))
  } else {
    EventList <- .addEditDisclaimer_editedEventList(EventList = EventList,
                                                    oid       = oid,
                                                    conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "Disclaimer")
    }
  }
  
  if (nrow(EventList)){
    insertRecord(EventList, 
                 table_name = "DisclaimerEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditDisclaimer_editedEventList <- function(EventList, 
                                               oid,
                                               conn){
  EventList$ParentDisclaimer <- rep(oid, 
                                    nrow(EventList))
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisDisclaimer <- queryDisclaimer(oid)
  
  CurrentValue <- c(ThisDisclaimer$Disclaimer, 
                    ThisDisclaimer$IsActive)
  
  EventList[CurrentValue != EventList$NewValue, ]
}
