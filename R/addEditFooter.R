#' @name addEditFooter
#' @title Add or Edit Footers
#' 
#' @description Add a new dooter to the database, or edit an existing 
#'   footer.
#'   
#' @param oid `integerish(0/1)`. The OID of the Footer object to be 
#'   edited. When length is 0, a new Footer object is added.
#' @param footer `character(1)`. The text of the footer. Limited to
#'   200 characters.
#' @param is_active `logical(1)`. When `TRUE`, the footer is marked
#'   as active. 
#' @param event_user `integerish(1)`. The OID of the user performing the 
#'   action.
#'   
#' @export

addEditFooter <- function(oid = numeric(0), 
                          footer, 
                          is_active = TRUE, 
                          event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertString(x = footer, 
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
  
  footer <- trimws(footer)
  
  event_time <- Sys.time()
  
  AddEditData <- data.frame(Footer = footer, 
                            IsActive = as.numeric(is_active))
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 3), 
               EventType = c("Add", 
                             "EditFooter", 
                             if (is_active) "Activate" else "Deactivate"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 3), 
               NewValue = c("", 
                            footer,
                            is_active))
  
  if (length(oid) == 0){
    
    OID <- insertRecord(AddEditData, 
                        table_name = "Footer")
    
    EventList$ParentFooter<- rep(OID$OID, 
                                 nrow(EventList))
  } else {
    EventList <- .addEditFooter_editedEventList(EventList = EventList,
                                                oid       = oid,
                                                conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "Footer")
    }
  }
  
  if (nrow(EventList)){
    insertRecord(EventList, 
                 table_name = "FooterEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditFooter_editedEventList <- function(EventList, 
                                           oid,
                                           conn){
  EventList$ParentFooter <- rep(oid, 
                                nrow(EventList))
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisFooter <- queryFooter(oid)
  
  CurrentValue <- c(ThisFooter$Footer, 
                    ThisFooter$IsActive)
  
  EventList[compareValues(CurrentValue, EventList$NewValue), ]
}
