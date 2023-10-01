#' @name addEditReportTemplateDisclaimer
#' @title Add or Edit ReportTemplateDisclaimer Objects
#' 
#' @description Enables the user to add ReportTemplateDisclaimer objects 
#'   or edit an existing ReportTemplateDisclaimer object. Also manages the
#'   recording of events associated with the action. 
#'   
#' @param oid `integerish(0/1)`. The OID of the ReportTemplateDisclaimer 
#'   object to be edited. Use `numeric(0)` to add a new object.
#' @param parent_report_template `integerish(1)`. The OID of the ReportTemplate
#'   object being associated with a disclaimer. 
#' @param parent_disclaimer `integerish(1)`. The OID of the Disclaimer
#'   object being associated with the ReportTemplate.
#' @param is_active `logical(1)`. When `TRUE`, the association will be marked
#'   as active. 
#' @param event_user `integerish(1)`. The OID of the User performing the action.
#' 
#' @export

addEditReportTemplateDisclaimer <- function(oid = numeric(0), 
                                            parent_report_template, 
                                            parent_disclaimer, 
                                            is_active = TRUE, 
                                            event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_template, 
                              len = 1,
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_disclaimer, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = is_active, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (length(oid) == 0){
    CurrentReportTemplateDisclaimer <- 
      queryReportTemplateDisclaimer(parent_report_template = parent_report_template, 
                                    parent_disclaimer = parent_disclaimer)
    
    if (nrow(CurrentReportTemplateDisclaimer) > 0){
      coll$push(sprintf("A ReportTemplateDisclaimer record for ReportTemplate.OID = %s and Disclaimer.OID = %s already exists. Edit the exisitng record instead of adding another", 
                        parent_report_template, 
                        parent_disclaimer))
    }
  }
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------

  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  AddEditData <- data.frame(ParentReportTemplate = parent_report_template, 
                            ParentDisclaimer = parent_disclaimer, 
                            IsActive   = is_active)
  
  event_time <- Sys.time()
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 2), 
               EventType = c("Add", 
                             if (is_active) "Activate" else "Deactivate"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 2), 
               NewValue = c("", 
                            is_active), 
               stringsAsFactors = FALSE)
  
  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "ReportTemplateDisclaimer", 
                        return_oid = TRUE)
    
    EventList$ParentReportTemplateDisclaimer <- rep(OID$OID, 
                                                      nrow(EventList))
  } else {
    EventList <- .addEditReportTemplateDisclaimer_editedEventList(EventList = EventList,
                                                                    oid       = oid,
                                                                    conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "ReportTemplateDisclaimer")      
    }
  }
  
  if (nrow(EventList) > 0){
    insertRecord(EventList, 
                 table_name = "ReportTemplateDisclaimerEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditReportTemplateDisclaimer_editedEventList <- function(EventList, 
                                                               oid,
                                                               conn){
  EventList$ParentReportTemplateDisclaimer <- rep(oid, 
                                                    nrow(EventList))
  
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisRTD <- queryReportTemplateDisclaimer(oid)
  
  CurrentValue <- c(ThisRTD$IsActive)
  
  EventList[compareValues(CurrentValue, EventList$NewValue), ]
}
