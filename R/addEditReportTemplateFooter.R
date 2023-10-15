#' @name addEditReportTemplateFooter
#' @title Add or Edit ReportTemplateFooter Objects
#' 
#' @description Enables the user to add ReportTemplateFooter objects 
#'   or edit an existing ReportTemplateFooter object. Also manages the
#'   recording of events associated with the action. 
#'   
#' @param oid `integerish(0/1)`. The OID of the ReportTemplateFooter 
#'   object to be edited. Use `numeric(0)` to add a new object.
#' @param parent_report_template `integerish(1)`. The OID of the ReportTemplate
#'   object being associated with a disclaimer. 
#' @param parent_footer `integerish(1)`. The OID of the Footer
#'   object being associated with the ReportTemplate.
#' @param order `integerish(1)`. The ordered position in which the 
#'   footer is displayed on the report.
#' @param is_active `logical(1)`. When `TRUE`, the association will be marked
#'   as active. 
#' @param event_user `integerish(1)`. The OID of the User performing the action.
#' 
#' @export

addEditReportTemplateFooter <- function(oid = numeric(0), 
                                        parent_report_template, 
                                        parent_footer,
                                        order,
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
  
  checkmate::assertIntegerish(x = parent_footer, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = order, 
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
    CurrentReportTemplateFooter <- 
      queryReportTemplateFooter(parent_report_template = parent_report_template, 
                                parent_footer = parent_footer)
    
    if (nrow(CurrentReportTemplateFooter) > 0){
      coll$push(sprintf("A ReportTemplateFooter record for ReportTemplate.OID = %s and Footer.OID = %s already exists. Edit the exisitng record instead of adding another", 
                        parent_report_template, 
                        parent_footer))
    }
  }
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------

  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  AddEditData <- data.frame(ParentReportTemplate = parent_report_template, 
                            ParentFooter = parent_footer,
                            Order = order,
                            IsActive   = is_active)
  
  event_time <- Sys.time()
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 3), 
               EventType = c("Add", 
                             "Reorder",
                             if (is_active) "Activate" else "Deactivate"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 3), 
               NewValue = c("", 
                            order,
                            is_active), 
               stringsAsFactors = FALSE)
  
  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "ReportTemplateFooter", 
                        return_oid = TRUE)
    
    EventList$ParentReportTemplateFooter <- rep(OID$OID, 
                                                nrow(EventList))
  } else {
    EventList <- .addEditReportTemplateFooter_editedEventList(EventList = EventList,
                                                              oid       = oid,
                                                              conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "ReportTemplateFooter")      
    }
  }
  
  if (nrow(EventList) > 0){
    insertRecord(EventList, 
                 table_name = "ReportTemplateFooterEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditReportTemplateFooter_editedEventList <- function(EventList, 
                                                               oid,
                                                               conn){
  EventList$ParentReportTemplateFooter <- rep(oid, 
                                                    nrow(EventList))
  
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisRTF <- queryReportTemplateFooter(oid)
  
  CurrentValue <- c(ThisRTF$Order,
                    ThisRTF$IsActive)
  
  EventList[compareValues(CurrentValue, EventList$NewValue), ]
}
