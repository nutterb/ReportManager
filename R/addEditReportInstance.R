#' @name addEditReportInstance
#' @title Add and Edit ReportInstance Records
#' 
#' @description Provides the user with the tools to add new report instances
#'   or edit existing instances. 
#'   
#' @param report_instance_oid `integerish(0/1)`. The OID of the report 
#'   instance to be edited. When length 0, a new record is added.
#' @param parent_report_template `integerish(0/1)`. The OID of the parent
#'   report template. This will be ignored with the instance OID is known, 
#'   but is required when adding a new instance.
#' @param start_time `POSIXct(1)`. The start time for the reporting period.
#' @param end_time `POSIXct(1)`. The end time for the reporting period.
#' @param is_signature_required `logical(1)`. Indicates if signatures must 
#'   be given prior to submission.
#' @param is_scheduled `logical(1)`. Indicates if the instance is a scheduled
#'   instance by the template's schedule settings.
#' @param instance_title `character(1)`. The title of the instance.
#' @param is_submitted `character(1)`. Indicates if the instance has been
#'   submitted. 
#'   
#' @export

addEditReportInstance <- function(report_instance_oid = numeric(0), 
                                  parent_report_template = numeric(0), 
                                  start_time, 
                                  end_time, 
                                  is_signature_required, 
                                  is_scheduled, 
                                  instance_title, 
                                  is_submitted, 
                                  event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_template, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertPOSIXct(x = start_time, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertPOSIXct(x = end_time, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_signature_required, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_scheduled, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertCharacter(x = instance_title, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertLogical(is_submitted, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  instance_title <- trimws(instance_title)
  
  event_time <- Sys.time()
  
  AddEditData <- data.frame(ParentReportTemplate = parent_report_template, 
                            StartDateTime = format(start_time, 
                                                   format = "%Y-%m-%d %H:%M:%S"), 
                            EndDateTime = format(end_time, 
                                                 format = "%Y-%m-%d %H:%M:%S"), 
                            IsSignatureRequired = is_signature_required, 
                            IsScheduled = is_scheduled, 
                            InstanceTitle = instance_title, 
                            IsSubmitted = is_submitted)
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 7), 
               EventType = c("Add", 
                             "EditStartTime", 
                             "EditEndTime", 
                             "EditIsScheduled", 
                             "EditIsSignatureRequired", 
                             "EditIsSubmitted", 
                             "EditInstanceTitle"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 7), 
               NewValue = c("", 
                            format(start_time, 
                                   format = "%Y-%m-%d %H:%M:%S"), 
                            format(end_time, 
                                   format = "%Y-%m-%d %H:%M:%S"), 
                            is_scheduled, 
                            is_signature_required, 
                            is_submitted, 
                            instance_title))
  
  if (length(report_instance_oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "ReportInstance")
    EventList$ParentReportInstance <- rep(OID$OID, 
                                          nrow(EventList))
  } else {
    EventList <- .addEditReportInstance_editedEventList(EventList = EventList, 
                                                        oid = report_instance_oid, 
                                                        conn = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = report_instance_oid), 
                   table_name = "ReportInstance")
    }
  }
  
  if (nrow(EventList)){
    insertRecord(EventList, 
                 table_name = "ReportInstanceEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditReportInstance_editedEventList <- function(EventList, 
                                                   oid, 
                                                   conn){
  EventList$ParentReportInstance <- rep(oid, 
                                        nrow(EventList))
  EventList <- EventList[!EventList$EventType == "Add", ]
  
  ThisReportInstance <- queryReportInstance(oid)
  
  CurrentValue <- c(format(ThisReportInstance$StartDateTime), 
                    format(ThisReportInstance$EndDateTime), 
                    ThisReportInstance$IsScheduled,
                    ThisReportInstance$IsSignatureRequired, 
                    ThisReportInstance$IsSubmitted, 
                    ThisReportInstance$InstanceTitle)
  
  if (length(CurrentValue) > 0){
    EventList[compareValues(CurrentValue, EventList$NewValue), ]
  } else {
    EventList
  }
}
