#' @name addEditReportTemplateSchedule
#' @title Add or Edit ReportTemplateSchedule Objects
#' 
#' @description Enables the user to add ReportTemplateSchedule objects 
#'   or edit an existing ReportTemplateSchedule object. Also manages the
#'   recording of events associated with the action. 
#'   
#' @param oid `integerish(0/1)`. The OID of the ReportTemplateSchedule 
#'   object to be edited. Use `numeric(0)` to add a new object.
#' @param parent_report_template `integerish(1)`. The OID of the ReportTemplate
#'   object being associated with a disclaimer. 
#' @param parent_schedule `integerish(1)`. The OID of the Schedule
#'   object being associated with the ReportTemplate.
#' @param start_date `POSIXct(1)`. The date/time for start of the the 
#'   first instance of the report.
#' @param is_active `logical(1)`. When `TRUE`, the association will be marked
#'   as active. 
#' @param event_user `integerish(1)`. The OID of the User performing the action.
#' 
#' @export

addEditReportTemplateSchedule <- function(oid = numeric(0), 
                                          parent_report_template, 
                                          parent_schedule,
                                          start_date,
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
  
  checkmate::assertIntegerish(x = parent_schedule, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertPOSIXct(x = start_date, 
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
    CurrentReportTemplateSchedule <- 
      queryReportTemplateSchedule(parent_report_template = parent_report_template, 
                                  parent_schedule = parent_schedule)
    
    if (nrow(CurrentReportTemplateSchedule) > 0){
      coll$push(sprintf("A ReportTemplateSchedule record for ReportTemplate.OID = %s and Schedule.OID = %s already exists. Edit the exisitng record instead of adding another", 
                        parent_report_template, 
                        parent_schedule))
    }
  }
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------

  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  AddEditData <- data.frame(ParentReportTemplate = parent_report_template, 
                            ParentSchedule = parent_schedule,
                            StartDateTime = format(start_date, 
                                                   format = "%Y-%m-%d %H:%M:%S"),
                            IsActive   = is_active)
  
  event_time <- Sys.time()
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 3), 
               EventType = c("Add", 
                             "EditStartDate",
                             if (is_active) "Activate" else "Deactivate"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 3), 
               NewValue = c("", 
                            format(start_date, format = "%Y-%M-%d %H:%M:%S"),
                            is_active), 
               stringsAsFactors = FALSE)
  
  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "ReportTemplateSchedule", 
                        return_oid = TRUE)
    
    EventList$ParentReportTemplateSchedule <- rep(OID$OID, 
                                                  nrow(EventList))
  } else {
    EventList <- .addEditReportTemplateSchedule_editedEventList(EventList = EventList,
                                                                oid       = oid,
                                                                conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "ReportTemplateSchedule")      
    }
  }
  
  if (nrow(EventList) > 0){
    insertRecord(EventList, 
                 table_name = "ReportTemplateScheduleEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditReportTemplateSchedule_editedEventList <- function(EventList, 
                                                           oid,
                                                           conn){
  EventList$ParentReportTemplateSchedule <- rep(oid, 
                                                nrow(EventList))
  
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisRTS <- queryReportTemplateSchedule(oid)

  CurrentValue <- c(format(ThisRTS$StartDateTime,
                           format = "%Y-%M-%d %H:%M:%S"),
                    ThisRTS$IsActive)

  EventList[compareValues(CurrentValue, EventList$NewValue), ]
}
