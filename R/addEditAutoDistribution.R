#' @name addEditAutoDistribution
#' @title Add or Edit Auto Distribution Configurations
#'
#' @description Provides the user with the tools to add or modify the
#'   automatic distribution configuration of report templates.
#'   
#' @param oid `integerish(0/1)`. The OID of the AutoDistribution object 
#'   being edited. If a zero-length value, a new object is created.
#' @param parent_report_template `integerish(1)`. The OID of the report
#'   template.
#' @param parent_schedule `integerish(1)`. The OID of the reporting schedule.
#' @param start_date_time `POSIXct(1)`. The date and time for the
#'   report template to begin distribution.
#' @param is_active `logical(1)`. When `TRUE`, the configuration is active
#'   and distributes. When `FALSE`, distribution is skipped.
#' @param delay_after_instance_end `integerish(1)`. The period of time to 
#'   wait after an instance completes to attempt distribution. Delays may 
#'   be necessary to accomodate data processing tasks that must be completed
#'   before the report is valid.
#' @param delay_units `character(1)`. The unit of time for the delay. One 
#'   of `c("Second", "Minute", "Hour", "Day", "Week", "Month", "Year")`
#' @param current_or_last_instance `character(1)`. One of 
#'   `c("Current" or "LastCompleted")`.
#' @param is_add_to_archive `logical(1)`. When `TRUE`, automatically 
#'   distributed reports are also added to the archive. 
#' @param report_format `character(1)`. One of `c("html", "pdf")`. Determines
#'   the file format for the report attached to the e-mail.
#' @param is_distribute_internal_only `logical(1)`. When `TRUE`, report is
#'   only distributed to internal users. Set to `FALSE` in order to include
#'   external users as well.
#' @param is_embed_html `logical(1)`. Only applicable when 
#'   `report_format = "html"`. When `TRUE`, HTML reports are embedded into 
#'   the e-mail. When `FALSE`, they are attached as a file.
#'   
#' @export

addEditAutoDistribution <- function(oid = numeric(0), 
                                    parent_report_template, 
                                    parent_schedule, 
                                    start_date_time, 
                                    is_active, 
                                    delay_after_instance_end, 
                                    delay_units = "Hour", 
                                    current_or_last_instance = c("Current", "LastCompleted"), 
                                    is_add_to_archive = FALSE, 
                                    report_format = c("html", "pdf"), 
                                    is_distribute_internal_only = TRUE, 
                                    is_embed_html = TRUE, 
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
  
  checkmate::assertPOSIXct(x = start_date_time, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_active, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = delay_after_instance_end, 
                              len = 1, 
                              add = coll)
  
  delay_units <- checkmate::matchArg(x = delay_units, 
                                     choices = UNIT_OF_TIME, 
                                     .var.name = "delay_units", 
                                     add = coll)
  
  current_or_last_instance <- checkmate::matchArg(x = current_or_last_instance, 
                                                  choices = c("Current", "LastCompleted"), 
                                                  .var.name = "current_or_last_instance", 
                                                  add = coll)
  
  checkmate::assertLogical(x = is_add_to_archive, 
                           len = 1, 
                           add = coll)
  
  report_format <- checkmate::matchArg(x = report_format, 
                                       choices = c("html", "pdf"), 
                                       .var.name = "report_format", 
                                       add = coll)
  
  checkmate::assertLogical(x = is_distribute_internal_only, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_embed_html, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  AddEditData <- data.frame(ParentReportTemplate = parent_report_template, 
                            ParentSchedule = parent_schedule, 
                            StartDateTime = start_date_time, 
                            IsActive = is_active, 
                            DelayAfterInstanceEnd = delay_after_instance_end, 
                            DelayUnits = delay_units, 
                            CurrentOrLastInstance = current_or_last_instance, 
                            IsAddToArchive = is_add_to_archive, 
                            ReportFormat = report_format, 
                            IsDistributeInternalOnly = is_distribute_internal_only, 
                            IsEmbedHtml = is_embed_html)
  
  event_time <- Sys.time()
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 11), 
               EventType = c("Add", 
                             if (is_active) "Activate" else "Deactivate", 
                             "EditSchedule", 
                             "EditStartDateTime", 
                             "EditDelayAfterInstanceEnd", 
                             "EditDelayUnits", 
                             "EditCurrentOrLastInstance", 
                             "EditIsAddToArchive", 
                             "EditReportFormat", 
                             "EditIsDistributeInternalOnly", 
                             "EditIsEmbedHtml"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 11), 
               NewValue = c("", 
                            is_active, 
                            parent_schedule, 
                            format(start_date_time, 
                                   format = "%Y-%m-%d %H:%M:%S"), 
                            delay_after_instance_end, 
                            delay_units, 
                            current_or_last_instance, 
                            is_add_to_archive, 
                            report_format, 
                            is_distribute_internal_only, 
                            is_embed_html), 
               stringsAsFactors = FALSE)
  
  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "AutoDistribution", 
                        return_oid = TRUE)
    
    EventList$ParentAutoDistribution <- rep(OID$OID, 
                                            nrow(EventList))
  } else {
    EventList <- .addEditAutoDistribution_editedEventList(EventList = EventList,
                                                          oid       = oid,
                                                          conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "AutoDistribution")      
    }
  }
  
  if (nrow(EventList) > 0){
    insertRecord(EventList, 
                 table_name = "AutoDistributionEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditAutoDistribution_editedEventList <- function(EventList, 
                                                               oid,
                                                               conn){
  EventList$ParentAutoDistribution <- rep(oid, 
                                          nrow(EventList))
  
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisAD <- queryAutoDistribution(oid)
  
  CurrentValue <- c(ThisAD$IsActive, 
                    ThisAD$ParentSchedule, 
                    ThisAD$StartDateTime, 
                    ThisAD$DelayAfterInstanceEnd, 
                    ThisAD$DelayUnits, 
                    ThisAD$CurrentOrLastInstance, 
                    ThisAD$IsAddToArchive, 
                    ThisAD$ReportFormat, 
                    ThisAD$IsDistributeInternalOnly, 
                    ThisAD$IsEmbedHtml)
  
  EventList[compareValues(CurrentValue, EventList$NewValue), ]
}

