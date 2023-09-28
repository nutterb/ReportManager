#' @name addEditDateReportingFormat
#' @title Add or Edit a DateReportingFormat Object
#' 
#' @description Adds a new DateReportingFormat object or edits an existing 
#'   DateReportingFormat objects in the database.
#'   
#' @param oid `integerish(0/1)`. The OID of the DateReportingFormat to be edited. 
#'   When the length is 0, a new DateReportingFormat will be added. 
#' @param format_name `character(1)`. The name of the reporting format. 
#' @param description `character(1)`. An option description of the format. Its
#'   recommended use is to give an example of what the format will look like
#'   when rendered in a report.
#' @param format_code `character(1)`. The code used to format the date/time.
#'   Use the formats described in [strptime()].
#' @param increment_start `integerish(1)`. An optional amount of time to 
#'   increment the starting time when reported. In almost all cases, 0 will be
#'   suitable.
#' @param increment_start_unit `character(0)`. The unit of time to apply to
#'   `increment_start`. One of `c("Second", "Minute", "Hour", "Day", "Week", "Month", "Year")`.
#' @param increment_end `integerish(1)`. An optional amount of time to 
#'   increment the ending time when report. See Details.
#' @param increment_end_unit `character(0)`. The unit of time to apply to
#'   `increment_end`.  One of `c("Second", "Minute", "Hour", "Day", "Week", "Month", "Year")`.
#' @param is_active `logical(1)`. When `TRUE`, the record will be marked as
#'   active.
#' @param event_user `integerish(1)`. the User.OID of the user performing 
#'   the action. 
#'   
#' @export

addEditDateReportingFormat <- function(oid = numeric(0), 
                                       format_name, 
                                       description, 
                                       format_code, 
                                       increment_start = 0, 
                                       increment_start_unit = "Second", 
                                       increment_end = 0, 
                                       increment_end_unit = "Second",
                                       is_active = TRUE, 
                                       event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertString(x = format_name, 
                          max.chars = 25, 
                          add = coll)
  
  checkmate::assertString(x = description, 
                          max.chars = 50, 
                          add = coll)
  
  checkmate::assertString(x = format_code, 
                          max.chars = 25, 
                          add = coll)
  
  checkmate::assertIntegerish(x = increment_start, 
                              len = 1, 
                              add = coll)
  
  increment_start_unit <- checkmate::matchArg(x = increment_start_unit, 
                                              choices = UNIT_OF_TIME, 
                                              .var.name = "increment_start_unit",
                                              add = coll)
  
  checkmate::assertIntegerish(x = increment_end, 
                              len = 1, 
                              add = coll)
  
  increment_end_unit <- checkmate::matchArg(x = increment_end_unit, 
                                            choices = UNIT_OF_TIME, 
                                            .var.name = "increment_end_unit",
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
  
  format_name <- trimws(format_name)
  description <- trimws(description)
  format_code <- trimws(format_code)
  
  event_time <- Sys.time()
  
  AddEditData <- data.frame(FormatName = format_name, 
                            Description = description, 
                            FormatCode = format_code, 
                            IncrementStart = increment_start, 
                            IncrementStartUnit = increment_start_unit, 
                            IncrementEnd = increment_end, 
                            IncrementEndUnit = increment_end_unit, 
                            IsActive = is_active, 
                            stringsAsFactors = FALSE)
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 7), 
               EventType = c("Add", 
                             if (is_active) "Activate" else "Deactivate",
                             "EditFormatName",
                             "EditFormatDescription", 
                             "EditFormatCode", 
                             "EditIncrementStart", 
                             "EditIncrementEnd"), 
               EventDateTime = rep(event_time, 7), 
               NewValue = c("", 
                            is_active, 
                            format_name,
                            description, 
                            format_code,
                            sprintf("%s %s", increment_start, increment_start_unit), 
                            sprintf("%s %s", increment_end, increment_end_unit)), 
               stringsAsFactors = FALSE)

  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "DateReportingFormat", 
                        return_oid = TRUE)
    
    EventList$ParentDateReportingFormat <- rep(OID$OID, 
                                               nrow(EventList))
  } else {
    EventList <- .addEditDateReportingFormat_editedEventList(EventList = EventList,
                                                             oid       = oid,
                                                             conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "DateReportingFormat")      
    }
  }
  
  if (nrow(EventList) > 0){
    insertRecord(EventList, 
                 table_name = "DateReportingFormatEvent", 
                 return_oid = FALSE)
  }
}


# Unexported --------------------------------------------------------

.addEditDateReportingFormat_editedEventList <- function(EventList, 
                                                        oid,
                                                        conn){
  EventList$ParentDateReportingFormat <- rep(oid, 
                                             nrow(EventList))
  
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisDateReportFormat <- queryDateReportingFormat(oid)
  
  CurrentValue <- c(ThisDateReportFormat$IsActive, 
                    ThisDateReportFormat$FormatName, 
                    ThisDateReportFormat$Description, 
                    ThisDateReportFormat$FormatCode,
                    sprintf("%s %s", ThisDateReportFormat$IncrementStart, ThisDateReportFormat$IncrementStartUnit), 
                    sprintf("%s %s", ThisDateReportFormat$IncrementEnd, ThisDateReportFormat$IncrementEndUnit))
  
  EventList[vapply(CurrentValue != EventList$NewValue, 
                   isTRUE, 
                   logical(1)), ]
}
