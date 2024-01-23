#' @name addEditSchedule
#' @title Add or Edit a Schedule Object
#' 
#' @description Adds a new schedule object or edits an existing schedule
#'   objects in the database.
#'   
#' @param oid `integerish(0/1)`. The OID of the schedule to be edited. 
#'   When the length is 0, a new schedule will be added. 
#' @param schedule_name `character(1)`. The name of the schedule. 
#' @param frequency `integerish(1)`. The frequency at which the schedule repeats.
#' @param frequency_unit `character(1)`. The units associated with the frequency.
#'   One of `c("Minute", "Hour", "Day", "Week", "Month", "Year")`.
#' @param offset_overlap `integerish(1)`. The duration of scheduling overlap. 
#'   This allows for schedules with overlapping reporting periods. 
#' @param offset_overlap_unit `character(1)`. The units associated with the
#'   offset overlap. One of `c("None", "Minute", "Hour", "Day", "Week", "Month", "Year")`.
#' @param is_active `logical(1)`. When `TRUE`, the schedule is marked as 
#'   active. 
#' @param is_period_to_date `logical(1)`. When `TRUE`, the schedule will take
#'   the form of a "period-to-date" (year-to-date, for example). These schedules
#'   may have multiple instances with the same start date but end dates that
#'   progress throughout the periodicity.
#' @param event_user `integerish(1)`. the User.OID of the user performing 
#'   the action. 
#'   
#' @export

addEditSchedule <- function(oid = numeric(0), 
                            schedule_name, 
                            frequency, 
                            frequency_unit, 
                            offset_overlap = 0, 
                            offset_overlap_unit = "Day", 
                            is_active = TRUE, 
                            is_period_to_date = FALSE,
                            event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertString(x = schedule_name, 
                          max.chars = 50, 
                          add = coll)
  
  checkmate::assertIntegerish(x = frequency, 
                              len = 1, 
                              add = coll)
  
  frequency_unit <- checkmate::matchArg(x = frequency_unit, 
                                        choices = UNIT_OF_TIME, 
                                        .var.name = "frequency_unit",
                                        add = coll)
  
  checkmate::assertIntegerish(x = offset_overlap, 
                              len = 1, 
                              add = coll)
  
  offset_overlap_unit <- checkmate::matchArg(x = offset_overlap_unit, 
                                             choices = UNIT_OF_TIME_WITH_NONE, 
                                             .var.name = "offset_overlap_unit",
                                             add = coll)
  
  checkmate::assertLogical(x = is_active, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_period_to_date, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  schedule_name <- trimws(schedule_name)
  
  event_time <- Sys.time()
 
  AddEditData <- data.frame(ScheduleName = schedule_name, 
                            Frequency = frequency, 
                            FrequencyUnit = frequency_unit, 
                            OffsetOverlap = offset_overlap, 
                            OffsetOverlapUnit = offset_overlap_unit, 
                            IsActive = as.numeric(is_active), 
                            IsPeriodToDate = as.numeric(is_period_to_date),
                            stringsAsFactors = FALSE)
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 6), 
               EventType = c("Add", 
                             if (is_active) "Activate" else "Deactivate",
                             if (is_period_to_date) "SetIsPeriodToDateTrue" else "SetIsPeriodToDateFalse",
                             "EditScheduleName", 
                             "EditFrequency", 
                             "EditOverlap"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 6), 
               NewValue = c("", 
                            is_active, 
                            is_period_to_date,
                            schedule_name, 
                            sprintf("%s %s", frequency, frequency_unit), 
                            sprintf("%s %s", offset_overlap, offset_overlap_unit)), 
               stringsAsFactors = FALSE)
  
  if (length(oid) == 0){

    OID <- insertRecord(AddEditData, 
                        table_name = "Schedule", 
                        return_oid = TRUE)

    EventList$ParentSchedule <- rep(OID$OID, 
                                    nrow(EventList))
  } else {
    EventList <- .addEditSchedule_editedEventList(EventList = EventList,
                                                  oid       = oid,
                                                  conn      = conn)

    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "Schedule")      
    }
  }
  
  if (nrow(EventList) > 0){
    insertRecord(EventList, 
                 table_name = "ScheduleEvent", 
                 return_oid = FALSE)
  }
}


# Unexported --------------------------------------------------------

.addEditSchedule_editedEventList <- function(EventList, 
                                             oid,
                                             conn){
  EventList$ParentSchedule <- rep(oid, 
                              nrow(EventList))
  
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisSchedule <- querySchedule(oid)
  
  CurrentValue <- c(ThisSchedule$IsActive,
                    ThisSchedule$IsPeriodToDate,
                    ThisSchedule$ScheduleName, 
                    sprintf("%s %s", ThisSchedule$Frequency, ThisSchedule$FrequencyUnit), 
                    sprintf("%s %s", ThisSchedule$OffsetOverlap, ThisSchedule$OffsetOverlapUnit))

  EventList[compareValues(CurrentValue, EventList$NewValue), ]
}
