#' @name makeReportInstanceSchedule
#' @title Make the Schedule of Report Instances for a Template
#' 
#' @description Generate the expected schedule of report instances for 
#'   a reporting template based on its scheduling properties.
#'   
#' @param frequency `integerish(1)`. The number of time units to 
#'   increment between instances.
#' @param frequency_unit `character(1)` One of `c("minute", "hour", "day",
#'  "week", "month" or "year")`. 
#' @param start_date `POSIXct(1)`. The start date for the schedule.
#' @param offset `integerish(1)` The offset value for overlapping reports.
#' @param offset_unit `character(1)`. The unit of time in which the `offset`
#'   is calculated.
#' @param end_date `POSIXct(1)`. The time to end the report scheduling.
#' @param is_offset `logical(1)`. When `TRUE`, the standard scheduling algorithm
#'   is used. When `FALSE`, the period-to-date algorithm is used.
#' @param `index_date` `POSIXct(1)`. The index date for a period-to-date
#'   format schedule.
#'   
#' @details Two primary types of schedules are considered in this function; 
#'   The Standard scheduling algorithm and the Period-to-date algorithm. 
#'   Each of these may be scheduled with an offset to allow for overlapping 
#'   reports. 
#'   
#'   The Standard algorithm with a zero-offset calculates the schedule by 
#'   adding the frequency of the schedule to the start date until it 
#'   reaches the end date. The end date of each instance is then determined
#'   by adding the frequency to the start date of the instance.
#'   
#'   The Standard algorithm with a non-zero offset calculate the schedule by
#'   adding the offset of the schedule until it reaches the end date. 
#'   The end date of each instance is then determined
#'   by adding the frequency to the start date of the instance.
#'   
#'   The Period-to-Date algorithm first develops the schedule as determined in
#'   the previous paragraphs. The start date is then altered to the most 
#'   recent anniversary of the index date. For example, if the report has an 
#'   index date of January 1 and is a year-to-date report, the start date will 
#'   be set to the most recent occurrence of January 1. (Similarly, if the report
#'   is month to date, it will set the start date to the first of the current
#'   month).
#'   
#' @return Returns a data frame with the columns `StartDate` and `EndDate`.
#'   
#' @export

makeReportInstanceSchedule <- function(frequency,
                                       frequency_unit,
                                       offset,
                                       offset_unit,
                                       schedule_type, 
                                       start_date,
                                       end_date = Sys.time(),
                                       is_offset, 
                                       index_date){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = frequency,
                              len = 1,
                              add = coll)
  
  frequency_unit <- checkmate::matchArg(x = tolower(frequency_unit),
                                        choices = c("minute", "hour", "day",
                                                    "week", "month", "year"),
                                        .var.name = "frequency_unit",
                                        add = coll)
  
  checkmate::assertPOSIXct(x = start_date, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = offset,
                              len = 1,
                              add = coll)
  
  offset_unit <- checkmate::matchArg(x = tolower(offset_unit),
                                     choices = c("none", "
                                                 minute", "hour", "day",
                                                 "week", "month", "year"),
                                     .var.name = "offset_unit",
                                     add = coll)
  
  checkmate::assertPOSIXct(x = end_date, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertPOSIXct(x = index_date, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertCharacter(x = schedule_type,
                             len = 1,
                             add = coll)
  
  checkmate::assertLogical(x = is_offset, 
                           len = 1, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Return empty data frame for a "None" Schedule
  
  if (tolower(schedule_type) == "none"){
    return(data.frame(StartDate = as.Date(character(0)),
                      EndDate = as.Date(character(0)),
                      stringsAsFactors = FALSE))
  }

  # Standardize the appearance of frequency unit to what R routines expect
  frequency_unit <- tolower(frequency_unit)
  if (frequency_unit == "minute") frequency_unit <- "min"

  # Start date of each instance     
  start <- seq(from  = start_date,
               to = end_date,
               by = sprintf("%s %s", frequency, frequency_unit))

  # Set the by frequency depending on the type of schedule
  by_freq <- if (offset_unit == "none" | !is_offset) frequency else offset
  by_unit <- if (offset_unit == "none" | !is_offset) frequency_unit else offset_unit
  
  # Get the Schedule
  Schedule <- .makeReportInstanceSchedule_scheduleFrame(start, 
                                                        start_date,
                                                        by_freq, 
                                                        by_unit)
  
  # Adjust [Period]-to-date schedule start dates
  if (!is_offset){
    Schedule <- .makeReportInstanceSchedule_periodToDate(Schedule, 
                                                         index_date, 
                                                         end_date, 
                                                         offset, 
                                                         offset_unit)
  }
  
  Schedule
}

# Unexported --------------------------------------------------------

.makeReportInstanceSchedule_scheduleFrame <- function(start,
                                                      start_date,
                                                      by_freq, 
                                                      by_unit){
  # For each start date, get the end date
  # This returns a list of vectors
  Schedule <- lapply(start,
                     seq,
                     length.out = 2,
                     by = sprintf("%s %s",
                                  by_freq, tolower(by_unit)))
  
  # Stack the list of vectors into a matrix, then data frame
  Schedule <- do.call("rbind", Schedule)
  Schedule <- as.data.frame(Schedule)
  
  # Restore the dates to POSIXct class
  Schedule <- lapply(Schedule,
                     as.POSIXct,
                     origin = as.POSIXct("1970-01-01 00:00:00",
                                         tz = "UTC"),
                     tz = "UTC")
  Schedule <- as.data.frame(Schedule)
  Schedule <- stats::setNames(Schedule, c("StartDate", "EndDate"))
  
  # Filter out extraneous schedule lines
  Schedule <- Schedule[!is.na(Schedule$StartDate), ]
  Schedule <- Schedule[Schedule$StartDate >= start_date, ]
  rownames(Schedule) <- NULL
  Schedule
}

.makeReportInstanceSchedule_periodToDate <- function(Schedule, 
                                                     index_date, 
                                                     end_date, 
                                                     offset, 
                                                     offset_unit){
  index_start <- seq(index_date, 
                     end_date, 
                     by = sprintf("%s %s", offset, offset_unit))
  
  Schedule$StartDate <- 
    index_start[vapply(Schedule$StartDate, 
                       function(x) max(which(index_start <= x)), 
                       numeric(1))]
  
  Schedule
}
