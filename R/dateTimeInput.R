#*********************************************************************
#* File Name:     dateTimeInput.R
#* Author:        Benjamin Nutter
#* Copyright:     Battelle Memorial Institute
#*                This software is property of the U.S. Government
#*
#*                Change log located at bottom of file
#*********************************************************************
#' @name dateTimeInput
#' @title Date/Time Input for Shiny Applications
#'
#' @description Draws a date/time input for a shiny application.
#'
#' @param inputId \code{character(1)} The name for the input object.
#' @param label \code{character(1)} The label to display.
#' @param start \code{POSIXct(1)} The start date/time for the control. If
#'   \code{NULL}, the current date at midnight is used.
#' @param end \code{POSIXct(1)} The end date/time for the control. If
#'   \code{NULL}, the next day at midnight is used.
#' @param single_date \code{logical(1)} When \code{FALSE}, two date/times
#'   are input.  When \code{TRUE}, only a single date/time is input.
#' @param timepicker \code{logical(1)} Use \code{FALSE} to suppress the
#'   time selection (although, at this point, you may as well use
#'   \code{dateInput}).
#' @param ... Additional arguments to pass to \code{tags$input}
#'
#' @author Benjamin Nutter
#'
#' @source \url{http://www.daterangepicker.com/#example2}
#'
#' @export

dateTimeInput <- function(inputId,
                          label,
                          start = NULL,
                          end = NULL,
                          single_date = TRUE,
                          timepicker = TRUE,
                          ...){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = inputId,
                              len = 1,
                              add = coll)
  
  checkmate::assert_character(x = label,
                              len = 1,
                              add = coll)
  
  if (!is.null(start) & !inherits(start, "POSIXct")){
    coll$push("`start` must be a POSIXct object")
  }
  
  if (!is.null(end) & !inherits(end, "POSIXct")){
    coll$push("`end` must be a POSIXct object")
  }
  
  checkmate::assert_logical(x = single_date,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = timepicker,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (is.null(start)){
    start <- as.POSIXct(format(Sys.Date(),
                               format = "%Y-%m-%d"),
                        tz = "UTC")
  }
  
  if (is.null(end)){
    end <- as.POSIXct(format(Sys.Date() + 1,
                             format = "%Y-%m-%d"),
                      tz = "UTC")
  }
  
  shiny::tagList(
              shiny::p(label, style = "font-weight:bold"),
              shiny::tags$input(type = "text", name = inputId, id = inputId, ...),
              shiny::tags$script(paste0("$(function() {
                              $('input[name=\"", inputId, "\"]').daterangepicker({
                              showDropdowns: true, 
                              singleDatePicker: ", tolower(single_date), ",
                              timePicker: ", tolower(timepicker), ",
                              startDate: '", format(start, format = "%d-%b-%Y %H:%M"), "',
                              endDate: '", format(end, format = "%d-%b-%Y %H:%M"), "',
                              locale: {
                              format: 'DD-MMM-YYYY HH:mm'
                              }
});
                              });"))
  )
}

#' @rdname dateTimeInput
#' @export

useDateTimeInput <- function(){
  shiny::tagList(
    shiny::tags$script(type = "text/javascript",
                       src = system.file("appUtilities/moment.min.js",
                                         package = "bgcappShiny")),
    shiny::tags$script(type = "text/javascript",
                       src = system.file("appUtilities/daterangepicker.min.js",
                                         package = "bgcappShiny")),
    shiny::tags$link(rel = "stylesheet",
                     type = "text/css",
                     href = system.file("appUtilities/daterangepicker.css",
                                        package = "bgcappShiny"))
  )
}

#*********************************************************************
#* Changes:
#*
#* DATE        USER     DESCRIPTION
#*
#*********************************************************************
