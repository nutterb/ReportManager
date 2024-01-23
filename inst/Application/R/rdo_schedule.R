..rdo_schedule <- function(rv_Schedule, input){
  oid <- as.numeric(input$rdo_schedule)
  rv_Schedule$SelectedSchedule <- 
    rv_Schedule$Schedule[rv_Schedule$Schedule$OID == oid, ]
}
