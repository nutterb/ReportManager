# Observe Event - rdo_schedule --------------------------------------

OE_rdo_schedule <- function(rv_Schedule, input){
  oid <- as.numeric(input$rdo_schedule)
  
  rv_Schedule$SelectedSchedule <- 
    rv_Schedule$Schedule[rv_Schedule$Schedule$OID == oid, ]
}

# Observe Event - btn_schedule_addSchedule --------------------------

OE_btn_schedule_addSchedule <- function(session, rv_Schedule){
  rv_Schedule$AddEdit <- "Add"
  
  updateTextInput(session = session, 
                  inputId = "txt_schedule_scheduleName", 
                  value = "")
  
  updateNumericInput(session = session, 
                     inputId = "num_schedule_frequency", 
                     value = 0)
  
  updateSelectInput(session = session, 
                    inputId = "sel_schedule_frequencyUnit", 
                    selected = "Day")
  
  updateNumericInput(session = session, 
                     inputId = "num_schedule_offset", 
                     value = 0)
  
  updateSelectInput(session = session, 
                    inputId = "sel_schedule_offsetUnit", 
                    selected = "Day")
  
  toggleModal(session = session, 
              modalId = "modal_schedule_addEdit", 
              toggle = "open")
}

# Observe Event - btn_schedule_editSchedule -------------------------

OE_btn_schedule_editSchedule <- function(session, rv_Schedule){
  rv_Schedule$AddEdit <- "Edit"
  
  updateTextInput(session = session, 
                  inputId = "txt_schedule_scheduleName", 
                  value = rv_Schedule$SelectedSchedule$ScheduleName)
  
  updateNumericInput(session = session, 
                     inputId = "num_schedule_frequency", 
                     value = rv_Schedule$SelectedSchedule$Frequency)
  
  updateSelectInput(session = session, 
                    inputId = "sel_schedule_frequencyUnit", 
                    selected = rv_Schedule$SelectedSchedule$FrequencyUnit)
  
  updateNumericInput(session = session, 
                     inputId = "num_schedule_offset", 
                     value = rv_Schedule$SelectedSchedule$OffsetOverlap)
  
  updateSelectInput(session = session, 
                    inputId = "sel_schedule_offsetUnit", 
                    selected = rv_Schedule$SelectedSchedule$OffsetOverlapUnit)
  
  toggleModal(session = session, 
              modalId = "modal_schedule_addEdit", 
              toggle = "open")
}

