..btn_schedule_editSchedule <- function(session, rv_Schedule){
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
