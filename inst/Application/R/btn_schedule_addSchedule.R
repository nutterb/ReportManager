..btn_schedule_addSchedule <- function(session, rv_Schedule){
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
