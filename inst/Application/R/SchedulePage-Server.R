# Schedules ---------------------------------------------------------
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

# Observe Event - btn_schedule_addEditSchedule ----------------------

OE_btn_schedule_addEditSchedule <- function(session, 
                                            rv_Schedule, 
                                            input, 
                                            current_user_oid, 
                                            proxy){
  oid <- if(rv_Schedule$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_schedule)
  
  val <- validateScheduleInputs(rv_Schedule = rv_Schedule, 
                                input = input, 
                                is_edit = rv_Schedule$AddEdit == "Edit", 
                                this_schedule_name = rv_Schedule$SelectedSchedule$ScheduleName)
  
  if (!val$is_ok()){
    alert(val$report())
  } else {
    addEditSchedule(oid = oid, 
                    schedule_name = input$txt_schedule_scheduleName, 
                    frequency = input$num_schedule_frequency, 
                    frequency_unit = input$sel_schedule_frequencyUnit, 
                    offset_overlap = input$num_schedule_offset, 
                    offset_overlap_unit = input$sel_schedule_offsetUnit, 
                    is_active = input$chk_schedule_isActive, 
                    event_user = current_user_oid)
    
    RM_replaceData(query_fun = querySchedule, 
                   reactive_list = rv_Schedule, 
                   data_slot = "Schedule", 
                   selected_slot = "SelectedSchedule", 
                   id_variable = "OID", 
                   element_name = "rdo_user", 
                   oid = oid, 
                   proxy = proxy)
    
    toggleModal(session = session, 
                modalId = "modal_schedule_addEdit", 
                toggle = "close")
  }
}

# Observe Event - btn_schedule_activate/deactivate ------------------

OE_btn_schedule_activateDeactivate <- function(activate, 
                                               rv_Schedule, 
                                               input, 
                                               current_user_oid, 
                                               proxy){
  oid <- as.numeric(input$rdo_schedule)
  
  activateRecord(oid, 
                 active = activate, 
                 event_user = current_user_oid, 
                 table_name = "Schedule", 
                 event_table_name = "ScheduleEvent", 
                 parent_field_name = "ParentSchedule")
  
  RM_replaceData(query_fun = querySchedule, 
                 reactive_list = rv_Schedule, 
                 data_slot = "Schedule", 
                 selected_slot = "SelectedSchedule", 
                 id_variable = "OID", 
                 element_name = "rdo_user", 
                 oid = oid, 
                 proxy = proxy)
}

# Validate Schedule Inputs ------------------------------------------

validateScheduleInputs <- function(rv_Schedule, 
                                   input, 
                                   is_edit, 
                                   this_schedule_name){
  schedule_name <- trimws(input$txt_schedule_scheduleName)
  
  val <- inputValidationCollection()
  
  duplicate_schedule_message <- 
    sprintf("Schedule '%s' already exists in the database. Duplicates are not allowed.", 
            schedule_name)
  
  if (is_edit){
    if ((this_schedule_name != schedule_name) &&
        schedule_name %in% rv_Schedule$Schedule$ScheduleName){
      val$invalidate(duplicate_schedule_message)
    } 
  } else {
    print(isTRUE(this_schedule_name %in% rv_Schedule$Schedule$ScheduleName))
    print(this_schedule_name)
    print(rv_Schedule$Schedule$ScheduleName)
    if (isTRUE(schedule_name %in% rv_Schedule$Schedule$ScheduleName)){
      val$invalidate(duplicate_schedule_message)
    }
  }
  
  if (schedule_name == ""){
    val$invalidate("Schedule Name is empty or only whitespace")
  }
  
  val
}


# Date Formats ------------------------------------------------------
# Observe Event - rdo_dateFormat ------------------------------------

OE_rdo_dateFormat <- function(rv_DateFormat, input){
  oid <- as.numeric(input$rdo_dateFormat)
  
  rv_DateFormat$SelectedDateFormat <-  
    rv_DateFormat$DateFormat[rv_DateFormat$DateFormat$OID == oid, ]
}

# Observe Event - btn_dateFormat_addFormat --------------------------

OE_dateFormat_addFormat <- function(session, rv_DateFormat){
  rv_DateFormat$AddEdit <- "Add"
  
  updateTextInput(session = session, 
                  inputId = "txt_dateFormat_formatName", 
                  value = "")
  
  updateTextInput(session = session, 
                  inputId = "txt_dateFormat_description", 
                  value = "")
  
  updateTextInput(session = session, 
                  inputId = "txt_dateFormat_formatCode", 
                  value = "")
  
  updateNumericInput(session = session, 
                     inputId = "num_dateFormat_incrementStart", 
                     value = 0)
  
  updateSelectInput(session = session, 
                    inputId = "sel_dateFormat_incrementStartUnit", 
                    selected = "Second")
  
  updateNumericInput(session = session, 
                     inputId = "num_dateFormat_incrementEnd", 
                     value = 0)
  
  updateSelectInput(session = session, 
                    inputId = "sel_dateFormat_incrementEndUnit", 
                    selected = "Second")
  
  updateCheckboxInput(session = session, 
                      inputId = "chk_dateFormat_isActive", 
                      value = TRUE)
  
  toggleModal(session = session, 
              modalId = "modal_dateFormat", 
              toggle = "open")
}

# Observe Event - btn_dateFormat_editFormat -------------------------

OE_btn_dateFormat_editFormat <- function(session, rv_DateFormat){
  rv_DateFormat$AddEdit <- "Edit"
  
  updateTextInput(session = session, 
                  inputId = "txt_dateFormat_formatName", 
                  value = rv_DateFormat$SelectedDateFormat$ScheduleName)
  
  updateTextInput(session = session, 
                  inputId = "txt_dateFormat_description", 
                  value = rv_DateFormat$SelectedDateFormat$Description)
  
  updateTextInput(session = session, 
                  inputId = "txt_dateFormat_formatCode", 
                  value = rv_DateFormat$SelectedDateFormat$ScheduleCode)
  
  updateNumericInput(session = session, 
                     inputId = "num_dateFormat_incrementStart", 
                     value = rv_DateFormat$SelectedDateFormat$IncrementStart)
  
  updateSelectInput(session = session, 
                    inputId = "sel_dateFormat_incrementStartUnit", 
                    selected = rv_DateFormat$SelectedDateFormat$IncrementStartUnit)
  
  updateNumericInput(session = session, 
                     inputId = "num_dateFormat_incrementEnd", 
                     value = rv_DateFormat$SelectedDateFormat$IncrementEnd)
  
  updateSelectInput(session = session, 
                    inputId = "sel_dateFormat_incrementEndUnit", 
                    selected = rv_DateFormat$SelectedDateFormat$IncrementEndUnit)
  
  updateCheckboxInput(session = session, 
                      inputId = "chk_dateFormat_isActive", 
                      value = rv_DateFormat$SelectedDateFormat$IsActive)
  
  toggleModal(session = session, 
              modalId = "modal_dateFormat", 
              toggle = "open")
}
