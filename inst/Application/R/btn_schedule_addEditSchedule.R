..btn_schedule_addEditSchedule <- function(session, 
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
                    is_period_to_date = input$chk_schedule_isPeriodToDate,
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
