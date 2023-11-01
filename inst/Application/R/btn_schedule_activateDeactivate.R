..btn_schedule_activateDeactivate <- function(activate, 
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
