..btn_dateFormat_activateDeactivate <- function(activate, 
                                                 rv_DateFormat, 
                                                 input, 
                                                 current_user_oid, 
                                                 proxy){
  oid <- as.numeric(input$rdo_dateFormat)
  activateRecord(oid, 
                 active = activate, 
                 event_user = current_user_oid, 
                 table_name = "DateReportingFormat", 
                 event_table_name = "DateReportingFormatEvent", 
                 parent_field_name = "ParentDateReportingFormat")
  RM_replaceData(query_fun = queryDateReportingFormat, 
                 reactive_list = rv_DateFormat, 
                 data_slot = "DateFormat", 
                 selected_slot = "SelectedDateFormat", 
                 id_variable = "OID", 
                 element_name = "rdo_dateFormat", 
                 oid = oid, 
                 proxy = proxy)
}
