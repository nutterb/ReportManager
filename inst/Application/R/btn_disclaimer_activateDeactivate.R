..btn_disclaimer_activateDeactivate <- function(activate, 
                                                 rv_Disclaimer, 
                                                 input, 
                                                 current_user_oid, 
                                                 proxy){
  oid <- as.numeric(input$rdo_disclaimer)
  activateRecord(oid, 
                 active = activate, 
                 event_user = current_user_oid, 
                 table_name = "Disclaimer", 
                 event_table_name = "DisclaimerEvent", 
                 parent_field_name = "ParentDisclaimer")
  RM_replaceData(query_fun = queryDisclaimer, 
                 reactive_list = rv_Disclaimer, 
                 data_slot = "Disclaimer", 
                 selected_slot = "SelectedDisclaimer", 
                 id_variable = "OID", 
                 element_name = "rdo_disclaimer", 
                 oid = oid, 
                 proxy = proxy)
}
