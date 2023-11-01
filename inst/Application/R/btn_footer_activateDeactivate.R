..btn_footer_activateDeactivate <- function(activate, 
                                             rv_Footer, 
                                             input, 
                                             current_user_oid, 
                                             proxy){
  oid <- as.numeric(input$rdo_footer)
  activateRecord(oid, 
                 active = activate, 
                 event_user = current_user_oid, 
                 table_name = "Footer", 
                 event_table_name = "FooterEvent", 
                 parent_field_name = "ParentFooter")
  RM_replaceData(query_fun = queryFooter, 
                 reactive_list = rv_Footer, 
                 data_slot = "Footer", 
                 selected_slot = "SelectedFooter", 
                 id_variable = "OID", 
                 element_name = "rdo_footer", 
                 oid = oid, 
                 proxy = proxy)
}
