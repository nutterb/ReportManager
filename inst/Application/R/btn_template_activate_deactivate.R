..btn_template_activate_deactivate <- function(activate, 
                                                input, 
                                                current_user_oid, 
                                                rv_Template, 
                                                proxy){
  oid <- as.numeric(input$rdo_template)
  activateRecord(oid, 
                 active = activate, 
                 event_user = current_user_oid, 
                 table_name = "ReportTemplate", 
                 event_table_name = "ReportTemplateEvent", 
                 parent_field_name = "ParentReportTemplate")
  RM_replaceData(query_fun = queryReportTemplate, 
                 reactive_list = rv_Template, 
                 data_slot = "Template", 
                 selected_slot = "SelectedTemplate", 
                 id_variable = "OID", 
                 element_name = "rdo_template", 
                 oid = oid, 
                 proxy = proxy, 
                 cols = REPORT_TEMPLATE_DISPLAY_PROPERTIES)
}
