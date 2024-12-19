..btn_template_add_edit <- function(session, 
                                     rv_Template, 
                                     input, 
                                     current_user_oid, 
                                     proxy){
  oid <- if (rv_Template$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_template)
  addEditReportTemplate(oid = oid, 
                        template_directory = input$sel_template_directory, 
                        template_file = input$sel_template_file, 
                        title = input$txt_template_title, 
                        title_size = input$sel_template_titleSize, 
                        include_toc = input$chk_template_includeTableOfContents,
                        is_signature_required = input$chk_template_isSignatureRequired, 
                        is_active = input$chk_template_isActive, 
                        logo_oid = as.numeric(input$sel_template_logo), 
                        default_email = input$txt_template_defaultEmailText,
                        date_reporting_format = as.numeric(input$sel_template_dateReportingFormat),
                        event_user = current_user_oid)
  RM_replaceData(query_fun = queryReportTemplate, 
                 reactive_list = rv_Template, 
                 data_slot = "Template", 
                 selected_slot = "SelectedTemplate", 
                 id_variable = "OID", 
                 element_name = "rdo_template", 
                 oid = oid, 
                 proxy = proxy, 
                 cols = REPORT_TEMPLATE_DISPLAY_PROPERTIES)
  toggleModal(session = session, 
              modalId = "modal_template_addEdit", 
              toggle = "close")
}
