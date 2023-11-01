..btn_dateFormat_addEditFormat <- function(session, 
                                            rv_DateFormat, 
                                            input, 
                                            current_user_oid, 
                                            proxy){
  oid <- if(rv_DateFormat$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_dateFormat)
  val <- validateDateFormatInputs(rv_DateFormat = rv_DateFormat, 
                                  input = input, 
                                  is_edit = rv_DateFormat$AddEdit == "Edit", 
                                  this_format_name = rv_DateFormat$SelectedDateFormat$FormatName)
  if (!val$is_ok()){
    alert(val$report())
  } else {
    print(oid)
    addEditDateReportingFormat(oid = oid, 
                               format_name = input$txt_dateFormat_formatName, 
                               description = input$txt_dateFormat_description, 
                               format_code = input$txt_dateFormat_formatCode, 
                               increment_start = input$num_dateFormat_incrementStart, 
                               increment_start_unit = input$sel_dateFormat_incrementStartUnit, 
                               increment_end = input$num_dateFormat_incrementEnd, 
                               increment_end_unit = input$sel_dateFormat_incrementEndUnit, 
                               is_active = input$chk_dateFormat_isActive, 
                               event_user = current_user_oid)
    RM_replaceData(query_fun = queryDateReportingFormat, 
                   reactive_list = rv_DateFormat, 
                   data_slot = "DateFormat", 
                   selected_slot = "SelectedDateFormat", 
                   id_variable = "OID", 
                   element_name = "rdo_dateFormat", 
                   oid = oid, 
                   proxy = proxy)
    toggleModal(session = session, 
                modalId = "modal_dateFormat_addEdit", 
                toggle = "close")
  }
}
