..btn_footer_addEditFooter <- function(session, 
                                            rv_Footer, 
                                            input, 
                                            current_user_oid, 
                                            proxy){
  oid <- if(rv_Footer$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_footer)
  val <- validateFooterInputs(rv_Footer = rv_Footer, 
                              input = input)
  if (!val$is_ok()){
    alert(val$report())
  } else {
    addEditFooter(oid = oid, 
                  footer = input$txt_footer_text,
                  is_active = input$chk_footer_isActive, 
                  event_user = current_user_oid)
    RM_replaceData(query_fun = queryFooter, 
                   reactive_list = rv_Footer, 
                   data_slot = "Footer", 
                   selected_slot = "SelectedFooter", 
                   id_variable = "OID", 
                   element_name = "rdo_footer", 
                   oid = oid, 
                   proxy = proxy)
    toggleModal(session = session, 
                modalId = "modal_footer_addEdit", 
                toggle = "close")
  }
}
