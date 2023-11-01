..btn_disclaimer_addEditDisclaimer <- function(session, 
                                             rv_Disclaimer, 
                                             input, 
                                             current_user_oid, 
                                             proxy){
  oid <- if(rv_Disclaimer$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_disclaimer)
  
  val <- validateDisclaimerInputs(rv_Disclaimer = rv_Disclaimer, 
                                  input = input)
  
  if (!val$is_ok()){
    alert(val$report())
  } else {
    addEditDisclaimer(oid = oid, 
                      disclaimer = input$txt_disclaimer_text,
                      is_active = input$chk_disclaimer_isActive, 
                      event_user = current_user_oid)
    
    RM_replaceData(query_fun = queryDisclaimer, 
                   reactive_list = rv_Disclaimer, 
                   data_slot = "Disclaimer", 
                   selected_slot = "SelectedDisclaimer", 
                   id_variable = "OID", 
                   element_name = "rdo_disclaimer", 
                   oid = oid, 
                   proxy = proxy)
    
    toggleModal(session = session, 
                modalId = "modal_disclaimer_addEdit", 
                toggle = "close")
  }
}
