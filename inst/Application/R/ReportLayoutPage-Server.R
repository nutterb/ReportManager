# Disclaimers -------------------------------------------------------
# Observe Event - rdo_disclaimer ------------------------------------

OE_rdo_disclaimer <- function(rv_Disclaimer, input){
  oid <- as.numeric(input$rdo_disclaimer)
  
  rv_Disclaimer$SelectedDisclaimer <- 
    rv_Disclaimer$Disclaimer[rv_Disclaimer$Disclaimer$OID == oid, ]
}

# Observe Event - btn_disclaimer_add --------------------------------

OE_btn_disclaimer_add <- function(session, 
                                  rv_Disclaimer, 
                                  input){
  rv_Disclaimer$AddEdit <- "Add"
  
  updateTextInput(session = session, 
                  inputId = "txt_disclaimer_text", 
                  value = "")
  updateCheckboxInput(inputId = "chk_disclaimer_isActive", 
                      value = TRUE)
  
  toggleModal(session = session, 
              modalId = "modal_disclaimer_addEdit", 
              toggle = "open")
}

# Observe Event - btn_disclaimer_edit -------------------------------

OE_btn_disclaimer_edit <- function(session, 
                                   rv_Disclaimer, 
                                   input){
  rv_Disclaimer$AddEdit <- "Edit"
  
  updateTextInput(session = session, 
                  inputId = "txt_disclaimer_text",
                  value = rv_Disclaimer$SelectedDisclaimer$Disclaimer)
  updateCheckboxInput(inputId = "chk_disclaimer_isActive", 
                      value = rv_Disclaimer$SelectedDisclaimer$IsActive)
  
  toggleModal(session = session, 
              modalId = "modal_disclaimer_addEdit", 
              toggle = "open")
}

# Observe Event - btn_disclaimer_addEditDisclaimer ------------------

OE_btn_disclaimer_addEditDisclaimer <- function(session, 
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

# Observe Event - btn_disclaimer_activateDeactivate -----------------

OE_btn_disclaimer_activateDeactivate <- function(activate, 
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

# Validation - validateDisclaimerInputs -----------------------------

validateDisclaimerInputs <- function(rv_Disclaimer, 
                                     input){
  disclaimer <- trimws(input$txt_disclaimer_text)
  
  val <- inputValidationCollection()
  
  if (disclaimer == ""){
    val$invalidate("Disclaimer is empty or all whitespace.")
  }
  
  if (nchar(disclaimer) > 2000){
    val$invalidate("Disclaimer cannot exceed 2,000 characters.")
  }
  
  val
}

# Footers -----------------------------------------------------------
# Observe Event - rdo_footer ----------------------------------------

OE_rdo_footer <- function(rv_Footer, input){
  oid <- as.numeric(input$rdo_footer)
  
  rv_Footer$SelectedFooter <- 
    rv_Footer$Footer[rv_Footer$Footer$OID == oid, ]
}

# Observe Event - btn_footer_add ------------------------------------

OE_btn_footer_add <- function(session, 
                              rv_Footer, 
                              input){
  rv_Footer$AddEdit <- "Add"
  
  updateTextInput(session = session, 
                  inputId = "txt_footer_text", 
                  value = "")
  updateCheckboxInput(inputId = "chk_footer_isActive", 
                      value = TRUE)
  
  toggleModal(session = session, 
              modalId = "modal_footer_addEdit", 
              toggle = "open")
}

# Observe Event - btn_footer_edit -----------------------------------

OE_btn_footer_edit <- function(session, 
                               rv_Footer, 
                               input){
  rv_Footer$AddEdit <- "Edit"
  
  updateTextInput(session = session, 
                  inputId = "txt_footer_text",
                  value = rv_Footer$SelectedFooter$Footer)
  updateCheckboxInput(inputId = "chk_footer_isActive", 
                      value = rv_Footer$SelectedFooter$IsActive)
  
  toggleModal(session = session, 
              modalId = "modal_footer_addEdit", 
              toggle = "open")
}

# Observe Event - btn_footer_addEditFooter --------------------------

OE_btn_footer_addEditFooter <- function(session, 
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

# Observe Event - btn_footer_activateDeactivate ---------------------

OE_btn_footer_activateDeactivate <- function(activate, 
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

# Validation - validateFooterInputs ---------------------------------

validateFooterInputs <- function(rv_Footer, 
                                 input){
  footer <- trimws(input$txt_footer_text)
  
  val <- inputValidationCollection()
  
  if (footer == ""){
    val$invalidate("Footer is empty or all whitespace.")
  }
  
  if (nchar(footer) > 2000){
    val$invalidate("Footer cannot exceed 2,000 characters.")
  }
  
  val
}
