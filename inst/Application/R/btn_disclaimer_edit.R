..btn_disclaimer_edit <- function(session, 
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
