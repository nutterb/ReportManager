..btn_disclaimer_add <- function(session, 
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
