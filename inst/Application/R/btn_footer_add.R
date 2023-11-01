..btn_footer_add <- function(session, 
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
