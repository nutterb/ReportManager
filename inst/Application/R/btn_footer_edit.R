..btn_footer_edit <- function(session, 
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
