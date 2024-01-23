..btn_logo_add <- function(session, 
                            rv_Logo){
  rv_Logo$AddEdit <- "Add"
  show("file_logo_add")
  updateTextInput(session = session, 
                  inputId = "txt_logo_add_fileName",
                  value = "")
  updateTextInput(session = session, 
                  inputId = "txt_logo_add_description", 
                  value = "")
  updateTextInput(session = session, 
                  inputId = "txt_logo_add_extension", 
                  value = "")
  toggleModal(session = session, 
              modalId = "modal_logo_addEdit", 
              toggle = "open")
}
