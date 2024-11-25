..btn_logo_edit <- function(session, 
                             rv_Logo){
  rv_Logo$AddEdit <- "Edit"
  shinyjs::hide(id = "file_logo_add")
  updateTextInput(session = session, 
                  inputId = "txt_logo_add_fileName",
                  value = rv_Logo$SelectedLogo$FileName)
  updateTextInput(session = session, 
                  inputId = "txt_logo_add_description", 
                  value = rv_Logo$SelectedLogo$Description)
  updateTextInput(session = session, 
                  inputId = "txt_logo_add_extension", 
                  value = rv_Logo$SelectedLogo$FileExtension)
  toggleModal(session = session, 
              modalId = "modal_logo_addEdit", 
              toggle = "open")
}
