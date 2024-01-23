..btn_role_add <- function(session, rv_Roles){
  rv_Roles$AddEdit <- "Add"
  updateTextInput(session = session, 
                  inputId = "txt_role_roleName", 
                  value = "")
  updateTextInput(session = session, 
                  inputId = "txt_role_roleDescription", 
                  value = "")
  updateCheckboxInput(session = session, 
                      inputId = "chk_role_isActive", 
                      value = TRUE)
  toggleModal(session = session, 
              modalId = "modal_role_addEdit", 
              toggle = "open")
}
