..brn_role_edit <- function(session, rv_Roles, input){
  rv_Roles$AddEdit <- "Edit"
  updateTextInput(session = session, 
                  inputId = "txt_role_roleName", 
                  value = rv_Roles$SelectedRole$RoleName)
  updateTextInput(session = session, 
                  inputId = "txt_role_roleDescription", 
                  value = rv_Roles$SelectedRole$RoleDescription)
  updateCheckboxInput(session = session, 
                      inputId = "chk_role_isActive", 
                      value = rv_Roles$SelectedRole$IsActive)
  toggleModal(session = session, 
              modalId = "modal_role_addEdit", 
              toggle = "open")
}
