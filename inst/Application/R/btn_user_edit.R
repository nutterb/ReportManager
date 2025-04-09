..btn_user_edit <- function(session, rv_User, input){
  rv_User$AddEdit <- "Edit"
  updateTextInput(session = session, 
                  inputId = "txt_user_lastName", 
                  value = rv_User$SelectedUser$LastName)
  updateTextInput(session = session, 
                  inputId = "txt_user_firstName", 
                  value = rv_User$SelectedUser$FirstName)
  updateTextInput(session = session, 
                  inputId = "txt_user_loginId", 
                  value = rv_User$SelectedUser$LoginId)
  updateTextInput(session = session, 
                  inputId = "txt_user_emailAddress", 
                  value = rv_User$SelectedUser$EmailAddress)
  updateCheckboxInput(session = session, 
                      inputId = "chk_user_isInternal", 
                      value = rv_User$SelectedUser$IsInternal)
  updateCheckboxInput(session = session, 
                      inputId = "chk_user_isActive", 
                      value = rv_User$SelectedUser$IsActive)
  reset("file_user_signature")
  rv_User$SignatureFileInput <- data.frame()
  
  rv_User$CurrentUserSignature <- queryUserSignature(rv_User$SelectedUser$OID)
  
  toggleModal(session = session, 
              modalId = "modal_user_addEdit", 
              toggle = "open")
}
