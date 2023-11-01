..btn_user_add <- function(session, rv_User, input){
  rv_User$AddEdit <- "Add"
  lapply(c("txt_user_lastName", 
           "txt_user_firstName", 
           "txt_user_loginId", 
           "txt_user_emailAddress"), 
         function(ctrl) updateTextInput(session = session, 
                                        inputId = ctrl, 
                                        value = ""))
  updateCheckboxInput(session = session, 
                      inputId = "chk_user_isInternal", 
                      value = FALSE)
  updateCheckboxInput(session = session, 
                      inputId = "chk_user_isActive", 
                      value = TRUE)
  toggleModal(session = session, 
              modalId = "modal_user_addEdit", 
              toggle = "open")
}
