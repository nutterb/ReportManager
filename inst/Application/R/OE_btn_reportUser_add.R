OE_btn_reportUser_add <- function(session, rv_ReportUser, input){
  rv_ReportUser$AddEdit <- "Add"
  lapply(c("txt_reportUser_lastName", 
           "txt_reportUser_firstName", 
           "txt_reportUser_loginId", 
           "txt_reportUser_emailAddress"), 
         function(ctrl) updateTextInput(session = session, 
                                        inputId = ctrl, 
                                        value = ""))
  
  updateCheckboxInput(session = session, 
                      inputId = "chk_reportUser_isInternal", 
                      value = FALSE)
  updateCheckboxInput(session = session, 
                      inputId = "chk_reportUser_isActive", 
                      value = TRUE)
  
  toggleModal(session = session, 
              modalId = "modal_reportUser_addEdit", 
              toggle = "open")
}
