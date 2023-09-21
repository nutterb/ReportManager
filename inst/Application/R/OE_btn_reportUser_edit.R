OE_btn_reportUser_edit <- function(session, rv_ReportUser, input){
  rv_ReportUser$AddEdit <- "Edit"
  
  updateTextInput(session = session, 
                  inputId = "txt_reportUser_lastName", 
                  value = rv_ReportUser$SelectedReportUser$LastName)
  updateTextInput(session = session, 
                  inputId = "txt_reportUser_firstName", 
                  value = rv_ReportUser$SelectedReportUser$FirstName)
  updateTextInput(session = session, 
                  inputId = "txt_reportUser_loginId", 
                  value = rv_ReportUser$SelectedReportUser$LoginId)
  updateTextInput(session = session, 
                  inputId = "txt_reportUser_emailAddress", 
                  value = rv_ReportUser$SelectedReportUser$EmailAddress)
  
  updateCheckboxInput(session = session, 
                      inputId = "chk_reportUser_isInternal", 
                      value = rv_ReportUser$SelectedReportUser$IsInternal)
  updateCheckboxInput(session = session, 
                      inputId = "chk_reportUser_isActive", 
                      value = rv_ReportUser$SelectedReportUser$IsActive)
  
  toggleModal(session = session, 
              modalId = "modal_reportUser_addEdit", 
              toggle = "open")
}
