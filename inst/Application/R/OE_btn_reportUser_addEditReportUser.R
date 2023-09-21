OE_btn_reportUser_addEditReportUser <- function(session, 
                                            rv_ReportUser, input, 
                                            current_user_oid, proxy){
  oid <- if (rv_ReportUser$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_reportUser)
  
  addEditReportUser(oid = oid, 
                    last_name = input$txt_reportUser_lastName, 
                    first_name = input$txt_reportUser_firstName, 
                    login_id = input$txt_reportUser_loginId, 
                    email = input$txt_reportUser_emailAddress, 
                    is_internal = input$chk_reportUser_isInternal, 
                    is_active = input$chk_reportUser_isActive, 
                    event_user = current_user_oid)
  
  NewData <- queryReportUser()
  rv_ReportUser$ReportUser <- NewData
  rv_ReportUser$SelectedReportUser <- NewData[NewData$OID == oid, ]
  
  NewData %>% 
    radioDataTable(id_variable = "OID", 
                   element_name = "rdo_reportUser", 
                   checked = as.character(oid)) %>% 
    DT::replaceData(proxy = proxy, 
                    data = ., 
                    resetPaging = FALSE,
                    rownames = FALSE)
  
  toggleModal(session = session, 
              modalId = "modal_reportUser_addEdit", 
              toggle = "close")
}
