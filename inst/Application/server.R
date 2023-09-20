shinyServer(function(input, output, session){
  
  # Global ----------------------------------------------------------
  # Global - Reactive Values ----------------------------------------
  
  USER_IS_USER_ADMIN <- 
    reactive({
      TRUE
    })
  
  USER_IS_REPORT_ADMIN <- 
    reactive({
      TRUE
    })
  
  # Global - Passive Observer ---------------------------------------
  # Global - Event Observer -----------------------------------------
  
  # ReportUser ------------------------------------------------------
  # ReportUser - Reactive Values ------------------------------------
  
  rv_ReportUser <- 
    reactiveValues(
      AddEdit = "Add", 
      ReportUser = queryReportUser(), 
      SelectedReportUser = NULL
    )
  
  # ReportUser - Passive Observer -----------------------------------
  
  observe({
    toggleState(id = "btn_reportUser_add", 
                condition = USER_IS_USER_ADMIN())
  })
  
  # ReportUser - Event Observer -------------------------------------
  
  observeEvent(
    input$btn_reportUser_add, 
    {
      rv_ReportUser$AddEdit <- "Add"
      lapply(c("txt_userRole_lastName", 
               "txt_userRole_firstName", 
               "txt_userRole_loginId", 
               "txt_userRole_emailAddress"), 
             function(ctrl) updateTextInput(inputId = ctrl, 
                                            value = ""))
      
      updateCheckboxInput(inputId = "chk_userRole_isInternal", 
                          value = FALSE)
      updateCheckboxInput(inputId = "chk_userRole_isActive", 
                          value = TRUE)
      
      toggleModal(session = session, 
                  modalId = "modal_reportUser_addEdit", 
                  toggle = "open")
    }
  )
  # ReportUser - Output ---------------------------------------------
  
  output$dt_reportUser <- 
    DT::renderDataTable({
      queryReportUser() %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_reportUser") %>% 
        RM_datatable(escape = -1)
    })
  
  output$title_addEditReportUser <- 
    renderText({
      if (rv_ReportUser$AddEdit == "Add"){
        "Add a New Report User"
      } else {
        sprintf("Editing User %s (%s)", 
                NA, NA)
      }
    })
  
  # Stop App when Session Ends --------------------------------------
  session$onSessionEnded(function(){ stopApp() })
})
