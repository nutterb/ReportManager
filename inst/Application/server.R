shinyServer(function(input, output, session){
  
  # Global ----------------------------------------------------------
  # Global - Reactive Values ----------------------------------------
  
  CURRENT_USER_OID <- 
    reactive({
      req(rv_ReportUser$ReportUser)
      rv_ReportUser$ReportUser$OID[rv_ReportUser$ReportUser$LoginId %in% Sys.info()["login"]]
    })
  
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
    
    toggleState(id = "btn_reportUser_edit", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_reportUser) > 0)
  })
  
  observe({
    req(rv_ReportUser$SelectedReportUser)

    toggleState(id = "btn_reportUser_activate", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_reportUser) > 0 &&
                  isFALSE(rv_ReportUser$SelectedReportUser$IsActive))
    
    toggleState(id = "btn_reportUser_deactivate", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_reportUser) > 0 &&
                  isTRUE(rv_ReportUser$SelectedReportUser$IsActive))
  })
  
  # ReportUser - Event Observer -------------------------------------
  
  observeEvent(input$rdo_reportUser, 
               OE_rdo_reportUser(rv_ReportUser, 
                                 input))
  
  observeEvent(input$btn_reportUser_add, 
               OE_btn_reportUser_add(session, 
                                     rv_ReportUser, 
                                     input))
  
  observeEvent(input$btn_reportUser_edit, 
               OE_btn_reportUser_edit(session, 
                                      rv_ReportUser, 
                                      input))
  
  observeEvent(input$btn_reportUser_addEditReportUser, 
               OE_btn_reportUser_addEditReportUser(session, 
                                                   rv_ReportUser, 
                                                   input, 
                                                   CURRENT_USER_OID(), 
                                                   proxy_dt_reportUser))
  
  observeEvent(
    input$btn_reportUser_activate, 
    {
      oid <- as.numeric(input$rdo_reportUser)

      activateReportUser(oid, 
                         TRUE, 
                         CURRENT_USER_OID())
      
      NewData <- queryReportUser()
      rv_ReportUser$ReportUser <- NewData
      rv_ReportUser$SelectedReportUser <- NewData[NewData$OID == oid, ]
      
      NewData %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_reportUser", 
                       checked = as.character(oid)) %>% 
        DT::replaceData(proxy = proxy_dt_reportUser, 
                        data = ., 
                        resetPaging = FALSE,
                        rownames = FALSE)
    }
  )
  
  observeEvent(
    input$btn_reportUser_deactivate, 
    {
      oid <- as.numeric(input$rdo_reportUser)

      activateReportUser(oid, 
                         FALSE, 
                         CURRENT_USER_OID())
      
      NewData <- queryReportUser()
      rv_ReportUser$ReportUser <- NewData
      rv_ReportUser$SelectedReportUser <- NewData[NewData$OID == oid, ]
      
      NewData %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_reportUser", 
                       checked = as.character(oid)) %>% 
        DT::replaceData(proxy = proxy_dt_reportUser, 
                        data = ., 
                        resetPaging = FALSE,
                        rownames = FALSE)
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
  
  proxy_dt_reportUser <- DT::dataTableProxy("dt_reportUser")
  
  output$title_addEditReportUser <- 
    renderText({
      if (rv_ReportUser$AddEdit == "Add"){
        "Add a New Report User"
      } else {
        sprintf("Editing User %s, %s (%s)", 
                rv_ReportUser$SelectedReportUser$LastName, 
                rv_ReportUser$SelectedReportUser$FirstName, 
                rv_ReportUser$SelectedReportUser$OID)
      }
    })
  
  # Stop App when Session Ends --------------------------------------
  session$onSessionEnded(function(){ stopApp() })
})
