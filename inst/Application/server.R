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
  
  # Roles -----------------------------------------------------------
  # Roles - Reactive Values -----------------------------------------
  rv_Roles <- 
    reactiveValues(
      AddEdit = "Add", 
      Roles = queryRole(), 
      SelectedRole = NULL
    )
  
  # Roles - Passive Observers ---------------------------------------
  
  observe({
    toggleState(id = "btn_role_add", 
                condition = USER_IS_USER_ADMIN())
    
    toggleState(id = "btn_role_edit", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_role) > 0)
    
    toggleState(id = "btn_role_viewEdit", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_role) > 0)
  })
  
  observe({
    req(rv_Roles$SelectedRole)
    
    toggleState(id = "btn_role_activate", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_role) > 0 &&
                  isFALSE(rv_Roles$SelectedRole$IsActive))
    
    toggleState(id = "btn_role_deactivate", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_role) > 0 &&
                  isTRUE(rv_Roles$SelectedRole$IsActive))
  })
  
  # Roles - Event Observers -----------------------------------------
  
  observeEvent(input$rdo_role, 
               OE_rdo_role(rv_Roles = rv_Roles, 
                           input    = input))
  
  observeEvent(input$btn_role_add,
               OE_btn_role_add(session  = session, 
                               rv_Roles = rv_Roles))
  
  observeEvent(input$btn_role_edit, 
               OE_brn_role_edit(session  = session, 
                                rv_Roles = rv_Roles, 
                                input    = input))
  
  observeEvent(input$btn_role_addEditRole, 
               OE_btn_role_addEditRole(session          = session, 
                                       rv_Roles         = rv_Roles, 
                                       input            = input, 
                                       is_edit          = rv_Roles$AddEdit == "Edit", 
                                       this_role_name   = rv_Roles$SelectedRole$RoleName, 
                                       current_user_oid = CURRENT_USER_OID(), 
                                       proxy            = proxy_dt_role))
  
  observeEvent(input$btn_role_activate, 
               OE_btn_role_activateDeactivate(active           = TRUE, 
                                              rv_Roles         = rv_Roles,
                                              input            = input, 
                                              current_user_oid = CURRENT_USER_OID(), 
                                              proxy            = proxy_dt_role))
  
  observeEvent(input$btn_role_deactivate, 
               OE_btn_role_activateDeactivate(active           = FALSE, 
                                              rv_Roles         = rv_Roles,
                                              input            = input, 
                                              current_user_oid = CURRENT_USER_OID(), 
                                              proxy            = proxy_dt_role))
  
  # Roles - Output --------------------------------------------------
  
  output$dt_role <- 
    DT::renderDataTable({
      queryRole() %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_role") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_role <- DT::dataTableProxy("dt_role")
  
  output$title_addEditRole <- 
    renderText({
      if (rv_ReportUser$AddEdit == "Add"){
        "Add a New Role"
      } else {
        sprintf("Editing Role %s (%s)", 
                rv_Roles$SelectedRole$RoleName, 
                rv_Roles$SelectedRole$OID)
      }
    })
  
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
               OE_rdo_reportUser(rv_ReportUser = rv_ReportUser, 
                                 input         = input))
  
  observeEvent(input$btn_reportUser_add, 
               OE_btn_reportUser_add(session       = session, 
                                     rv_ReportUser = rv_ReportUser, 
                                     input         = input))
  
  observeEvent(input$btn_reportUser_edit, 
               OE_btn_reportUser_edit(session       = session, 
                                      rv_ReportUser = rv_ReportUser, 
                                      input         = input))
  
  observeEvent(input$btn_reportUser_addEditReportUser,
               OE_btn_reportUser_addEditReportUser(session          = session, 
                                                   rv_ReportUser    = rv_ReportUser, 
                                                   input            = input, 
                                                   current_user_oid = CURRENT_USER_OID(), 
                                                   proxy            = proxy_dt_reportUser, 
                                                   is_edit          = rv_ReportUser$AddEdit == "Edit",
                                                   this_login_id    = rv_ReportUser$SelectedReportUser$LoginId))
  
  observeEvent(input$btn_reportUser_activate, 
               OE_btn_reportUser_activate(active           = TRUE, 
                                          rv_ReportUser    = rv_ReportUser, 
                                          input            = input, 
                                          current_user_oid = CURRENT_USER_OID(), 
                                          proxy            = proxy_dt_reportUser))
  
  observeEvent(input$btn_reportUser_deactivate, 
               OE_btn_reportUser_activate(active           = FALSE, 
                                          rv_ReportUser    = rv_ReportUser, 
                                          input            = input, 
                                          current_user_oid = CURRENT_USER_OID(), 
                                          proxy            = proxy_dt_reportUser))
  
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
