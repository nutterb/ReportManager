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
  
  observeEvent(
    input$rdo_role, 
    {
      oid <- as.numeric(input$rdo_role)
      
      ThisRole <- rv_Roles$Roles
      ThisRole <- ThisRole[ThisRole$OID == oid, ]
      rv_Roles$SelectedRole <- ThisRole
    }
  )
  
  observeEvent(
    input$btn_role_add, 
    {
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
  )
  
  observeEvent(
    input$btn_role_edit, 
    {
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
  )
  
  observeEvent(
    input$btn_role_addEditRole, 
    {
      oid <- if (rv_Roles$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_role)
      
      val <- validateRoleInputs(rv_Roles, 
                                input, 
                                is_edit = rv_Roles$AddEdit == "Edit", 
                                this_role_name = rv_Roles$SelectedRole$RoleName)
      
      if (!val$is_ok()){
        alert(val$report())
      } else {
        addEditRole(oid = oid,
                    role_name = trimws(input$txt_role_roleName),
                    role_description = trimws(input$txt_role_roleDescription),
                    is_active = input$chk_role_isActive,
                    event_user = CURRENT_USER_OID())
        
        RM_replaceData(query_fun = queryRole, 
                       reactive_list = rv_Roles, 
                       data_slot = "Roles", 
                       selected_slot = "SelectedRole", 
                       id_variable = "OID", 
                       element_name = "rdo_role", 
                       oid = oid, 
                       proxy = proxy_dt_role)
        
        toggleModal(session = session, 
                    modalId = "modal_role_addEdit", 
                    toggle = "close")
      }
    }
  )
  
  observeEvent(
    input$btn_role_activate, 
    {
      oid <- as.numeric(input$rdo_role)
      
      activateRecord(oid, 
                     TRUE, 
                     CURRENT_USER_OID(), 
                     table_name = "Role", 
                     event_table_name = "RoleEvent", 
                     parent_field_name = "ParentRole")
      
      RM_replaceData(query_fun = queryRole, 
                     reactive_list = rv_Roles, 
                     data_slot = "Roles", 
                     selected_slot = "SelectedRole", 
                     id_variable = "OID", 
                     oid = oid, 
                     element_name = "rdo_role", 
                     proxy = proxy_dt_role)
    }
  )
  
  observeEvent(
    input$btn_role_deactivate, 
    {
      oid <- as.numeric(input$rdo_role)
      
      activateRecord(oid, 
                     FALSE, 
                     CURRENT_USER_OID(), 
                     table_name = "Role", 
                     event_table_name = "RoleEvent", 
                     parent_field_name = "ParentRole")
      
      RM_replaceData(query_fun = queryRole, 
                     reactive_list = rv_Roles, 
                     data_slot = "Roles", 
                     selected_slot = "SelectedRole", 
                     id_variable = "OID", 
                     oid = oid, 
                     element_name = "rdo_role", 
                     proxy = proxy_dt_role)
    }
  )
  
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
