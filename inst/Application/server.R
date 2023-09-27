shinyServer(function(input, output, session){
  
  # Global ----------------------------------------------------------
  # Global - Reactive Values ----------------------------------------
  
  CURRENT_USER_OID <- 
    reactive({
      req(rv_User$User)
      rv_User$User$OID[rv_User$User$LoginId %in% Sys.info()["login"]]
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
  
  # Schedule --------------------------------------------------------
  # Schedule - Reactive Values --------------------------------------
  
  rv_Schedule <- reactiveValues(
    Schedule = querySchedule(), 
    AddEdit = "Add", 
    SelectedSchedule = NULL
  )
  
  rv_DateFormat <- reactiveValues(
    DateFormat = queryDateReportingFormat(), 
    AddEdit = "Add", 
    SelectedDateFormat = NULL
  )
  
  # Schedule - Passive Observers ------------------------------------
  
  observe({
    toggleState(id = "btn_schedule_addSchedule", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id = "btn_schedule_editSchedule", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_schedule) > 0)
  })
  
  observe({
    req(rv_Schedule$SelectedSchedule)
    
    toggleState(id = "btn_schedule_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_schedule) > 0 &&
                  isFALSE(rv_Schedule$SelectedSchedule$IsActive))
    
    toggleState(id = "btn_schedule_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_schedule) > 0 &&
                  isTRUE(rv_Schedule$SelectedSchedule$IsActive))
  })
  
  # Schedule - Event Observers --------------------------------------
  
  observeEvent(input$rdo_schedule, 
               OE_rdo_schedule(rv_Schedule, 
                               input))
  
  observeEvent(input$btn_schedule_addSchedule, 
               OE_btn_schedule_addSchedule(session, 
                                           rv_Schedule))
  
  observeEvent(input$btn_schedule_editSchedule,
               OE_btn_schedule_editSchedule(session, 
                                            rv_Schedule))
  
  observeEvent(
    input$btn_schedule_addEditSchedule,
    {
      oid <- if(rv_Schedule$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_schedule)
      
      addEditUser(oid = oid, 
                  schedule_name = input)
    }
  )
  
  observeEvent(
    input$btn_schedule_deactivate, 
    {
      oid <- as.numeric(input$rdo_schedule)
      
      activateRecord(oid, 
                     active = FALSE, 
                     event_user = CURRENT_USER_OID(), 
                     table_name = "Schedule", 
                     event_table_name = "ScheduleEvent", 
                     parent_field_name = "ParentSchedule")
      
      RM_replaceData(query_fun = querySchedule, 
                     reactive_list = rv_Schedule, 
                     data_slot = "Schedule", 
                     selected_slot = "SelectedSchedule", 
                     id_variable = "OID", 
                     element_name = "rdo_user", 
                     oid = oid, 
                     proxy = proxy_dt_schedule)
    }
  )
  
  observeEvent(
    input$btn_schedule_activate, 
    {
      oid <- as.numeric(input$rdo_schedule)
      
      activateRecord(oid, 
                     active = TRUE, 
                     event_user = CURRENT_USER_OID(), 
                     table_name = "Schedule", 
                     event_table_name = "ScheduleEvent", 
                     parent_field_name = "ParentSchedule")
      
      RM_replaceData(query_fun = querySchedule, 
                     reactive_list = rv_Schedule, 
                     data_slot = "Schedule", 
                     selected_slot = "SelectedSchedule", 
                     id_variable = "OID", 
                     element_name = "rdo_user", 
                     oid = oid, 
                     proxy = proxy_dt_schedule)
    }
  )
  
  # Schedule - Output -----------------------------------------------
  
  output$dt_schedule <- 
    DT::renderDataTable({
      querySchedule() %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_schedule") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_schedule <- dataTableProxy("dt_schedule")
  
  output$title_schedule_addEdit <- 
    renderText({
      if (rv_Schedule$AddEdit == "Add"){
        "Add a New Schedule"
      } else {
        sprintf("Editing Schedule %s (%s)", 
                rv_Schedule$SelectedSchedule$ScheduleName, 
                rv_Schedule$SelectedSchedule$OID)
      }
    })
  
  # Roles -----------------------------------------------------------
  # Roles - Reactive Values -----------------------------------------
  rv_Roles <- 
    reactiveValues(
      AddEdit = "Add", 
      Roles = queryRole(), 
      SelectedRole = NULL, 
      UserRole = NULL
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
  
  observeEvent(input$btn_role_viewEdit, 
               OE_btn_role_viewEdit(rv_User  = rv_User, 
                                    rv_Roles = rv_Roles, 
                                    session  = session))
  
  observeEvent(input$multi_userRole_move_all_right, 
               updateMultiSelect(session = session, 
                                 inputId = "multi_userRole", 
                                 input = input, 
                                 "move_all_right"))
  
  observeEvent(input$multi_userRole_move_right, 
               updateMultiSelect(session = session, 
                                 inputId = "multi_userRole", 
                                 input = input, 
                                 "move_right"))
  
  observeEvent(input$multi_userRole_move_left, 
               updateMultiSelect(session = session, 
                                 inputId = "multi_userRole", 
                                 input = input, 
                                 "move_left"))
  
  observeEvent(input$multi_userRole_move_all_left, 
               updateMultiSelect(session = session, 
                                 inputId = "multi_userRole", 
                                 input = input, 
                                 "move_all_left"))
  
  observeEvent(
    input$btn_userRole_save, 
    {
      OE_btn_userRole_save(input = input, 
                           current_user_oid = CURRENT_USER_OID())
    })
  
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
      if (rv_User$AddEdit == "Add"){
        "Add a New Role"
      } else {
        sprintf("Editing Role %s (%s)", 
                rv_Roles$SelectedRole$RoleName, 
                rv_Roles$SelectedRole$OID)
      }
    })
  
  output$title_userRole_edit <- 
    renderText({
      sprintf("Users assigned to role: %s", 
              rv_Roles$SelectedRole$RoleName)
    })
  
  # User ------------------------------------------------------------
  # User - Reactive Values ------------------------------------------
  
  rv_User <- 
    reactiveValues(
      AddEdit = "Add", 
      User = queryUser(), 
      SelectedUser = NULL
    )
  
  # User - Passive Observer -----------------------------------------
  
  observe({
    toggleState(id = "btn_user_add", 
                condition = USER_IS_USER_ADMIN())
    
    toggleState(id = "btn_user_edit", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_user) > 0)
  })
  
  observe({
    req(rv_User$SelectedUser)

    toggleState(id = "btn_user_activate", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_user) > 0 &&
                  isFALSE(rv_User$SelectedUser$IsActive))
    
    toggleState(id = "btn_user_deactivate", 
                condition = USER_IS_USER_ADMIN() &&
                  length(input$rdo_user) > 0 &&
                  isTRUE(rv_User$SelectedUser$IsActive))
  })
  
  # User - Event Observer -------------------------------------------
  
  observeEvent(input$rdo_user, 
               OE_rdo_user(rv_User = rv_User, 
                                 input         = input))
  
  observeEvent(input$btn_user_add, 
               OE_btn_user_add(session       = session, 
                                     rv_User = rv_User, 
                                     input         = input))
  
  observeEvent(input$btn_user_edit, 
               OE_btn_user_edit(session       = session, 
                                      rv_User = rv_User, 
                                      input         = input))
  
  observeEvent(input$btn_user_addEditUser,
               OE_btn_user_addEditUser(session          = session, 
                                                   rv_User    = rv_User, 
                                                   input            = input, 
                                                   current_user_oid = CURRENT_USER_OID(), 
                                                   proxy            = proxy_dt_user, 
                                                   is_edit          = rv_User$AddEdit == "Edit",
                                                   this_login_id    = rv_User$SelectedUser$LoginId))
  
  observeEvent(input$btn_user_activate, 
               OE_btn_user_activate(active           = TRUE, 
                                          rv_User    = rv_User, 
                                          input            = input, 
                                          current_user_oid = CURRENT_USER_OID(), 
                                          proxy            = proxy_dt_user))
  
  observeEvent(input$btn_user_deactivate, 
               OE_btn_user_activate(active           = FALSE, 
                                    rv_User          = rv_User, 
                                    input            = input, 
                                    current_user_oid = CURRENT_USER_OID(), 
                                    proxy            = proxy_dt_user))
  
  # User - Output ---------------------------------------------------
  
  output$dt_user <- 
    DT::renderDataTable({
      queryUser() %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_user") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_user <- DT::dataTableProxy("dt_user")
  
  output$title_addEditUser <- 
    renderText({
      if (rv_User$AddEdit == "Add"){
        "Add a New User"
      } else {
        sprintf("Editing User %s, %s (%s)", 
                rv_User$SelectedUser$LastName, 
                rv_User$SelectedUser$FirstName, 
                rv_User$SelectedUser$OID)
      }
    })
  
  # Stop App when Session Ends --------------------------------------
  session$onSessionEnded(function(){ stopApp() })
})
