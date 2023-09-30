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
  
  # Report Template -------------------------------------------------
  # Report Template - Reactive Values -------------------------------
  
  rv_Template <- reactiveValues(
    Template = queryReportTemplate(), 
    AddEdit = "Add", 
    SelectedTemplate = NULL
  )
  
  # Report Template - Passive Observers -----------------------------
  
  # Populate the Logo Choices
  observe({
    current_logo <- input$sel_template_logo

    .choices <- rv_Logo$Logo$OID 
    names(.choices) <- rv_Logo$Logo$FileName
    
    updateSelectInput(session = session, 
                      inputId = "sel_template_logo", 
                      choices = .choices, 
                      selected = current_logo)
  })
  
  observe({
    toggleState(id = "btn_template_add", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id = "btn_template_edit", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_template) > 0)
  })
  
  observe({
    req(rv_Template$SelectedTemplate)
    
    toggleState(id = "btn_template_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_template) > 0 &&
                  !rv_Template$SelectedTemplate$IsActive)
    
    toggleState(id = "btn_template_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_template) > 0 &&
                  rv_Template$SelectedTemplate$IsActive)
  })
  
  # Report Template - Event Observers -------------------------------
  
  observeEvent(
    input$rdo_template, 
    {
      oid <- as.numeric(input$rdo_template)
      
      rv_Template$SelectedTemplate <- 
        rv_Template$Template[rv_Template$Template$OID == oid, ]
    }
  )
  
  observeEvent(
    input$btn_template_add, 
    {
      rv_Template$AddEdit <- "Add"
      
      updateTextInput(session = session, 
                      inputId = "txt_template_title", 
                      value = "")
      updateSelectInput(session = session, 
                        inputId = "sel_template_directory", 
                        selected = TEMPLATE_FOLDERS[1])
      updateSelectInput(session = session, 
                        inputId = "sel_template_file", 
                        selected = character(0))
      updateCheckboxInput(session = session, 
                          inputId = "chk_template_isSignatureRequired", 
                          value = FALSE)
      updateCheckboxInput(session = session, 
                          inputId = "chk_template_isActive", 
                          value = TRUE)
      updateSelectInput(session = session, 
                        inputId = "sel_template_titleSize", 
                        selected = "LARGE")
      updateSelectInput(session = session, 
                        inputId = "sel_template_logo", 
                        selected = character(0))
      
      output$img_template_logo_preview <- NULL
      
      toggleModal(session = session, 
                  modalId = "modal_template_addEdit", 
                  toggle = "open")
    }
  )
  
  observeEvent(
    input$btn_template_edit, 
    {
      rv_Template$AddEdit <- "Edit"
      
      updateTextInput(session = session, 
                      inputId = "txt_template_title", 
                      value = rv_Template$SelectedTemplate$Title)
      updateSelectInput(session = session, 
                        inputId = "sel_template_directory", 
                        selected = rv_Template$SelectedTemplate$TemplateDirectory)
      updateSelectInput(session = session, 
                        inputId = "sel_template_file", 
                        selected = rv_Template$SelectedTemplate$TemplateFile)
      updateCheckboxInput(session = session, 
                          inputId = "chk_template_isSignatureRequired", 
                          value = rv_Template$SelectedTemplate$IsSignatureRequired)
      updateCheckboxInput(session = session, 
                          inputId = "chk_template_isActive", 
                          value = rv_Template$SelectedTemplate$IsActive)
      updateSelectInput(session = session, 
                        inputId = "sel_template_titleSize", 
                        selected = rv_Template$SelectedTemplate$TitleSize)
      updateSelectInput(session = session, 
                        inputId = "sel_template_logo", 
                        selected = rv_Template$SelectedTemplate$LogoFileArchive)
      
      toggleModal(session = session, 
                  modalId = "modal_template_addEdit", 
                  toggle = "open")
    }
  )
  
  observeEvent(
    input$btn_template_addEdit, 
    {
      oid <- if (rv_Template$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_template)
   
      addEditReportTemplate(oid = oid, 
                            template_directory = input$sel_template_directory, 
                            template_file = input$sel_template_file, 
                            title = input$txt_template_title, 
                            title_size = input$sel_template_titleSize, 
                            is_signature_required = input$chk_template_isSignatureRequired, 
                            is_active = input$chk_template_isActive, 
                            logo_oid = as.numeric(input$sel_template_logo), 
                            event_user = CURRENT_USER_OID())
      
      RM_replaceData(query_fun = queryReportTemplate, 
                     reactive_list = rv_Template, 
                     data_slot = "Template", 
                     selected_slot = "SelectedTemplate", 
                     id_variable = "OID", 
                     element_name = "rdo_template", 
                     oid = oid, 
                     proxy = proxy_dt_template, 
                     cols = REPORT_TEMPLATE_DISPLAY_PROPERTIES)
      
      toggleModal(session = session, 
                  modalId = "modal_template_addEdit", 
                  toggle = "close")
    }
  )
  
  observeEvent(
    input$sel_template_directory, 
    {
      req(input$sel_template_directory)
      
      dir <- system.file("ReportTemplate", package = "ReportManager")
      dir <- file.path(dir, input$sel_template_directory)
      
      files <- list.files(dir, 
                          pattern = ".Rmd$")

      updateSelectInput(session = session, 
                        inputId = "sel_template_file", 
                        choices = files, 
                        selected = files[1])
    }
  )
  
  observeEvent(
    input$sel_template_logo, 
    {
      output$img_template_logo_preview <- 
        renderImage({
          display <- input$sel_template_logo != ""
          
          Logo <- queryFromFileArchive(oid = as.numeric(input$sel_template_logo),
                                       file_dir = tempdir())
          
          list(src = Logo$SavedTo,
               height = "100px",
               width = "100px")
        }, deleteFile = TRUE)
    }
  )
  
  observeEvent(
    input$btn_template_activate, 
    {
      oid <- as.numeric(input$rdo_template)
      
      activateRecord(oid, 
                     active = TRUE, 
                     event_user = CURRENT_USER_OID(), 
                     table_name = "ReportTemplate", 
                     event_table_name = "ReportTemplateEvent", 
                     parent_field_name = "ParentReportTemplate")
      
      RM_replaceData(query_fun = queryReportTemplate, 
                     reactive_list = rv_Template, 
                     data_slot = "Template", 
                     selected_slot = "SelectedTemplate", 
                     id_variable = "OID", 
                     element_name = "rdo_template", 
                     oid = oid, 
                     proxy = proxy_dt_template, 
                     cols = REPORT_TEMPLATE_DISPLAY_PROPERTIES)
    }
  )
  
  observeEvent(
    input$btn_template_deactivate, 
    {
      oid <- as.numeric(input$rdo_template)
      
      activateRecord(oid, 
                     active = FALSE, 
                     event_user = CURRENT_USER_OID(), 
                     table_name = "ReportTemplate", 
                     event_table_name = "ReportTemplateEvent", 
                     parent_field_name = "ParentReportTemplate")
      
      RM_replaceData(query_fun = queryReportTemplate, 
                     reactive_list = rv_Template, 
                     data_slot = "Template", 
                     selected_slot = "SelectedTemplate", 
                     id_variable = "OID", 
                     element_name = "rdo_template", 
                     oid = oid, 
                     proxy = proxy_dt_template, 
                     cols = REPORT_TEMPLATE_DISPLAY_PROPERTIES)
    }
  )
  
  
  # Report Template - Output ----------------------------------------
  
  output$dt_template <- 
    DT::renderDataTable({
      queryReportTemplate()[REPORT_TEMPLATE_DISPLAY_PROPERTIES] %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_template") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_template <- DT::dataTableProxy("dt_template")
  
  
  
  # Schedule --------------------------------------------------------
  # Schedule - Reactive Values --------------------------------------
  
  rv_Schedule <- reactiveValues(
    Schedule = querySchedule(), 
    AddEdit = "Add", 
    SelectedSchedule = NULL
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
  
  observeEvent(input$btn_schedule_addEditSchedule,
               OE_btn_schedule_addEditSchedule(session = session, 
                                               rv_Schedule = rv_Schedule, 
                                               input = input, 
                                               current_user_oid = CURRENT_USER_OID(), 
                                               proxy = proxy_dt_schedule))
  
  observeEvent(input$btn_schedule_deactivate, 
               OE_btn_schedule_activateDeactivate(activate = FALSE, 
                                                  rv_Schedule = rv_Schedule, 
                                                  input = input, 
                                                  current_user_oid = CURRENT_USER_OID(), 
                                                  proxy = proxy_dt_schedule))
  
  observeEvent(input$btn_schedule_activate, 
               OE_btn_schedule_activateDeactivate(activate = TRUE, 
                                                  rv_Schedule = rv_Schedule, 
                                                  input = input, 
                                                  current_user_oid = CURRENT_USER_OID(), 
                                                  proxy = proxy_dt_schedule))
  
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
  
  # Date Reporting Format -------------------------------------------
  # Date Reporting Format - Reactive Values -------------------------
  
  rv_DateFormat <- reactiveValues(
    DateFormat = queryDateReportingFormat(), 
    AddEdit = "Add", 
    SelectedDateFormat = NULL
  )
  
  # Date Reporting Format - Passive Observers -----------------------
  
  observe({
    toggleState(id = "btn_dateFormat_addFormat", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id = "btn_dateFormat_editFormat", 
                condition = USER_IS_REPORT_ADMIN() && 
                            length(input$rdo_dateFormat))
  })
  
  observe({
    req(rv_DateFormat$SelectedDateFormat)
    
    toggleState(id = "btn_dateFormat_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_dateFormat) && 
                            !rv_DateFormat$SelectedDateFormat$IsActive)
    
    toggleState(id = "btn_dateFormat_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_dateFormat) && 
                            rv_DateFormat$SelectedDateFormat$IsActive)
  })
  
  # Date Reporting Format - Event Observers -------------------------
  
  observeEvent(input$rdo_dateFormat, 
               OE_rdo_dateFormat(rv_DateFormat = rv_DateFormat, 
                                 input = input))
  
  observeEvent(input$btn_dateFormat_addFormat, 
               OE_dateFormat_addFormat(session = session, 
                                       rv_DateFormat = rv_DateFormat))
  
  observeEvent(input$btn_dateFormat_editFormat, 
               OE_btn_dateFormat_editFormat(session = session, 
                                            rv_DateFormat = rv_DateFormat))
  
  observeEvent(input$btn_dateFormat_addEditFormat, 
               OE_btn_dateFormat_addEditFormat(session = session, 
                                               rv_DateFormat = rv_DateFormat, 
                                               input = input, 
                                               current_user_oid = CURRENT_USER_OID(), 
                                               proxy = proxy_dt_dateFormat))
  
  observeEvent(input$btn_dateFormat_activate, 
               OE_btn_dateFormat_activateDeactivate(activate = TRUE, 
                                                    rv_DateFormat = rv_DateFormat, 
                                                    input = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy = proxy_dt_dateFormat))
  
  observeEvent(input$btn_dateFormat_deactivate, 
               OE_btn_dateFormat_activateDeactivate(activate = FALSE, 
                                                    rv_DateFormat = rv_DateFormat, 
                                                    input = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy = proxy_dt_dateFormat))
  
  # Date Reporting Format - Output ----------------------------------
  
  output$dt_dateFormat <- 
    DT::renderDataTable({
      queryDateReportingFormat() %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_dateFormat") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_dateFormat <- DT::dataTableProxy("dt_dateFormat")
  
  output$title_dateFormat <- 
    renderText({
      if (rv_DateFormat$AddEdit == "Add"){
        "Add New Format"
      } else {
        sprintf("Editing Format '%s'", 
                rv_DateFormat$SelectedDateFormat$FormatName)
      }
    })
  
  # Disclaimer ------------------------------------------------------
  # Disclaimer - Reactive Values ------------------------------------
  
  rv_Disclaimer <- reactiveValues(
    Disclaimer = queryDisclaimer(), 
    AddEdit = "Add", 
    SelectedDisclaimer = NULL
  )
  
  # Disclaimer - Passive Observers ----------------------------------
  
  observe({
    toggleState(id = "btn_disclaimer_add", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id = "btn_disclaimer_edit", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_disclaimer) > 0)
  })
  
  observe({
    req(rv_Disclaimer$SelectedDisclaimer)
    
    toggleState(id = "btn_disclaimer_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_disclaimer) > 0 &&
                  !rv_Disclaimer$SelectedDisclaimer$IsActive)
    
    toggleState(id = "btn_disclaimer_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_disclaimer) > 0 &&
                  rv_Disclaimer$SelectedDisclaimer$IsActive)
  })
  
  # Disclaimer - Event Observers ------------------------------------
  
  observeEvent(input$rdo_disclaimer, 
               OE_rdo_disclaimer(rv_Disclaimer, 
                                 input))
  
  observeEvent(input$btn_disclaimer_add, 
               OE_btn_disclaimer_add(session = session, 
                                     rv_Disclaimer = rv_Disclaimer, 
                                     input = input))
  
  observeEvent(input$btn_disclaimer_edit, 
               OE_btn_disclaimer_edit(session = session, 
                                      rv_Disclaimer = rv_Disclaimer, 
                                      input = input))
  
  observeEvent(input$btn_disclaimer_addEditDisclaimer, 
               OE_btn_disclaimer_addEditDisclaimer(session = session, 
                                                   rv_Disclaimer = rv_Disclaimer, 
                                                   input = input, 
                                                   current_user_oid = CURRENT_USER_OID(), 
                                                   proxy = proxy_dt_disclaimer))
  
  observeEvent(input$btn_disclaimer_activate,
               OE_btn_disclaimer_activateDeactivate(activate = TRUE, 
                                                    rv_Disclaimer = rv_Disclaimer, 
                                                    input = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy = proxy_dt_disclaimer))
  
  observeEvent(input$btn_disclaimer_deactivate, 
               OE_btn_disclaimer_activateDeactivate(activate = FALSE, 
                                                    rv_Disclaimer = rv_Disclaimer, 
                                                    input = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy = proxy_dt_disclaimer))
  
  # Disclaimer - Output ---------------------------------------------
  
  output$dt_disclaimer <- 
    DT::renderDataTable({
      queryDisclaimer() %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_disclaimer") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_disclaimer <- DT::dataTableProxy("dt_disclaimer")
  
  output$title_disclaimer_addEdit <- 
    renderText({
      if (rv_Disclaimer$AddEdit == "Add"){
        "Add New Disclaimer"
      } else {
        sprintf("Editing Disclaimer %s", 
                rv_Disclaimer$SelectedDisclaimer$OID)
      }
    })
  
  # Footer ----------------------------------------------------------
  # Footer - Reactive Values ----------------------------------------
  
  rv_Footer <- reactiveValues(
    Footer = queryFooter(), 
    AddEdit = "Add", 
    SelectedFooter = NULL
  )
  
  # Footer - Passive Observers --------------------------------------
  
  observe({
    toggleState(id = "btn_footer_add", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id = "btn_footer_edit", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_footer) > 0)
  })
  
  observe({
    req(rv_Footer$SelectedFooter)
    
    toggleState(id = "btn_footer_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_footer) > 0 &&
                  !rv_Footer$SelectedFooter$IsActive)
    
    toggleState(id = "btn_footer_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_footer) > 0 &&
                  rv_Footer$SelectedFooter$IsActive)
  })
  
  # Footer - Event Observers ----------------------------------------
  
  observeEvent(input$rdo_footer, 
               OE_rdo_footer(rv_Footer, 
                                 input))
  
  observeEvent(input$btn_footer_add, 
               OE_btn_footer_add(session = session, 
                                     rv_Footer = rv_Footer, 
                                     input = input))
  
  observeEvent(input$btn_footer_edit, 
               OE_btn_footer_edit(session = session, 
                                      rv_Footer = rv_Footer, 
                                      input = input))
  
  observeEvent(input$btn_footer_addEditFooter, 
               OE_btn_footer_addEditFooter(session = session, 
                                                   rv_Footer = rv_Footer, 
                                                   input = input, 
                                                   current_user_oid = CURRENT_USER_OID(), 
                                                   proxy = proxy_dt_footer))
  
  observeEvent(input$btn_footer_activate,
               OE_btn_footer_activateDeactivate(activate = TRUE, 
                                                    rv_Footer = rv_Footer, 
                                                    input = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy = proxy_dt_footer))
  
  observeEvent(input$btn_footer_deactivate, 
               OE_btn_footer_activateDeactivate(activate = FALSE, 
                                                    rv_Footer = rv_Footer, 
                                                    input = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy = proxy_dt_footer))
  
  # Footer - Output -------------------------------------------------
  
  output$dt_footer <- 
    DT::renderDataTable({
      queryFooter() %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_footer") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_footer <- DT::dataTableProxy("dt_footer")
  
  output$title_footer_addEdit <- 
    renderText({
      if (rv_Footer$AddEdit == "Add"){
        "Add New Footer"
      } else {
        sprintf("Editing Footer %s", 
                rv_Footer$SelectedFooter$OID)
      }
    })
  
  # Logo ------------------------------------------------------------
  # Logo - Reactive Values ------------------------------------------
  
  rv_Logo <- reactiveValues(
    Logo = queryLogo(), 
    AddEdit = "Add", 
    SelectedLogo = NULL
  )
  
  # Logo - Passive Observers ----------------------------------------
  
  observe({
    toggleState("btn_logo_add", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState("btn_logo_edit", 
                condition = USER_IS_REPORT_ADMIN() && 
                  length(input$rdo_logo) > 0)
    
    hide("btn_logo_activate")
    hide("btn_logo_deactivate")
  })
  
  # Logo - Event Observers ------------------------------------------
  
  observeEvent(input$rdo_logo, 
               OE_rdo_logo(rv_Logo, input))
  
  observeEvent(input$btn_logo_add, 
               OE_btn_logo_add(session = session, 
                               rv_Logo = rv_Logo))
  
  observeEvent(input$btn_logo_edit,
               OE_btn_logo_edit(session = session, 
                                rv_Logo = rv_Logo))
  
  observeEvent(input$file_logo_add,
               OE_file_logo_add(session = session, 
                                input = input))
  
  observeEvent(input$btn_logo_addEdit, 
               OE_btn_logo_addEdit(session = session, 
                                   rv_Logo = rv_Logo, 
                                   input = input, 
                                   proxy = proxy_dt_logo))
  
  # Logo - Output ---------------------------------------------------
  
  output$dt_logo <- 
    DT::renderDataTable({
      queryLogo() %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_logo") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_logo <- DT::dataTableProxy("dt_logo")
  
  output$img_logo_preview <- 
    renderImage({
      filepath <- 
        if (rv_Logo$AddEdit == "Add"){
          req(input$file_logo_add)
          input$file_logo_add$datapath
        } else {
          File <- queryFromFileArchive(rv_Logo$SelectedLogo$OID, 
                                       file_dir = tempdir())
          File$SavedTo
        }
      
      list(src = filepath, 
           width = "100px", 
           height = "100px", 
           alt = "logo image could not be displayed")      
    }, deleteFile = FALSE)
  
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
