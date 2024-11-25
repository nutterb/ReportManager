shinyServer(function(input, output, session){
  
  # Global ----------------------------------------------------------
  
  # Global - Reactive Values ----------------------------------------
  
  CURRENT_USER_OID <- 
    reactive({
      req(rv_User$User)
      ..rtv_currentUserOid(rv_User)
    })
  
  CURRENT_USER_ROLE <- 
    reactive({
      req(CURRENT_USER_OID())
      ..rtv_currentUserRole(current_user_oid = CURRENT_USER_OID())
    })
  
  USER_IS_USER_ADMIN <- 
    reactive({
      req(CURRENT_USER_ROLE())
      ..rtv_userHasRole(role              = "UserAdministrator", 
                        current_user_role = CURRENT_USER_ROLE())
    })
  
  USER_IS_REPORT_ADMIN <- 
    reactive({
      req(CURRENT_USER_ROLE())
      ..rtv_userHasRole(role              = "ReportAdministrator", 
                        current_user_role = CURRENT_USER_ROLE())
    })
  
  # Global - Passive Observer ---------------------------------------
  # Global - Event Observer -----------------------------------------
  
  # Generate Report -------------------------------------------------
  # Generate Report - Reactive Values -------------------------------
  
  rv_GenerateReport <- reactiveValues(
    Templates = queryReportSelection(), 
    SelectedTemplate = NULL, 
    
    ScheduledReportInstance = queryReportInstance(report_template_oid = -1),
    SelectedScheduledReportInstance = numeric(0), 
    
    UnscheduledReportInstance = queryReportInstance(report_template_oid = -1),
    SelectedUnscheduledReportInstance = numeric(0), 
  )
  
  selected_instance_oid <- reactive({
    if (input$cd_genReport_scheduledReport){
      rv_GenerateReport$SelectedScheduledReportInstance
    } else if (input$cd_genReport_unscheduledReport){
      rv_GenerateReport$SelectedUnscheduledReportInstance
    } else {
      -1
    }
  })
  
  # Generate Report - Template --------------------------------------
  
  # Generate Report - Template - Event Observers --------------------
  
  observeEvent(
    input$rdo_genReport_template, 
    {
      ..rdo_genReport_template(
        rdo_genReport_template = input$rdo_genReport_template, 
        current_user_oid       = CURRENT_USER_OID(), 
        rv_GenerateReport      = rv_GenerateReport
      )
    }
  )
  
  # Generate Report - Template - Output -----------------------------
  
  output$dt_genReport_template <- 
    DT::renderDataTable({
      ..out_dt_genReport_template(rv_GenerateReport)
    })
  
  # Generate Report - Instance --------------------------------------
  # Generate Report - Instance - Passive Observer -------------------
  
  observe({
    toggleState(id        = "btn_genReport_unscheduled_changeSignatureRequirement", 
                condition = length(rv_GenerateReport$SelectedUnscheduledReportInstance) > 0)
    toggle(id        = "h3_genReport_reportInstanceNote_noInstanceSelected", 
           condition = length(selected_instance_oid()) == 0)
    toggle(id        = "div_genReport_reportInstanceNote", 
           condition = length(selected_instance_oid()) > 0)
  })
  
  # Generate Report - Instance - Event Observer ---------------------
  
  observeEvent(
    input$cd_genReport_scheduledReport, 
    {
      ..cd_genReport_scheduledReport(
        cd_genReport_scheduledReport = input$cd_genReport_scheduledReport, 
        session                      = session
      )
    }
  )
  
  observeEvent(
    input$rdo_report_instance_scheduled, 
    {
      rv_GenerateReport$SelectedScheduledReportInstance <- 
        as.numeric(input$rdo_report_instance_scheduled)
    }
  )
  
  observeEvent(
    input$cd_genReport_unscheduledReport, 
    {
      ..cd_genReport_unscheduledReport(
        cd_genReport_unscheduledReport = input$cd_genReport_unscheduledReport, 
        session                        = session
      )
    }
  )
  
  observeEvent(
    input$rdo_report_instance_unscheduled, 
    {
      rv_GenerateReport$SelectedUnscheduledReportInstance <- 
        as.numeric(input$rdo_report_instance_unscheduled)
    }
  )
  
  observeEvent(
    input$cd_genReport_adhocReport, 
    {
      ..cd_genReport_adhocReport(
        cd_genReport_adhocReport = input$cd_genReport_adhocReport, 
        session                  = session
      )
    }
  )
  
  observeEvent(
    input$btn_genReport_unscheduled_addUnscheduledReport, 
    {
      ..btn_geneport_unscheduled_addUnscheduledReport(
        dttm_genReport_newUnscheduledInstance         = input$dttm_genReport_newUnscheduledInstance, 
        chk_genReport_unscheduled_isSignatureRequired = input$chk_genReport_unscheduled_isSignatureRequired, 
        rv_GenerateReport                             = rv_GenerateReport, 
        current_user_oid                              = CURRENT_USER_OID()
      )
    }
  )
  
  observeEvent(
    input$dt_instance_unscheduled_cell_edit,
    {
      ..dt_instance_unscheduled_cell_edit(
        dt_instance_unscheduled_cell_edit = input$dt_instance_unscheduled_cell_edit,
        rv_GenerateReport                 = rv_GenerateReport, 
        current_user_oid                  = CURRENT_USER_OID()
      )
    }
  )
  
  observeEvent(
    input$btn_genReport_unscheduled_changeSignatureRequirement, 
    {
      ..btn_genReport_unscheduled_changeSignatureRequirement(
        rv_GenerateReport = rv_GenerateReport, 
        current_user_oid  = CURRENT_USER_OID()
      )
    }
  )
  
  # Generate Report - Instance - Output -----------------------------
  
  output$dt_instance_scheduled <- 
    DT::renderDataTable({
      req(input$cd_genReport_scheduledReport)
      ..out_dt_instance_scheduled(rv_GenerateReport = rv_GenerateReport)
    })
  
  proxy_dt_instance_scheduled <- DT::dataTableProxy("dt_instance_scheduled")
  
  
  
  output$dt_instance_unscheduled <- 
    DT::renderDataTable({
      req(input$cd_genReport_unscheduledReport)
      ..out_dt_instance_unscheduled(rv_GenreateReport = rv_GenerateReport)
    })
  
  proxy_dt_instance_unscheduled <- DT::dataTableProxy("dt_instance_unscheduled")
  
  # Generate Report - Notes -----------------------------------------
  # Generate Report - Notes - Passive Observer ----------------------
  
  observe({
    toggle(id        = "txt_reportInstanceNote", 
           condition = length(selected_instance_oid()) > 0 & 
                        isTRUE(selected_instance_oid() > 0))
    toggle(id        = "btn_addReportInstanceNote", 
           condition = length(selected_instance_oid()) > 0 & 
                        isTRUE(selected_instance_oid() > 0))
  })
  
  # Generate Report - Notes - Event Observer ------------------------
  
  observeEvent(
    input$btn_addReportInstanceNote,
    {
      req(isTRUE(selected_instance_oid() > 0))
      ..btn_addReportInstanceNote(
        selected_instance_oid       = selected_instance_oid(), 
        current_user_oid            = CURRENT_USER_OID(), 
        txt_reportInstanceNote      = input$txt_reportInstanceNote, 
        proxy_dt_reportInstanceNote = proxy_dt_reportInstanceNote, 
        session                     = session
      )
    }
  )
  
  # Generate Report - Notes - Output --------------------------------
  
  output$dt_reportInstanceNote <- 
    DT::renderDataTable({
      req(isTRUE(selected_instance_oid() > 0))
      DT::datatable(
        queryReportInstanceNote(selected_instance_oid()), 
        rownames = FALSE
      )%>% 
        DT::formatDate(columns = c("NoteDateTime"),
                       method  = 'toLocaleTimeString',
                       params  = list('en-gb',
                                      list(year     = 'numeric',
                                           month    = 'short',
                                           day      = 'numeric',
                                           hour     = 'numeric',
                                           minute   = 'numeric',
                                           second   = 'numeric',
                                           timeZone = 'UTC')))
    })
  
  proxy_dt_reportInstanceNote <- DT::dataTableProxy("dt_reportInstanceNote")
  
  # Generate Report - Narrative -------------------------------------
  
  # Generate Report - Signatures ------------------------------------
  # Generate Report - Preview ---------------------------------------
  # Generate Report - Archival and Submission -----------------------
  # Generate Report - Archived Reports ------------------------------
  
  # Report Template -------------------------------------------------
  # Report Template - Reactive Values -------------------------------
  
  rv_Template <- reactiveValues(
    Template                     = queryReportTemplate(), 
    AddEdit                      = "Add", 
    SelectedTemplate             = NULL, 
    SelectedTemplateSchedule     = NULL, 
    SelectedTemplateDisclaimer   = NULL, 
    SelectedTemplateFooter       = NULL,
    SelectedTemplateSignature    = NULL, 
    SelectedTemplateDistribution = NULL, 
    SelectedTemplatePermission   = numeric(0), 
    TemplatePermissionAddEdit    = "Add"
  )
  
  # Report Template - Passive Observers -----------------------------
  
  # Populate the Logo Choices
  observe({
    current_logo <- input$sel_template_logo

    .choices <- rv_Logo$Logo$OID 
    names(.choices) <- rv_Logo$Logo$FileName
    
    updateSelectInput(session  = session, 
                      inputId  = "sel_template_logo", 
                      choices  = .choices, 
                      selected = current_logo)
  })
  
  observe({
    toggleState(id        = "btn_template_add", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id        = "btn_template_edit", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_template) > 0)
  })
  
  observe({
    req(rv_Template$SelectedTemplate)
    
    toggleState(id        = "btn_template_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_template) > 0 &&
                            !rv_Template$SelectedTemplate$IsActive)
    
    toggleState(id        = "btn_template_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_template) > 0 &&
                            rv_Template$SelectedTemplate$IsActive)
  })
  
  # Report Template - Event Observers -------------------------------
  
  observeEvent(
    input$rdo_template, 
    {
      ..rdo_template(rdo_template = input$rdo_template, 
                     rv_Template  = rv_Template, 
                     rv_Scheudle  = rv_Schedule)
    }
  )
  
  observeEvent(input$btn_template_add, 
               ..btn_template_add(session      = session, 
                                   rv_Template = rv_Template, 
                                   output      = output))
  
  observeEvent(input$btn_template_edit, 
               ..btn_template_edit(session      = session,
                                    output      = output,
                                    rv_Template = rv_Template))
  
  observeEvent(input$btn_template_addEdit, 
               ..btn_template_add_edit(session           = session, 
                                        rv_Template      = rv_Template, 
                                        input            = input, 
                                        current_user_oid = CURRENT_USER_OID(), 
                                        proxy            = proxy_dt_template))
  
  observeEvent(input$sel_template_directory, 
               ..sel_template_directory(session = session, 
                                         input  = input))
  
  observeEvent(input$sel_template_logo, 
               ..sel_template_logo(input   = input, 
                                    output = output))
  
  observeEvent(input$btn_template_activate, 
               ..btn_template_activate_deactivate(activate          = TRUE, 
                                                   input            = input, 
                                                   current_user_oid = CURRENT_USER_OID(), 
                                                   rv_Template      = rv_Template, 
                                                   proxy            = proxy_dt_template))
  
  observeEvent(input$btn_template_deactivate, 
               ..btn_template_activate_deactivate(activate          = FALSE, 
                                                   input            = input, 
                                                   current_user_oid = CURRENT_USER_OID(), 
                                                   rv_Template      = rv_Template, 
                                                   proxy            = proxy_dt_template))
  
  
  # Report Template - Output ----------------------------------------
  
  output$dt_template <- 
    DT::renderDataTable({
      queryReportTemplate()[REPORT_TEMPLATE_DISPLAY_PROPERTIES] %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_template") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_template <- DT::dataTableProxy("dt_template")
  
  output$title_reportTemplateTabset <- 
    renderUI({
      txt <- 
        if (length(input$rdo_template) == 0){
          "No Template Selected"
        } else {
          sprintf("Template Selected: %s", 
                  rv_Template$SelectedTemplate$Title)
        }
      h3(txt)
    })
  
  output$title_reportTemplateModal <- 
    renderText({
      if (rv_Template$AddEdit == "Add"){
        "Add a new template"
      } else {
        sprintf("Editing Template '%s'", 
                rv_Template$SelectedTemplate$Title)
      }
    })
  
  # Report Template Schedule ----------------------------------------
  # Report Template Schedule - Passive Observers --------------------
  
  observe({
    toggleState(id        = "btn_templateSchedule_edit", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_template) > 0)
  })

  observe({
    sched <- input$sel_templateSchedule
    ThisSchedule <- rv_Schedule$Schedule[rv_Schedule$Schedule$OID == as.numeric(sched), ]

    toggle(id        = "dttm_templateIndexDateTime", 
           condition = isTRUE(ThisSchedule$IsPeriodToDate))
    toggle(id        = "dttm_templateIndexDateTime-label", 
           condition = isTRUE(ThisSchedule$IsPeriodToDate))
  })
  
  # Report Template Schedule - Event Observers ----------------------
  
  observeEvent(
    input$btn_templateSchedule_edit, 
    {
      enable("sel_templateSchedule")
      enable("dttm_templateSchedule")
      enable("dttm_templateIndexDateTime")
      enable("btn_templateSchedule_save")
    }
  )
  
  observeEvent(
    input$btn_templateSchedule_save, 
    {
      addEditReportTemplateSchedule(
        oid                    = rv_Template$SelectedTemplateSchedule$OID,
        parent_report_template = as.numeric(input$rdo_template),
        parent_schedule        = as.numeric(input$sel_templateSchedule),
        start_date             = as.POSIXct(input$dttm_templateSchedule, 
                                            format = "%d-%b-%Y %H:%M", 
                                            tz     = "UTC"),
        index_date             = as.POSIXct(input$dttm_templateIndexDateTime, 
                                            format = "%d-%b-%Y %H:%M", 
                                            tz     = "UTC"),
        is_active              = TRUE, 
        event_user             = CURRENT_USER_OID()
      )
      
      disable("sel_templateSchedule")
      disable("dttm_templateSchedule")
      disable("btn_templateSchedule_save")
    }
  )
  
  # Report Template Layout ------------------------------------------
  # Report Template Layout - Passive Observers ----------------------
  
  observe({
    toggleState(id        = "btn_templateDisclaimer_edit", 
                condition = USER_IS_REPORT_ADMIN() & 
                            length(input$rdo_template) > 0)
    
    toggleState(id        = "btn_templateFooter_edit", 
                condition = USER_IS_REPORT_ADMIN() & 
                            length(input$rdo_template) > 0)
  })

  
  # Report Template Layout - Event Observers ------------------------
  
  observeEvent(
    input$btn_templateDisclaimer_edit, 
    {
      ..btn_templateDisclaimer_edit(rv_Template   = rv_Template, 
                                    rv_Disclaimer = rv_Disclaimer, 
                                    session       = session)
    }
  )
  
  observeEvent(input$templateDisclaimer_move_all_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDisclaimer", 
                                 input   = input,
                                 action  = "move_all_right"))
  
  observeEvent(input$templateDisclaimer_move_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDisclaimer", 
                                 input   = input,
                                 action  = "move_right"))
  
  observeEvent(input$templateDisclaimer_move_all_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDisclaimer", 
                                 input   = input,
                                 action  = "move_all_left"))
  
  observeEvent(input$templateDisclaimer_move_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDisclaimer", 
                                 input   = input,
                                 action  = "move_left"))
  
  observeEvent(input$templateDisclaimer_move_up, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDisclaimer", 
                                 input   = input,
                                 action  = "move_up"))
  
  observeEvent(input$templateDisclaimer_move_down, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDisclaimer", 
                                 input   = input,
                                 action  = "move_down"))
  
  observeEvent(input$btn_templateDisclaimer_addEdit,
               ..btn_templateDisclaimer_addEdit(session           = session,
                                                 input            = input, 
                                                 rv_Template      = rv_Template,
                                                 current_user_oid = CURRENT_USER_OID(),
                                                 proxy            = proxy_dt_templateDisclaimer))
  
  
  observeEvent(
    input$btn_templateFooter_edit, 
    {
      ..btn_templateFooter_edit(rv_Template = rv_Template, 
                                rv_Footer   = rv_Footer, 
                                session     = session)
    }
  )
  
  observeEvent(input$templateFooter_move_all_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateFooter", 
                                 input   = input,
                                 action  = "move_all_right"))
  
  observeEvent(input$templateFooter_move_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateFooter", 
                                 input   = input,
                                 action  = "move_right"))
  
  observeEvent(input$templateFooter_move_all_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateFooter", 
                                 input   = input,
                                 action  = "move_all_left"))
  
  observeEvent(input$templateFooter_move_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateFooter", 
                                 input   = input,
                                 action  = "move_left"))
  
  observeEvent(input$templateFooter_move_up, 
               updateMultiSelect(session = session, 
                                 inputId = "templateFooter", 
                                 input   = input,
                                 action  = "move_up"))
  
  observeEvent(input$templateFooter_move_down, 
               updateMultiSelect(session = session, 
                                 inputId = "templateFooter", 
                                 input   = input,
                                 action  = "move_down"))
  
  observeEvent(input$btn_templateFooter_addEdit,
               ..btn_templateFooter_addEdit(session           = session,
                                             input            = input, 
                                             rv_Template      = rv_Template,
                                             current_user_oid = CURRENT_USER_OID(),
                                             proxy            = proxy_dt_templateFooter))
  
  # Report Template Layout - Output ---------------------------------
  
  output$dt_templateDisclaimer <- 
    DT::renderDataTable({
      req(rv_Template$SelectedTemplateDisclaimer)
      makeTemplateDisclaimerData(rv_Template$SelectedTemplateDisclaimer, 
                                 rv_Disclaimer$Disclaimer) %>% 
        RM_datatable()
    })
  
  proxy_dt_templateDisclaimer <- DT::dataTableProxy("dt_templateDisclaimer")
  
  
  output$dt_templateFooter <- 
    DT::renderDataTable({
      req(rv_Template$SelectedTemplateFooter)

      makeTemplateFooterData(rv_Template$SelectedTemplateFooter, 
                                 rv_Footer$Footer) %>% 
        RM_datatable()
    })
  
  proxy_dt_templateFooter <- DT::dataTableProxy("dt_templateFooter")
  
  
  # Report Template Signature ---------------------------------------
  # Report Template Signature - Passive Observers -------------------
  
  observe({
    toggleState(id        = "btn_templateSignature_edit", 
                condition = USER_IS_REPORT_ADMIN() & 
                            length(input$rdo_template) > 0)
  })
  
  # Report Template Signature - Event Observers ---------------------
  
  observeEvent(
    input$btn_templateSignature_edit, 
    {
      ..btn_templateSignature_edit(rv_Template = rv_Template, 
                                   rv_Roles    = rv_Roles, 
                                   session     = session)
    }
  )
  
  observeEvent(input$templateSignature_move_all_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateSignature", 
                                 input   = input,
                                 action  = "move_all_right"))
  
  observeEvent(input$templateSignature_move_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateSignature", 
                                 input   = input,
                                 action  = "move_right"))
  
  observeEvent(input$templateSignature_move_all_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateSignature", 
                                 input   = input,
                                 action  = "move_all_left"))
  
  observeEvent(input$templateSignature_move_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateSignature", 
                                 input   = input,
                                 action  = "move_left"))
  
  observeEvent(input$templateSignature_move_up, 
               updateMultiSelect(session = session, 
                                 inputId = "templateSignature", 
                                 input   = input,
                                 action  = "move_up"))
  
  observeEvent(input$templateSignature_move_down, 
               updateMultiSelect(session = session, 
                                 inputId = "templateSignature", 
                                 input   = input,
                                 action  = "move_down"))
  
  observeEvent(input$btn_templateSignature_addEdit,
               ..btn_templateSignature_addEdit(session           = session,
                                                input            = input, 
                                                rv_Template      = rv_Template,
                                                current_user_oid = CURRENT_USER_OID(),
                                                proxy            = proxy_dt_templateSignature))
  
  
  # Report Template Signature - Output ------------------------------
  
  output$dt_templateSignature <- 
    DT::renderDataTable({
      req(rv_Template$SelectedTemplateSignature)
      
      makeTemplateSignatureData(rv_Template$SelectedTemplateSignature, 
                                rv_Roles$Roles) %>% 
        RM_datatable()
    })
  
  proxy_dt_templateSignature <- DT::dataTableProxy("dt_templateSignature")
  
  
  # Report Template Distribution ------------------------------------
  # Report Template Distribution - Passive Observers ----------------
  
  observe({
    toggleState(id        = "btn_templateDistribution_edit", 
                condition = USER_IS_REPORT_ADMIN() & 
                            length(input$rdo_template) > 0)
  })
  
  # Report Template Distribution - Event Observers ------------------
  
  observeEvent(
    input$btn_templateDistribution_edit, 
    {
      ..btn_templateDistribution_edit(rv_Template = rv_Template, 
                                      rv_User     = rv_User, 
                                      session     = session)
    }
  )
  
  observeEvent(input$templateDistributionUser_move_all_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDistributionUser", 
                                 input   = input,
                                 action  = "move_all_right"))
  
  observeEvent(input$templateDistributionUser_move_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDistributionUser", 
                                 input   = input,
                                 action  = "move_right"))
  
  observeEvent(input$templateDistributionUser_move_all_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDistributionUser", 
                                 input   = input,
                                 action  = "move_all_left"))
  
  observeEvent(input$templateDistributionUser_move_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDistributionUser", 
                                 input   = input,
                                 action  = "move_left"))
  
  observeEvent(input$templateDistributionRole_move_all_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDistributionRole", 
                                 input   = input,
                                 action  = "move_all_right"))
  
  observeEvent(input$templateDistributionRole_move_right, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDistributionRole", 
                                 input   = input,
                                 action  = "move_right"))
  
  observeEvent(input$templateDistributionRole_move_all_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDistributionRole", 
                                 input   = input,
                                 action  = "move_all_left"))
  
  observeEvent(input$templateDistributionRole_move_left, 
               updateMultiSelect(session = session, 
                                 inputId = "templateDistributionRole", 
                                 input   = input,
                                 action  = "move_left"))
  
  observeEvent(input$btn_templateDistribution_addEdit, 
               {
                 ..btn_templateDistribution_addEdit(
                   templateDistributionUser = input$templateDistributionUser,
                   templateDistributionRole = input$templateDistributionRole, 
                   rdo_template             = input$rdo_template, 
                   session                  = session
                 )
                 
               })
  
  
  # Report Template Distribution - Output ---------------------------
  
  output$dt_templateDistribution <- 
    DT::renderDataTable({
      req(rv_Template$SelectedTemplateDistribution)
      
      makeTemplateDistributionData(as.numeric(input$rdo_template)) %>% 
        RM_datatable()
    })
  
  proxy_dt_templateDistribution <- DT::dataTableProxy("dt_templateDistribution")
  
  # Report Template Permission - Passive Observers ------------------
  
  observe({
    toggleState(id        = "btn_templatePermission_add", 
                condition = USER_IS_REPORT_ADMIN() & 
                            length(input$rdo_template) > 0)
    
    toggleState(id        = "btn_templatePermission_edit", 
                condition = USER_IS_REPORT_ADMIN() & 
                            length(input$rdo_templatePermission) > 0)
  })
  
  # Report Template Permission - Event Observers --------------------
  
  observeEvent(
    input$rdo_templatePermission, 
    {
      rv_Template$SelectedTemplatePermission <- 
        as.numeric(input$rdo_templatePermission)
    }
  )
  
  observeEvent(
    input$btn_templatePermission_add,
    {
      ..btn_templatePermission_add(rdo_template = input$rdo_template, 
                                   rv_Template  = rv_Template, 
                                   rv_Roles     = rv_Roles,
                                   session      = session)
    }
  )
  
  observeEvent(
    input$btn_templatePermission_edit, 
    {
      ..btn_templatePermission_edit(rv_Template = rv_Template, 
                                    rv_Roles    = rv_Roles, 
                                    session     = session)
    }
  )
  
  observeEvent(
    input$btn_saveTemplatePermission, 
    {
      ..btn_saveTemplatePermission(
        rdo_template = input$rdo_template, 
        rdo_templatePermission = input$rdo_templatePermission,
        sel_templatePermissionRole = input$sel_templatePermissionRole, 
        chkgrp_templatePermission = input$chkgrp_templatePermission, 
        rv_Template = rv_Template, 
        proxy_dt_templatePermission = proxy_dt_templatePermission, 
        current_user_oid = CURRENT_USER_OID(), 
        session = session)
    }
  )
  
  # Report Template Permission - Output -----------------------------
  
  output$dt_templatePermission <- 
    DT::renderDataTable({
      req(rv_Template$SelectedTemplate)
      
      makeTemplatePermissionData(as.numeric(input$rdo_template)) %>% 
        radioDataTable(id_variable  = "OID", 
                       element_name = "rdo_templatePermission") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_templatePermission <- DT::dataTableProxy("dt_templatePermission")
  
  # Schedule --------------------------------------------------------
  # Schedule - Reactive Values --------------------------------------
  
  rv_Schedule <- reactiveValues(
    Schedule         = querySchedule(), 
    AddEdit          = "Add", 
    SelectedSchedule = NULL
  )
  
  # Schedule - Passive Observers ------------------------------------
  
  observe({
    toggleState(id        = "btn_schedule_addSchedule", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id        = "btn_schedule_editSchedule", 
                condition = USER_IS_REPORT_ADMIN() &&
                  length(input$rdo_schedule) > 0)
  })
  
  observe({
    req(rv_Schedule$SelectedSchedule)
    
    toggleState(id        = "btn_schedule_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_schedule) > 0 &&
                            isFALSE(rv_Schedule$SelectedSchedule$IsActive))
    
    toggleState(id        = "btn_schedule_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_schedule) > 0 &&
                            isTRUE(rv_Schedule$SelectedSchedule$IsActive))
  })
  
  # Schedule - Event Observers --------------------------------------
  
  observeEvent(input$rdo_schedule, 
               ..rdo_schedule(rv_Schedule, 
                               input))
  
  observeEvent(input$btn_schedule_addSchedule, 
               ..btn_schedule_addSchedule(session, 
                                           rv_Schedule))
  
  observeEvent(input$btn_schedule_editSchedule,
               ..btn_schedule_editSchedule(session, 
                                            rv_Schedule))
  
  observeEvent(input$btn_schedule_addEditSchedule,
               ..btn_schedule_addEditSchedule(session           = session, 
                                               rv_Schedule      = rv_Schedule, 
                                               input            = input, 
                                               current_user_oid = CURRENT_USER_OID(), 
                                               proxy            = proxy_dt_schedule))
  
  observeEvent(input$btn_schedule_deactivate, 
               ..btn_schedule_activateDeactivate(activate          = FALSE, 
                                                  rv_Schedule      = rv_Schedule, 
                                                  input            = input, 
                                                  current_user_oid = CURRENT_USER_OID(), 
                                                  proxy            = proxy_dt_schedule))
  
  observeEvent(input$btn_schedule_activate, 
               ..btn_schedule_activateDeactivate(activate          = TRUE, 
                                                  rv_Schedule      = rv_Schedule, 
                                                  input            = input, 
                                                  current_user_oid = CURRENT_USER_OID(), 
                                                  proxy            = proxy_dt_schedule))
  
  # Schedule - Output -----------------------------------------------
  
  output$dt_schedule <- 
    DT::renderDataTable({
      querySchedule() %>% 
        radioDataTable(id_variable  = "OID", 
                       element_name = "rdo_schedule") %>% 
        RM_datatable(escape = -1)
    })
  
  proxy_dt_schedule <- dataTableProxy("dt_schedule")
  
  output$title_schedule_addEdit <- 
    renderText({
      if (rv_Schedule$AddEdit == "Add"){
        "Add a New Schedule"
      } else {
        sprintf("Editing Schedule: %s (%s)", 
                rv_Schedule$SelectedSchedule$ScheduleName, 
                rv_Schedule$SelectedSchedule$OID)
      }
    })
  
  # Date Reporting Format -------------------------------------------
  # Date Reporting Format - Reactive Values -------------------------
  
  rv_DateFormat <- reactiveValues(
    DateFormat         = queryDateReportingFormat(), 
    AddEdit            = "Add", 
    SelectedDateFormat = NULL
  )
  
  # Date Reporting Format - Passive Observers -----------------------
  
  observe({
    toggleState(id        = "btn_dateFormat_addFormat", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id        = "btn_dateFormat_editFormat", 
                condition = USER_IS_REPORT_ADMIN() && 
                            length(input$rdo_dateFormat))
  })
  
  observe({
    req(rv_DateFormat$SelectedDateFormat)
    
    toggleState(id        = "btn_dateFormat_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_dateFormat) && 
                            !rv_DateFormat$SelectedDateFormat$IsActive)
    
    toggleState(id        = "btn_dateFormat_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_dateFormat) && 
                            rv_DateFormat$SelectedDateFormat$IsActive)
  })
  
  # Date Reporting Format - Event Observers -------------------------
  
  observeEvent(input$rdo_dateFormat, 
               ..rdo_dateFormat(rv_DateFormat = rv_DateFormat, 
                                 input        = input))
  
  observeEvent(input$btn_dateFormat_addFormat, 
               ..dateFormat_addFormat(session        = session, 
                                       rv_DateFormat = rv_DateFormat))
  
  observeEvent(input$btn_dateFormat_editFormat, 
               ..btn_dateFormat_editFormat(session        = session, 
                                            rv_DateFormat = rv_DateFormat))
  
  observeEvent(input$btn_dateFormat_addEditFormat, 
               ..btn_dateFormat_addEditFormat(session           = session, 
                                               rv_DateFormat    = rv_DateFormat, 
                                               input            = input, 
                                               current_user_oid = CURRENT_USER_OID(), 
                                               proxy            = proxy_dt_dateFormat))
  
  observeEvent(input$btn_dateFormat_activate, 
               ..btn_dateFormat_activateDeactivate(activate          = TRUE, 
                                                    rv_DateFormat    = rv_DateFormat, 
                                                    input            = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy            = proxy_dt_dateFormat))
  
  observeEvent(input$btn_dateFormat_deactivate, 
               ..btn_dateFormat_activateDeactivate(activate          = FALSE, 
                                                    rv_DateFormat    = rv_DateFormat, 
                                                    input            = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy            = proxy_dt_dateFormat))
  
  # Date Reporting Format - Output ----------------------------------
  
  output$dt_dateFormat <- 
    DT::renderDataTable({
      queryDateReportingFormat() %>% 
        radioDataTable(id_variable  = "OID", 
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
    Disclaimer         = queryDisclaimer(), 
    AddEdit            = "Add", 
    SelectedDisclaimer = NULL
  )
  
  # Disclaimer - Passive Observers ----------------------------------
  
  observe({
    toggleState(id        = "btn_disclaimer_add", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id        = "btn_disclaimer_edit", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_disclaimer) > 0)
  })
  
  observe({
    req(rv_Disclaimer$SelectedDisclaimer)
    
    toggleState(id        = "btn_disclaimer_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_disclaimer) > 0 &&
                            !rv_Disclaimer$SelectedDisclaimer$IsActive)
    
    toggleState(id        = "btn_disclaimer_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_disclaimer) > 0 &&
                            rv_Disclaimer$SelectedDisclaimer$IsActive)
  })
  
  # Disclaimer - Event Observers ------------------------------------
  
  observeEvent(input$rdo_disclaimer, 
               ..rdo_disclaimer(rv_Disclaimer, 
                                input))
  
  observeEvent(input$btn_disclaimer_add, 
               ..btn_disclaimer_add(session       = session, 
                                    rv_Disclaimer = rv_Disclaimer, 
                                    input         = input))
  
  observeEvent(input$btn_disclaimer_edit, 
               ..btn_disclaimer_edit(session       = session, 
                                     rv_Disclaimer = rv_Disclaimer, 
                                     input         = input))
  
  observeEvent(input$btn_disclaimer_addEditDisclaimer, 
               ..btn_disclaimer_addEditDisclaimer(session          = session, 
                                                  rv_Disclaimer    = rv_Disclaimer, 
                                                  input            = input, 
                                                  current_user_oid = CURRENT_USER_OID(), 
                                                  proxy            = proxy_dt_disclaimer))
  
  observeEvent(input$btn_disclaimer_activate,
               ..btn_disclaimer_activateDeactivate(activate          = TRUE, 
                                                    rv_Disclaimer    = rv_Disclaimer, 
                                                    input            = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy            = proxy_dt_disclaimer))
  
  observeEvent(input$btn_disclaimer_deactivate, 
               ..btn_disclaimer_activateDeactivate(activate          = FALSE, 
                                                    rv_Disclaimer    = rv_Disclaimer, 
                                                    input            = input, 
                                                    current_user_oid = CURRENT_USER_OID(), 
                                                    proxy            = proxy_dt_disclaimer))
  
  # Disclaimer - Output ---------------------------------------------
  
  output$dt_disclaimer <- 
    DT::renderDataTable({
      queryDisclaimer() %>% 
        radioDataTable(id_variable  = "OID", 
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
    Footer         = queryFooter(), 
    AddEdit        = "Add", 
    SelectedFooter = NULL
  )
  
  # Footer - Passive Observers --------------------------------------
  
  observe({
    toggleState(id        = "btn_footer_add", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id        = "btn_footer_edit", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_footer) > 0)
  })
  
  observe({
    req(rv_Footer$SelectedFooter)
    
    toggleState(id        = "btn_footer_activate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_footer) > 0 &&
                            !rv_Footer$SelectedFooter$IsActive)
    
    toggleState(id        = "btn_footer_deactivate", 
                condition = USER_IS_REPORT_ADMIN() &&
                            length(input$rdo_footer) > 0 &&
                            rv_Footer$SelectedFooter$IsActive)
  })
  
  # Footer - Event Observers ----------------------------------------
  
  observeEvent(input$rdo_footer, 
               ..rdo_footer(rv_Footer, 
                                 input))
  
  observeEvent(input$btn_footer_add, 
               ..btn_footer_add(session        = session, 
                                     rv_Footer = rv_Footer, 
                                     input     = input))
  
  observeEvent(input$btn_footer_edit, 
               ..btn_footer_edit(session        = session, 
                                      rv_Footer = rv_Footer, 
                                      input     = input))
  
  observeEvent(input$btn_footer_addEditFooter, 
               ..btn_footer_addEditFooter(session          = session, 
                                          rv_Footer        = rv_Footer, 
                                          input            = input, 
                                          current_user_oid = CURRENT_USER_OID(), 
                                          proxy            = proxy_dt_footer))
  
  observeEvent(input$btn_footer_activate,
               ..btn_footer_activateDeactivate(activate         = TRUE, 
                                               rv_Footer        = rv_Footer, 
                                               input            = input, 
                                               current_user_oid = CURRENT_USER_OID(), 
                                               proxy            = proxy_dt_footer))
  
  observeEvent(input$btn_footer_deactivate, 
               ..btn_footer_activateDeactivate(activate         = FALSE, 
                                               rv_Footer        = rv_Footer, 
                                               input            = input, 
                                               current_user_oid = CURRENT_USER_OID(), 
                                               proxy            = proxy_dt_footer))
  
  # Footer - Output -------------------------------------------------
  
  output$dt_footer <- 
    DT::renderDataTable({
      queryFooter() %>% 
        radioDataTable(id_variable  = "OID", 
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
    Logo         = queryLogo(), 
    AddEdit      = "Add", 
    SelectedLogo = NULL
  )
  
  # Logo - Passive Observers ----------------------------------------
  
  observe({
    toggleState(id        = "btn_logo_add", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState(id        = "btn_logo_edit", 
                condition = USER_IS_REPORT_ADMIN() && 
                            length(input$rdo_logo) > 0)
    
    hide("btn_logo_activate")
    hide("btn_logo_deactivate")
  })
  
  # Logo - Event Observers ------------------------------------------
  
  observeEvent(input$rdo_logo, 
               ..rdo_logo(rv_Logo, input))
  
  observeEvent(input$btn_logo_add, 
               ..btn_logo_add(session  = session, 
                               rv_Logo = rv_Logo))
  
  observeEvent(input$btn_logo_edit,
               ..btn_logo_edit(session  = session, 
                                rv_Logo = rv_Logo))
  
  observeEvent(input$file_logo_add,
               ..file_logo_add(session = session, 
                                input  = input))
  
  observeEvent(input$btn_logo_addEdit, 
               ..btn_logo_addEdit(session  = session, 
                                   rv_Logo = rv_Logo, 
                                   input   = input, 
                                   proxy   = proxy_dt_logo))
  
  # Logo - Output ---------------------------------------------------
  
  output$dt_logo <- 
    DT::renderDataTable({
      queryLogo() %>% 
        radioDataTable(id_variable  = "OID", 
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
          if (nrow(File)) File$SavedTo
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
      AddEdit      = "Add", 
      Roles        = queryRole(), 
      SelectedRole = NULL, 
      UserRole     = NULL
    )
  
  # Roles - Passive Observers ---------------------------------------
  
  observe({
    toggleState(id        = "btn_role_add", 
                condition = USER_IS_USER_ADMIN())
    
    toggleState(id        = "btn_role_edit", 
                condition = USER_IS_USER_ADMIN() &&
                            length(input$rdo_role) > 0 & 
                            !rv_Roles$SelectedRole$RoleName %in% c("UserAdministrator", 
                                                                   "ReportAdministrator", 
                                                                   "ReportSubmission"))
    
    toggleState(id        = "btn_role_viewEdit", 
                condition = USER_IS_USER_ADMIN() &&
                            length(input$rdo_role) > 0)
  })
  
  observe({
    req(rv_Roles$SelectedRole)
    
    toggleState(id        = "btn_role_activate", 
                condition = USER_IS_USER_ADMIN() &&
                            length(input$rdo_role) > 0 &&
                            isFALSE(rv_Roles$SelectedRole$IsActive))
    
    toggleState(id        = "btn_role_deactivate", 
                condition = USER_IS_USER_ADMIN() &&
                            length(input$rdo_role) > 0 &&
                            isTRUE(rv_Roles$SelectedRole$IsActive) & 
                            !rv_Roles$SelectedRole$RoleName %in% c("UserAdministrator", 
                                                                   "ReportAdministrator", 
                                                                   "ReportSubmission"))
  })
  
  # Roles - Event Observers -----------------------------------------
  
  observeEvent(input$rdo_role, 
               ..rdo_role(rv_Roles  = rv_Roles, 
                           input    = input))
  
  observeEvent(input$btn_role_add,
               ..btn_role_add(session   = session, 
                               rv_Roles = rv_Roles))
  
  observeEvent(input$btn_role_edit, 
               ..btn_role_edit(session   = session, 
                                rv_Roles = rv_Roles, 
                                input    = input))
  
  observeEvent(input$btn_role_addEditRole, 
               ..btn_role_addEditRole(session           = session, 
                                       rv_Roles         = rv_Roles, 
                                       input            = input, 
                                       is_edit          = rv_Roles$AddEdit == "Edit", 
                                       this_role_name   = rv_Roles$SelectedRole$RoleName, 
                                       current_user_oid = CURRENT_USER_OID(), 
                                       proxy            = proxy_dt_role))
  
  observeEvent(input$btn_role_activate, 
               ..btn_role_activateDeactivate(active            = TRUE, 
                                              rv_Roles         = rv_Roles,
                                              input            = input, 
                                              current_user_oid = CURRENT_USER_OID(), 
                                              proxy            = proxy_dt_role))
  
  observeEvent(input$btn_role_deactivate, 
               ..btn_role_activateDeactivate(active            = FALSE, 
                                              rv_Roles         = rv_Roles,
                                              input            = input, 
                                              current_user_oid = CURRENT_USER_OID(), 
                                              proxy            = proxy_dt_role))
  
  observeEvent(input$btn_role_viewEdit, 
               ..btn_role_viewEdit(rv_User   = rv_User, 
                                    rv_Roles = rv_Roles, 
                                    session  = session))
  
  observeEvent(input$multi_userRole_move_all_right, 
               updateMultiSelect(session = session, 
                                 inputId = "multi_userRole", 
                                 input   = input, 
                                 action  = "move_all_right"))
  
  observeEvent(input$multi_userRole_move_right, 
               updateMultiSelect(session = session, 
                                 inputId = "multi_userRole", 
                                 input   = input, 
                                 action  = "move_right"))
  
  observeEvent(input$multi_userRole_move_left, 
               updateMultiSelect(session = session, 
                                 inputId = "multi_userRole", 
                                 input   = input, 
                                 action  = "move_left"))
  
  observeEvent(input$multi_userRole_move_all_left, 
               updateMultiSelect(session = session, 
                                 inputId = "multi_userRole", 
                                 input   = input, 
                                 action  = "move_all_left"))
  
  observeEvent(
    input$btn_userRole_save, 
    {
      ..btn_userRole_save(input             = input, 
                           current_user_oid = CURRENT_USER_OID())
    })
  
  # Roles - Output --------------------------------------------------
  
  output$dt_role <- 
    DT::renderDataTable({
      queryRole() %>% 
        radioDataTable(id_variable  = "OID", 
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
      AddEdit      = "Add", 
      User         = queryUser(), 
      SelectedUser = NULL
    )
  
  # User - Passive Observer -----------------------------------------
  
  observe({
    toggleState(id        = "btn_user_add", 
                condition = USER_IS_USER_ADMIN())
    
    toggleState(id        = "btn_user_edit", 
                condition = USER_IS_USER_ADMIN() &&
                            length(input$rdo_user) > 0)
  })
  
  observe({
    req(rv_User$SelectedUser)

    toggleState(id        = "btn_user_activate", 
                condition = USER_IS_USER_ADMIN() &&
                            length(input$rdo_user) > 0 &&
                            isFALSE(rv_User$SelectedUser$IsActive))
    
    toggleState(id        = "btn_user_deactivate", 
                condition = USER_IS_USER_ADMIN() &&
                            length(input$rdo_user) > 0 &&
                            isTRUE(rv_User$SelectedUser$IsActive))
  })
  
  # User - Event Observer -------------------------------------------
  
  observeEvent(input$rdo_user, 
               ..rdo_user(rv_User = rv_User, 
                          input   = input))
  
  observeEvent(input$btn_user_add, 
               ..btn_user_add(session = session, 
                              rv_User = rv_User, 
                              input   = input))
  
  observeEvent(input$btn_user_edit, 
               ..btn_user_edit(session = session, 
                               rv_User = rv_User, 
                               input   = input))
  
  observeEvent(input$btn_user_addEditUser,
               ..btn_user_addEditUser(session          = session, 
                                      rv_User          = rv_User, 
                                      input            = input, 
                                      current_user_oid = CURRENT_USER_OID(), 
                                      proxy            = proxy_dt_user, 
                                      is_edit          = rv_User$AddEdit == "Edit",
                                      this_login_id    = rv_User$SelectedUser$LoginId))
  
  observeEvent(input$btn_user_activate, 
               ..btn_user_activate(active           = TRUE, 
                                   rv_User          = rv_User, 
                                   input            = input, 
                                   current_user_oid = CURRENT_USER_OID(), 
                                   proxy            = proxy_dt_user))
  
  observeEvent(input$btn_user_deactivate, 
               ..btn_user_activate(active           = FALSE, 
                                   rv_User          = rv_User, 
                                   input            = input, 
                                   current_user_oid = CURRENT_USER_OID(), 
                                   proxy            = proxy_dt_user))
  
  # User - Output ---------------------------------------------------
  
  output$dt_user <- 
    DT::renderDataTable({
      queryUser() %>% 
        radioDataTable(id_variable  = "OID", 
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
  session$onSessionEnded(function(){ 
    stopApp()
  })
})
