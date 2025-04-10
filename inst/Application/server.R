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
  
  USER_IS_REPORT_SUBMIT <- 
    reactive({
      req(CURRENT_USER_ROLE())
      SubmitRole <- CURRENT_USER_ROLE()
      SubmitRole <- SubmitRole[SubmitRole$RoleName == "ReportSubmission", ]
      isTRUE(SubmitRole$IsActiveRole)
    })
  
  # Global - Passive Observer ---------------------------------------
  # Global - Event Observer -----------------------------------------
  
  # Generate Report -------------------------------------------------
  # Generate Report - Reactive Values -------------------------------
  
  rv_GenerateReport <- reactiveValues(
    Templates = queryReportSelection(), 
    SelectedTemplate = NULL, 
    SelectedTemplateData = queryReportTemplate(oid = -1),
    
    ScheduledReportInstance = queryReportInstance(report_template_oid = -1),
    SelectedScheduledReportInstance = numeric(0), 
    
    UnscheduledReportInstance = queryReportInstance(report_template_oid = -1),
    SelectedUnscheduledReportInstance = numeric(0),
    
    SelectedInstance = queryReportInstance(report_instance_oid = -1),
    
    ReportTemplateUserPermission = queryReportTemplateUserPermission(parent_report_template = -1, 
                                                                     parent_user = -1),
    
    ReportInstanceNarrative = queryReportInstanceNarrative(report_instance_oid = -1),
    ReportInstanceNarrativeEvent = queryReportInstanceNarrativeEvent(report_instance_oid = -1),
    
    ReportInstanceSignature = queryReportInstanceSignature(report_instance_oid = -1), 
    
    Preview = NULL, 
    
    ReportInstanceSubmissionHistory = getRevisionHistory(-1),
    ReportInstanceDistribution = makeReportInstanceDistributionData(report_instance_oid = -1),
    UseSubmissionDialog = FALSE,
    
    FileArchive = queryFileArchive(parent_report_template = -1)
  )
  
  selected_instance_oid <- reactive({
    # When a new instance is selected, we want to clear the preview.
    rv_GenerateReport$Preview <- NULL
    
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
      # When a new template is selected, we want to clear the preview.
      rv_GenerateReport$Preview <- NULL
      rv_GenerateReport$FileArchive <- 
        queryFileArchive(parent_report_template = as.numeric(input$rdo_genReport_template))
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
  })

  # Generate Report - Instance - Event Observer ---------------------

  observeEvent(
    selected_instance_oid(),
    {
      req(selected_instance_oid())
      
      rv_GenerateReport$SelectedInstance <- 
        queryReportInstance(report_instance_oid = selected_instance_oid())
      # Report Instance Notes ---------------------------------------
      toggle(id        = "h3_genReport_reportInstanceNote_noInstanceSelected",
             condition = length(selected_instance_oid()) == 0)
      toggle(id        = "div_genReport_reportInstanceNote",
             condition = length(selected_instance_oid()) > 0)
      
      toggleState(id        = "txt_reportInstanceNote", 
                  condition = length(selected_instance_oid()) > 0 & 
                    rv_GenerateReport$ReportTemplateUserPermission$CanAddNote)
      toggleState(id        = "btn_addReportInstanceNote", 
                  condition = length(selected_instance_oid()) > 0 & 
                    rv_GenerateReport$ReportTemplateUserPermission$CanAddNote)

      # Report Instance Narrative -----------------------------------
      toggle(id        = "h3_genReport_reportInstanceNarrative_noInstanceSelected",
             condition = length(selected_instance_oid()) == 0)
      toggle(id        = "div_genReport_reportInstanceNarrative",
             condition = length(selected_instance_oid()) > 0)

      toggleState(id        = "btn_reportInstanceNarrativeEdit",
                  condition = length(selected_instance_oid()) > 0 &
                              rv_GenerateReport$ReportTemplateUserPermission$CanEditNarrative)
      
      rv_GenerateReport$ReportInstanceNarrative <- 
        queryReportInstanceNarrative(report_instance_oid = selected_instance_oid())
      
      rv_GenerateReport$ReportInstanceNarrativeEvent <- 
        queryReportInstanceNarrativeEvent(report_instance_oid = selected_instance_oid())
      
      updateTextInput(session = session, 
                      inputId = "txt_reportInstanceNarrative", 
                      value = rv_GenerateReport$ReportInstanceNarrative$Narrative)
      
      # Report Instance Signatures ----------------------------------
      
      toggle(id        = "h3_genReport_reportInstanceSignature_noInstanceSelected",
             condition = length(selected_instance_oid()) == 0)
      toggle(id        = "div_genReport_reportInstanceSignature",
             condition = length(selected_instance_oid()) > 0)
      
      rv_GenerateReport$ReportInstanceSignature <- 
        queryReportInstanceSignature(report_instance_oid = selected_instance_oid())
      
      # Report Instance Preview -------------------------------------
      toggle(id        = "h3_genReport_reportInstancePreview_noInstanceSelected",
             condition = length(selected_instance_oid()) == 0)
      toggle(id        = "div_genReport_reportInstancePreview",
             condition = length(selected_instance_oid()) > 0)
      
      # activate the shiny/configuration button
      template_dir <- rv_GenerateReport$SelectedTemplateData$TemplateDirectory
      template_dir <- system.file(file.path("ReportTemplate", template_dir), 
                                  package = "ReportManager")
      has_shiny <- 
        "shiny" %in% list.dirs(template_dir, 
                               full.names = FALSE)
      toggle(id = "btn_genReport_reportInstancePreview_shiny", 
             condition = has_shiny)
                 
      # Report Instance Submission ----------------------------------
      toggle(id        = "h3_genReport_reportInstanceSubmit_noInstanceSelected",
             condition = length(selected_instance_oid()) == 0)
      toggle(id        = "div_genReport_reportInstanceSubmit",
             condition = length(selected_instance_oid()) > 0)
      
      InstanceDistribution <- makeReportInstanceDistributionData(report_instance_oid = selected_instance_oid())

      rv_GenerateReport$ReportInstanceDistribution <- InstanceDistribution
        
      replaceData(proxy = proxy_dt_genReport_reportInstanceSubmit_distribution, 
                  data = radioDataTable(InstanceDistribution, 
                                        "RefId", 
                                        "rdo_genReport_reportInstanceSubmit_distribution"), 
                  resetPaging = FALSE,
                  rownames = FALSE)
      updateRadioButtons(session = session, 
                         inputId = "rdo_genReport_reportInstanceSubmit_distribution", 
                         selected = character(0))

      rv_GenerateReport$ReportInstanceSubmissionHistory <- 
        getRevisionHistory(selected_instance_oid())

    })

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
      ..out_dt_instance_unscheduled(rv_GenerateReport = rv_GenerateReport)
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
      ) %>%
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

  # Generate Report - Narrative - Passive Observer ------------------


  # Generate Report - Narrative - Event Observer --------------------
  
  observeEvent(
    input$btn_reportInstanceNarrativeEdit,
    {
      shinyjs::disable("btn_reportInstanceNarrativeEdit")
      shinyjs::enable("btn_reportInstanceNarrativeDiscard")
      shinyjs::enable("btn_reportInstanceNarrativeSave")
      shinyjs::enable("txt_reportInstanceNarrative")
    }
  )
  
  observeEvent(
    input$btn_reportInstanceNarrativeSave, 
    {
      shinyjs::enable("btn_reportInstanceNarrativeEdit")
      shinyjs::disable("btn_reportInstanceNarrativeDiscard")
      shinyjs::disable("btn_reportInstanceNarrativeSave")
      shinyjs::disable("txt_reportInstanceNarrative")
      
      addEditReportInstanceNarrative(report_instance_oid = selected_instance_oid(), 
                                     narrative = input$txt_reportInstanceNarrative, 
                                     event_user = CURRENT_USER_OID())
      
      rv_GenerateReport$ReportInstanceNarrative <- 
        queryReportInstanceNarrative(report_instance_oid = selected_instance_oid())
      
      rv_GenerateReport$ReportInstanceNarrativeEvent <- 
        queryReportInstanceNarrativeEvent(report_instance_oid = selected_instance_oid())
      
      DT::replaceData(proxy = proxy_dt_reportInstanceNarrativeHistory, 
                      data = rv_GenerateReport$ReportInstanceNarrativeEvent, 
                      resetPaging = FALSE,
                      rownames = FALSE)
    }
  )
  
  observeEvent(
    input$btn_reportInstanceNarrativeDiscard, 
    {
      shinyjs::enable("btn_reportInstanceNarrativeEdit")
      shinyjs::disable("btn_reportInstanceNarrativeDiscard")
      shinyjs::disable("btn_reportInstanceNarrativeSave")
      shinyjs::disable("txt_reportInstanceNarrative")
      
      updateTextInput(session = session, 
                      inputId = "txt_reportInstanceNarrative", 
                      value = rv_GenerateReport$ReportInstanceNarrative$Narrative)
    }
  )
  
  # Generate Report - Narrative - Output ----------------------------
  
  output$dt_reportInstanceNarrativeHistory <- 
    DT::renderDataTable({
      queryReportInstanceNarrativeEvent(selected_instance_oid()) %>% 
      DT::datatable(rownames = FALSE) %>% 
        DT::formatDate(columns = "EventDateTime", 
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
  
  proxy_dt_reportInstanceNarrativeHistory <- 
    DT::dataTableProxy("dt_reportInstanceNarrativeHistory")
  
  # Generate Report - Signatures ------------------------------------
  # Generate Report - Event Observers -------------------------------
  
  observe({
    sign_btn <- names(input)[grepl("btn_reportInstanceSignature_remove", 
                                   names(input))]
    lapply(sign_btn, 
           toggleState, 
           condition = !rv_GenerateReport$SelectedInstance$IsSubmitted)
  })
  
  observe({
    sign_btn <- names(input)[grepl("btn_reportInstanceSignature_sign", 
                                   names(input))]
    
    lapply(sign_btn, 
           function(b){
             observeEvent(
               input[[b]],
               {
                 role_oid <- as.numeric(sub(".+_sign_", "", b))

                 RIS <- rv_GenerateReport$ReportInstanceSignature
                 RIS <- RIS[RIS$ParentRole == role_oid & 
                              RIS$MostRecent == 1, ]
                 addReportInstanceSignature(
                   report_instance_oid = selected_instance_oid(),
                   report_template_signature = RIS$ReportTemplateSignatureOID,
                   signed = TRUE,
                   event_user = CURRENT_USER_OID())
                 
                 rv_GenerateReport$ReportInstanceSignature <- 
                   queryReportInstanceSignature(report_instance_oid = selected_instance_oid())
                 
                 hide(sprintf("btn_reportInstanceSignature_sign_%s", 
                              role_oid))
                 show(sprintf("btn_reportInstanceSignature_remove_%s", 
                              role_oid))
               }
             )
           })
  })
  
  observe({
    sign_btn <- names(input)[grepl("btn_reportInstanceSignature_remove", 
                                   names(input))]
    
    lapply(sign_btn, 
           function(b){
             observeEvent(
               input[[b]],
               {
                 role_oid <- as.numeric(sub(".+_remove_", "", b))

                 RIS <- rv_GenerateReport$ReportInstanceSignature
                 RIS <- RIS[RIS$ParentRole == role_oid & 
                              RIS$MostRecent == 1, ]
                 
                 addReportInstanceSignature(
                   report_instance_oid = selected_instance_oid(),
                   report_template_signature = RIS$ReportTemplateSignatureOID,
                   signed = FALSE,
                   event_user = CURRENT_USER_OID())
                 
                 rv_GenerateReport$ReportInstanceSignature <- 
                   queryReportInstanceSignature(report_instance_oid = selected_instance_oid())
                 
                 show(sprintf("btn_reportInstanceSignature_sign_%s", 
                              role_oid))
                 hide(sprintf("btn_reportInstanceSignature_remove_%s", 
                              role_oid))
               }
             )
           })
  })

  # Generate Report - Output ----------------------------------------
  
  output$tbl_genReport_reportInstanceSignature <- 
    renderUI({
      Signature <- rv_GenerateReport$ReportInstanceSignature
      HTML(makeSignatureTable(Signature))
    })
  
  output$dt_reportInstanceSignatureHistory <- 
    DT::renderDataTable({
      Signature <- rv_GenerateReport$ReportInstanceSignature
      Signature <- Signature[order(Signature$SignatureDateTime, 
                                   decreasing = TRUE), ]
      Signature <- Signature[c("RoleName", "SignatureName", 
                               "SignatureDateTime", "IsSigned")]
      Signature <- Signature[!is.na(Signature$SignatureDateTime), ]
      RM_datatable(data = Signature) %>% 
        DT::formatDate(columns = c("SignatureDateTime"),
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
  
  # Generate Report - Preview ---------------------------------------
  # Generate Report - Preview - Event Observer ----------------------
  
  observeEvent(
    input$btn_genReport_reportInstancePreview_preview, 
    {
      filename <- 
        ReportManager:::generateReportFile(
          report_instance_oid = selected_instance_oid(), 
          is_preview = TRUE,
          is_submission = FALSE,
          params = list(), 
          build_dir = tempdir(), 
          report_format = "html")
      
      report_code <- readLines(filename,
                               encoding = "UTF-8")
      report_code <- paste0(report_code, collapse = "\n")
      report_code <- sub("^.+[<]body[>]", "<div>", report_code)
      report_code <- sub("[<]/body[>].+", "</div>", report_code)
      
      rv_GenerateReport$Preview <- report_code
      
      addReportInstanceGeneration(
        report_instance_oid = selected_instance_oid(), 
        report_template_oid = rv_GenerateReport$SelectedTemplate,
        start_date_time = rv_GenerateReport$SelectedInstance$StartDateTime, 
        end_date_time = rv_GenerateReport$SelectedInstance$EndDateTime, 
        report_format = "preview", 
        include_data = FALSE, 
        is_preview = TRUE, 
        is_distributed = FALSE, 
        is_archived = FALSE, 
        is_submission = FALSE, 
        user_oid = CURRENT_USER_OID()
      )
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstancePreview_shiny, 
    {
      btn_reportInstancePreview_shiny(report_instance_oid = selected_instance_oid(), 
                                      report_template_oid = rv_GenerateReport$SelectedTemplate,
                                      output_dir = tempdir(), 
                                      rm_flavor = getOption("RM_sql_flavor"), 
                                      rm_database_file = getOption("RM_sqlite_flavor"), 
                                      rm_driver = getOption("RM_SqlServer_driver"), 
                                      rm_server = getOption("RM_sqlServer_server"), 
                                      rm_database = getOption("RM_sqlServer_database"))
     
      addReportInstanceGeneration(
        report_instance_oid = selected_instance_oid(), 
        report_template_oid = rv_GenerateReport$SelectedTemplate,
        start_date_time = rv_GenerateReport$SelectedInstance$StartDateTime, 
        end_date_time = rv_GenerateReport$SelectedInstance$EndDateTime, 
        report_format = "shiny", 
        include_data = FALSE, 
        is_preview = TRUE, 
        is_distributed = FALSE, 
        is_archived = FALSE, 
        is_submission = FALSE, 
        user_oid = CURRENT_USER_OID()
      )
    }
  )
  
  # Generate Report - Preview - Download Handler --------------------
  
  output$btn_genReport_reportInstancePreview_html <- 
    downloadHandler(
      filename = 
        makeReportFileName(report_instance_oid = selected_instance_oid(),
                           is_preview = TRUE),
      content = function(file){
        makeReportPreview(report_instance_oid = selected_instance_oid(), 
                          zipfile = file, 
                          include_image = "Images" %in% input$chkgrp_genReport_reportInstancePreview_supplementalFile, 
                          include_data = "Data" %in% input$chkgrp_genReport_reportInstancePreview_supplementalFile,
                          build_dir = tempdir(), 
                          params = list(), 
                          report_format = "html")
        
        addReportInstanceGeneration(
          report_instance_oid = selected_instance_oid(), 
          report_template_oid = rv_GenerateReport$SelectedTemplate,
          start_date_time = rv_GenerateReport$SelectedInstance$StartDateTime, 
          end_date_time = rv_GenerateReport$SelectedInstance$EndDateTime, 
          report_format = "html", 
          include_data = FALSE, 
          is_preview = TRUE, 
          is_distributed = FALSE, 
          is_archived = FALSE, 
          is_submission = FALSE, 
          user_oid = CURRENT_USER_OID()
        )
      }
    )
  
  output$btn_genReport_reportInstancePreview_pdf <- 
    downloadHandler(
      filename = 
        makeReportFileName(report_instance_oid = selected_instance_oid(),
                           is_preview = TRUE),
      content = function(file){
        makeReportPreview(report_instance_oid = selected_instance_oid(), 
                          zipfile = file, 
                          include_image = "Images" %in% input$chkgrp_genReport_reportInstancePreview_supplementalFile, 
                          include_data = "Data" %in% input$chkgrp_genReport_reportInstancePreview_supplementalFile,
                          build_dir = tempdir(), 
                          params = list(), 
                          report_format = "pdf")
        
        addReportInstanceGeneration(
          report_instance_oid = selected_instance_oid(), 
          report_template_oid = rv_GenerateReport$SelectedTemplate,
          start_date_time = rv_GenerateReport$SelectedInstance$StartDateTime, 
          end_date_time = rv_GenerateReport$SelectedInstance$EndDateTime, 
          report_format = "pdf", 
          include_data = FALSE, 
          is_preview = FALSE, 
          is_distributed = FALSE, 
          is_archived = FALSE, 
          is_submission = FALSE, 
          user_oid = CURRENT_USER_OID()
        )
      }
    )
  
  # Generate Report - Preview - Output Options ----------------------
  
  output$html_genReport_reportInstancePreview <- 
    renderUI({
      req(rv_GenerateReport$Preview)
      
      html <- HTML(rv_GenerateReport$Preview)
      withMathJax(html)
    })
  
  # Generate Report - Archival and Submission -----------------------
  # Generate Report - Archival and Submission - Observer ------------
  
  observe({
    toggleState("btn_genReport_reportInstanceSubmit_archiveDistribute", 
                condition = length(input$chk_genReport_reportInstanceSubmit_archiveDistribute) > 0)

    toggleState("btn_genReport_reportInstanceSubmission_changeTestEmailStatus", 
                condition = length(input$rdo_genReport_reportInstanceSubmit_distribution) > 0)
    
    toggleState("btn_genReport_reportInstanceSubmission_changeActiveStatus", 
                condition = length(input$rdo_genReport_reportInstanceSubmit_distribution) > 0)
  })
  
  observe({
    req(rv_GenerateReport$ReportInstanceSignature)
    Signature <- rv_GenerateReport$ReportInstanceSignature
    Signature <- split(Signature, 
                       Signature$ParentRole)
    Signature <- lapply(Signature, 
                        function(S){
                          S <- S[order(S$SignatureDateTime, 
                                       decreasing = TRUE), ]
                          head(S, 1)
                        })
    Signature <- do.call("rbind", Signature)

    all_signed <- isTRUE(all(Signature$IsSigned))
    
    Permission <- rv_GenerateReport$ReportTemplatePermission
    Permission <- Permission[Permission$ParentUser == CURRENT_USER_OID(), ]
    
    user_template_submit <- isTRUE(Permission$CanSubmit)
    
    is_submitted <- isTRUE(rv_GenerateReport$SelectedInstance$IsSubmitted)

    toggleState("btn_genReport_reportInstanceSubmit_submitToClient", 
                condition = all_signed & 
                  (USER_IS_REPORT_SUBMIT() | user_template_submit) & 
                  !is_submitted)
  })
  
  observe({

    Permission <- rv_GenerateReport$ReportTemplatePermission
    Permission <- Permission[Permission$ParentUser == CURRENT_USER_OID(), ]
    
    user_template_revise <- isTRUE(Permission$CanStartRevision)
    
    is_submitted <- isTRUE(rv_GenerateReport$SelectedInstance$IsSubmitted)
    
    toggleState("btn_genReport_reportInstanceSubmit_startRevision", 
                condition = (USER_IS_REPORT_SUBMIT() | user_template_revise) & 
                  is_submitted)
  })
  
  observe({
    toggleState("btn_genReport_instanceSubmit_confirmRevision", 
                condition = trimws(input$txt_genReport_instanceSubmit_reasonRevise) != "")
  })
  
  # Generate Report - Archival and Submission - Event Observer ------
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmit_archiveDistribute, 
    {
      disable("btn_genReport_reportInstanceSubmit_archiveDistribute")
      
      rv_GenerateReport$UseSubmissionDialog <- FALSE
      
      dist_opt <- tolower(input$chk_genReport_reportInstanceSubmit_archiveDistribute)
      if (length(dist_opt) == 0){
        alert("Neither 'Add to Archive' nor 'Distribute Internally' was selected. No action performed.")
        return(NULL)
      }
      
      is_add_to_archive <- "add to archive" %in% dist_opt
      is_distribute <- "distribute internally" %in% dist_opt
      
      if (!is_distribute){
        hide("rdo_genReport_reportInstance_embedHtml")
        hide("txt_genReport_reportInstance_emailMessage")
      } else {
        show("rdo_genReport_reportInstance_embedHtml")
        show("txt_genReport_reportInstance_emailMessage")
        
        message <- ReportManager:::.sendEmail_makeMessage(
          report_template = rv_GenerateReport$SelectedTemplateData, 
          report_instance = rv_GenerateReport$SelectedInstance)
        
        updateTextAreaInput(session = session, 
                            inputId = "txt_genReport_reportInstance_emailMessage", 
                            value = message)
      }
      
      toggleModal(session = session, 
                  modalId = "modal_genReport_reportInstance_distribute",
                  toggle = "open")
      
      
      
      hide("p_genReport_reportInstanceSubmit_makingReport", 
           anim = TRUE, 
           animType = "fade", 
           time = 2)
      enable("btn_genReport_reportInstanceSubmit_archiveDistribute")
    }
  )
  
  observeEvent(
    input$rdo_genReport_reportInstance_format, 
    {
      toggleState("rdo_genReport_reportInstance_embedHtml", 
                  condition = input$rdo_genReport_reportInstance_format == "HTML")
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmit_submitToClient, 
    {
      hide("rdo_genReport_reportInstance_format")
      hide("rdo_genReport_reportInstance_embedHtml")
      show("txt_genReport_reportInstance_emailMessage")
      
      message <- ReportManager:::.sendEmail_makeMessage(
        report_template = rv_GenerateReport$SelectedTemplateData, 
        report_instance = rv_GenerateReport$SelectedInstance)
      
      updateRadioButtons(session = session, 
                         inputId = "rdo_genReport_reportInstance_format", 
                         selected = "PDF")
      
      updateRadioButtons(session = session, 
                         inputId = "rdo_genReport_reportInstance_embedHTML", 
                         selected = "Attached")
      
      updateTextAreaInput(session = session, 
                          inputId = "txt_genReport_reportInstance_emailMessage", 
                          value = message)
      
      rv_GenerateReport$UseSubmissionDialog <- TRUE
      
      toggleModal(session = session, 
                  modalId = "modal_genReport_reportInstance_distribute",
                  toggle = "open")
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstance_sendReport,
    {
      show("p_genReport_reportInstanceSubmit_makingReport")
      
      disable("rdo_genReport_reportInstance_format")
      disable("rdo_genReport_reportInstance_embedHtml")
      disable("txt_genReport_reportInstance_emailMessage")
      disable("btn_genReport_reportInstance_sendReport")
      dist_opt <- tolower(input$chk_genReport_reportInstanceSubmit_archiveDistribute)
      
      is_submission <- rv_GenerateReport$UseSubmissionDialog
      
      is_add_to_archive <- if (is_submission) TRUE else "add to archive" %in% dist_opt
      is_distribute <- if (is_submission) TRUE else "distribute internally" %in% dist_opt
      is_embed_html <- if (is_submission) FALSE else "embed" %in% tolower(SETTINGS$SettingValue[SETTINGS$SettingKey == "htmlEmbed"])
      
      report_format <- if (is_submission) "pdf" else tolower(SETTINGS$SettingValue[SETTINGS$SettingKey == "defaultReportFormat"])
      
      submitReport(report_instance_oid = selected_instance_oid(), 
                   is_submission = is_submission, 
                   is_distribute = is_distribute, 
                   is_distribute_internal_only = "distribute internally" %in% dist_opt, 
                   is_add_to_archive = is_add_to_archive, 
                   is_embed_html = is_embed_html, 
                   params = list(), 
                   report_format = report_format, 
                   current_user_oid = CURRENT_USER_OID())
      
      if (is_submission){
        ReportInstance <- queryReportInstance(report_template_oid = rv_GenerateReport$SelectedTemplate)
        
        rv_GenerateReport$ScheduledReportInstance <-
          ReportInstance[ReportInstance$IsScheduled, ]
        
        rv_GenerateReport$UnscheduledReportInstance <-
          ReportInstance[!ReportInstance$IsScheduled, ]
        
        rv_GenerateReport$SelectedInstance <-
          queryReportInstance(report_instance_oid = selected_instance_oid())
        
        rv_GenerateReport$ReportInstanceSubmissionHistory <-
          getRevisionHistory(selected_instance_oid())
      }
      
      enable("rdo_genReport_reportInstance_format")
      toggleState("rdo_genReport_reportInstance_embedHtml", 
                  condition = input$rdo_genReport_reportInstance_format == "HTML")
      enable("btn_genReport_reportInstance_sendReport")
      enable("txt_genReport_reportInstance_emailMessage")
      
      hide("p_genReport_reportInstanceSubmit_makingReport")
      toggleModal(session = session, 
                  modalId = "modal_genReport_reportInstance_distribute",
                  toggle = "close")
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmit_startRevision, 
    {
      updateTextAreaInput(session = session, 
                          inputId = "txt_genReport_instanceSubmit_reasonRevise", 
                          value = "")
      toggleModal(session = session, 
                  modalId = "modal_genReport_instanceSubmit_startRevision", 
                  toggle = "open")
    }
  )
  
  observeEvent(
    input$btn_genReport_instanceSubmit_confirmRevision, 
    {
      disable("txt_genReport_instanceSubmit_reasonRevise")
      disable("btn_genReport_instanceSubmit_confirmRevision")
      revision_time <- Sys.time()
      
      # Update the revision generation and isSubmitted flag
      addReportInstanceRevision(report_instance_oid = selected_instance_oid(),
                                parent_user = CURRENT_USER_OID(),
                                revision_date_time = revision_time,
                                reason = input$txt_genReport_instanceSubmit_reasonRevise)

      updateInstanceIsSubmitted(report_instance_oid = selected_instance_oid(),
                                is_submitted = FALSE,
                                current_user_oid = CURRENT_USER_OID(),
                                event_date_time = revision_time)
    
      # Remove all signatures from the report  
      sig_remove <- 
        unique(rv_GenerateReport$ReportInstanceSignature$ParentReportTemplateSignature)
      
      lapply(sig_remove,
             function(rts){
               addReportInstanceSignature(report_instance_oid = selected_instance_oid(),
                                          report_template_signature = rts,
                                          signed = FALSE,
                                          event_user = CURRENT_USER_OID())
             })
      
      # Email users with the signatory roles
      
      SignUserRole <- lapply(unique(rv_GenerateReport$ReportInstanceSignature$ParentRole), 
                             function(sr) queryUserRole(role_oid = sr))
      SignUserRole <- do.call("rbind", SignUserRole)
      SignUserRole <- SignUserRole[SignUserRole$IsActive, ]
      
      SignUser <- lapply(unique(SignUserRole$ParentUser), 
                         function(u){queryUser(oid = u)})
      SignUser <- do.call("rbind", SignUser)
      SignUser <- SignUser[SignUser$IsActive, ]
      
      msg <- 
        ReportManager:::.sendEmail_makeRevisionMessage(
          report_template = rv_GenerateReport$SelectedTemplateData, 
          report_instance = rv_GenerateReport$SelectedInstance, 
          user_oid = CURRENT_USER_OID()
        )
      
      msg <- paste(trimws(msg), 
                   input$txt_genReport_instanceSubmit_reasonRevise,
                   sep = "\n")
      
      sendEmail(from_user_oid = CURRENT_USER_OID(), 
                to_address = unique(SignUser$EmailAddress), 
                message = msg, 
                report_template = rv_GenerateReport$SelectedTemplateData,
                report_instance_oid = selected_instance_oid(), 
                is_revision = TRUE)
      
      # Update ReportInstance reactives
      ReportInstance <- queryReportInstance(report_template_oid = rv_GenerateReport$SelectedTemplate)
      
      rv_GenerateReport$ScheduledReportInstance <-
        ReportInstance[ReportInstance$IsScheduled, ]
      
      rv_GenerateReport$UnscheduledReportInstance <-
        ReportInstance[!ReportInstance$IsScheduled, ]

      rv_GenerateReport$SelectedInstance <-
        queryReportInstance(report_instance_oid = selected_instance_oid())

      rv_GenerateReport$ReportInstanceSignature <-
        queryReportInstanceSignature(report_instance_oid = selected_instance_oid())
      
      rv_GenerateReport$ReportInstanceSubmissionHistory <- 
        getRevisionHistory(selected_instance_oid())

      toggleModal(session = session,
                  modalId = "modal_genReport_instanceSubmit_startRevision",
                  toggle = "close")
      enable("txt_genReport_instanceSubmit_reasonRevise")
      enable("btn_genReport_instanceSubmit_confirmRevision")
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmission_editDistributionList,
    {
      shinyjs::hide(id = "btn_templateDistribution_addEdit")
      shinyjs::show(id = "btn_instanceDistribution_addEdit")
      
      Selected <- 
        queryInstanceDistributionSelection(report_instance_oid = selected_instance_oid())

      SelectedUser <- Selected[Selected$DistributeBy == "Indiv.", ]
      SelectedUser <- SelectedUser[SelectedUser$IsActive, ]
      
      SelectedRole <- Selected[Selected$DistributeBy == "Role", ]
      SelectedRole <- SelectedRole[SelectedRole$IsActive, ]

      replaceMultiSelect(session = session,
                         inputId = "templateDistributionUser",
                         choices = as.character(rv_User$User$OID),
                         selected = as.character(SelectedUser$ParentUser),
                         names = sprintf("%s, %s (%s)", 
                                         rv_User$User$LastName, 
                                         rv_User$User$FirstName, 
                                         rv_User$User$LoginId))

      replaceMultiSelect(session = session,
                         inputId = "templateDistributionRole",
                         choices = as.character(rv_Roles$Roles$OID),
                         selected = as.character(SelectedRole$ParentRole),
                         names = rv_Roles$Roles$RoleName)
      
      toggleModal(session = session, 
                  modalId = "modal_templateDistribution_edit", 
                  toggle = "open")
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmission_addAllIncludeInTest, 
    {
      sel <- input$rdo_genReport_reportInstanceSubmit_distribution
      if (is.null(sel)) sel <- character(0)
      
      InstanceDistribution <- rv_GenerateReport$ReportInstanceDistribution
      InstanceDistribution$IncludeInTest <- rep(TRUE, 
                                                nrow(InstanceDistribution))
      rv_GenerateReport$ReportInstanceDistribution <- InstanceDistribution
      replaceData(proxy = proxy_dt_genReport_reportInstanceSubmit_distribution, 
                  data = radioDataTable(InstanceDistribution, 
                                        id_variable = "RefId", 
                                        element_name = "rdo_genReport_reportInstanceSubmit_distribution", 
                                        checked = sel),
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmission_removeAllIncludeInTest, 
    {
      sel <- input$rdo_genReport_reportInstanceSubmit_distribution
      if (is.null(sel)) sel <- character(0)
      
      InstanceDistribution <- rv_GenerateReport$ReportInstanceDistribution
      InstanceDistribution$IncludeInTest <- rep(FALSE, 
                                                nrow(InstanceDistribution))
      rv_GenerateReport$ReportInstanceDistribution <- InstanceDistribution
      replaceData(proxy = proxy_dt_genReport_reportInstanceSubmit_distribution, 
                  data = radioDataTable(InstanceDistribution, 
                                        id_variable = "RefId", 
                                        element_name = "rdo_genReport_reportInstanceSubmit_distribution",
                                        checked = sel),
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmission_changeTestEmailStatus, 
    {
      sel <- input$rdo_genReport_reportInstanceSubmit_distribution
      if (is.null(sel)) sel <- character(0)
      
      row <- which(InstanceDistribution$RefId == as.numeric(sel))
      
      InstanceDistribution <- rv_GenerateReport$ReportInstanceDistribution
      
      InstanceDistribution$IncludeInTest[row] <- 
        !InstanceDistribution$IncludeInTest[row]
      
      rv_GenerateReport$ReportInstanceDistribution <- InstanceDistribution
      replaceData(proxy = proxy_dt_genReport_reportInstanceSubmit_distribution, 
                  data = radioDataTable(InstanceDistribution, 
                                        id_variable = "RefId", 
                                        element_name = "rdo_genReport_reportInstanceSubmit_distribution", 
                                        checked = sel),
                  resetPaging = FALSE,
                  rownames = FALSE)
      
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmission_activateAll, 
    {
      sel <- input$rdo_genReport_reportInstanceSubmit_distribution
      if (is.null(sel)) sel <- character(0)
      
      InstanceDistribution <- rv_GenerateReport$ReportInstanceDistribution
      InstanceDistribution$IsActive <- rep(TRUE, 
                                           nrow(InstanceDistribution))
      rv_GenerateReport$ReportInstanceDistribution <- InstanceDistribution
      replaceData(proxy = proxy_dt_genReport_reportInstanceSubmit_distribution, 
                  data = radioDataTable(InstanceDistribution, 
                                        id_variable = "RefId", 
                                        element_name = "rdo_genReport_reportInstanceSubmit_distribution", 
                                        checked = sel),
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmission_deactivateAll, 
    {
      sel <- input$rdo_genReport_reportInstanceSubmit_distribution
      if (is.null(sel)) sel <- character(0)
      
      InstanceDistribution <- rv_GenerateReport$ReportInstanceDistribution
      InstanceDistribution$IsActive <- rep(FALSE, 
                                           nrow(InstanceDistribution))
      rv_GenerateReport$ReportInstanceDistribution <- InstanceDistribution
      replaceData(proxy = proxy_dt_genReport_reportInstanceSubmit_distribution, 
                  data = radioDataTable(InstanceDistribution, 
                                        id_variable = "RefId", 
                                        element_name = "rdo_genReport_reportInstanceSubmit_distribution",
                                        checked = sel),
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
  )
  
  observeEvent(
    input$btn_genReport_reportInstanceSubmission_changeActiveStatus, 
    {
      sel <- input$rdo_genReport_reportInstanceSubmit_distribution
      if (is.null(sel)) sel <- character(0)
      
      row <- which(InstanceDistribution$RefId == as.numeric(sel))
      
      InstanceDistribution <- rv_GenerateReport$ReportInstanceDistribution
      
      InstanceDistribution$IsActive[row] <- 
        !InstanceDistribution$IsActive[row]
      
      rv_GenerateReport$ReportInstanceDistribution <- InstanceDistribution
      replaceData(proxy = proxy_dt_genReport_reportInstanceSubmit_distribution, 
                  data = radioDataTable(InstanceDistribution, 
                                        id_variable = "RefId", 
                                        element_name = "rdo_genReport_reportInstanceSubmit_distribution", 
                                        checked = sel),
                  resetPaging = FALSE,
                  rownames = FALSE)
      
    }
  )
  
  observeEvent(
    input$btn_instanceDistribution_addEdit,
    {
      # ..btn_instanceDistribution_addEdit(
      #   templateDistributionUser = input$templateDistributionUser,
      #   templateDistributionRole = input$templateDistributionRole,
      #   rdo_template             = input$rdo_template,
      #   rv_Template              = rv_Template,
      #   current_user_oid         = CURRENT_USER_OID(),
      #   session                  = session
      # )
      instanceDistributionUser <- input$templateDistributionUser
      instanceDistributionRole <- input$templateDistributionRole
      
      InstanceDist <- queryInstanceDistributionSelection(report_instance_oid = selected_instance_oid())
    
      DistributionUser <- jsonlite::fromJSON(instanceDistributionUser)
      
      InputUser <- DistributionUser[c("choices", "selected")]
      names(InputUser) <- c("ParentUser", "IsActive")
      InputUser$ParentRole <- rep(NA_real_, nrow(InputUser))
      InputUser <- merge(InputUser,
                         InstanceDist[InstanceDist$DistributeBy == "Indiv.",
                                      c("ReportInstanceDistributionOID",
                                        "ParentUser", "ParentReportTemplate",
                                        "IsActiveInstance", "IsActiveTemplate")],
                         by = c("ParentUser"),
                         all.x = TRUE,
                         all.y = TRUE)
      InputUser <- InputUser[!is.na(InputUser$IsActiveInstance) | 
                               compareInstanceTemplateDistribution(InputUser$IsActive, 
                                                                   InputUser$IsActiveTemplate), ]

      DistributionRole <- jsonlite::fromJSON(instanceDistributionRole)
      InputRole <- DistributionRole[c("choices", "selected")]
      names(InputRole) <- c("ParentRole", "IsActive")
      InputRole$ParentUser <- rep(NA_real_, nrow(InputRole))
      InputRole <- merge(InputRole,
                         InstanceDist[InstanceDist$DistributeBy == "Role" ,
                                      c("ReportInstanceDistributionOID", 
                                        "ParentRole", "ParentReportTemplate", 
                                        "IsActiveInstance", "IsActiveTemplate")],
                         by = c("ParentRole"),
                         all.x = TRUE,
                         all.y = TRUE)
      InputRole <- InputRole[!is.na(InputRole$IsActiveInstance) | 
                               compareInstanceTemplateDistribution(InputRole$IsActive, 
                                                                   InputRole$IsActiveTemplate), ]                    # Existing records

      Input <- rbind(InputUser[c("ReportInstanceDistributionOID", "ParentUser", "ParentRole", "IsActive")],
                     InputRole[c("ReportInstanceDistributionOID", "ParentUser", "ParentRole", "IsActive")])

      for(i in seq_len(nrow(Input))){
        addEditReportInstanceDistribution(
          oid = if (is.na(Input$ReportInstanceDistributionOID[i])) numeric(0) else Input$ReportInstanceDistributionOID[i],
          parent_report_instance = selected_instance_oid(),
          parent_user = if (is.na(Input$ParentUser[i])) numeric(0) else as.numeric(Input$ParentUser[i]),
          parent_role = if (is.na(Input$ParentRole[i])) numeric(0) else as.numeric(Input$ParentRole[i]),
          is_active = isTRUE(Input$IsActive[i]),
          event_user = CURRENT_USER_OID()
        )
      }
      InstanceDistribution <- makeReportInstanceDistributionData(report_instance_oid = selected_instance_oid())
      
      rv_GenerateReport$ReportInstanceDistribution <- InstanceDistribution
      
      replaceData(proxy = proxy_dt_genReport_reportInstanceSubmit_distribution, 
                  data = radioDataTable(InstanceDistribution, 
                                        "RefId", 
                                        "rdo_genReport_reportInstanceSubmit_distribution"), 
                  resetPaging = FALSE,
                  rownames = FALSE)

      toggleModal(session = session,
                  modalId = "modal_templateDistribution_edit",
                  toggle = "close")
      
    })
  
  # Generate Report - Archival and Submission - Output --------------
  
  output$dt_genReport_reportInstanceSubmit_revisionHistory <- 
    DT::renderDataTable({
      RM_datatable(rv_GenerateReport$ReportInstanceSubmissionHistory) %>% 
        DT::formatDate(c("EventDateTime"),
                       method = 'toLocaleTimeString',
                       params = list('en-gb',
                                     list(year = 'numeric',
                                          month = 'short',
                                          day = 'numeric',
                                          hour = 'numeric',
                                          minute = 'numeric',
                                          second = 'numeric',
                                          timeZone = 'UTC')))
    })
  
  proxy_dt_genReport_reportInstanceSubmit_revisionHistory <- 
    DT::dataTableProxy("dt_genReport_reportInstanceSubmit_revisionHistory")
  
  output$txt_genReport_reportInstance_distributeTitle <- 
    renderText({
      dist_opt <- tolower(input$chk_genReport_reportInstanceSubmit_archiveDistribute)
      
      is_submission <- isTRUE(rv_GenerateReport$UseSubmissionDialog)
      is_add_to_archive <- "add to archive" %in% dist_opt
      is_distribute <- "distribute internally" %in% dist_opt
      
      if (is_submission){
        "Submit Completed Report"
      } else if (is_add_to_archive & !is_distribute){
        "Archive Report"
      } else if (!is_add_to_archive & is_distribute){
        "Distribute Report"
      } else {
        "Archive and Distribute Report"
      }
    })
  
  output$txt_genReport_reportInstance_sendReportButtonLabel <- 
    renderText({
      dist_opt <- tolower(input$chk_genReport_reportInstanceSubmit_archiveDistribute)
      
      is_submission <- isTRUE(rv_GenerateReport$UseSubmissionDialog)
      is_add_to_archive <- "add to archive" %in% dist_opt
      is_distribute <- "distribute internally" %in% dist_opt
      
      if (is_submission){
        "Submit"
      } else if (is_add_to_archive & !is_distribute){
        "Archive"
      } else if (!is_add_to_archive & is_distribute){
        "Distribute"
      } else {
        "Archive / Distribute"
      }
    })
  
  output$dt_genReport_reportInstanceSubmit_distribution <- 
    DT::renderDataTable({
      Distribution <- makeReportInstanceDistributionData(report_instance_oid = -1)
      Distribution <- radioDataTable(Distribution, 
                                     id_variable = "RefId", 
                                     element_name = "rdo_genReport_reportInstanceSubmit_distribution")
     RM_datatable(Distribution, 
                  escape = -1)
    })
  
  proxy_dt_genReport_reportInstanceSubmit_distribution <- 
    DT::dataTableProxy("dt_genReport_reportInstanceSubmit_distribution")
  
  # Generate Report - Archived Reports ------------------------------

  # Generate Report - Archived Reports - Observer -------------------
  
  observe({
    toggleState("dwn_genReport_archivedReport", 
                condition = length(input$rdo_genReport_archivedReport))
  })
  
  # Generate Report - Archived Reports - Download Handler -----------
  
  output$dwn_genReport_archivedReport <- 
    downloadHandler(
      filename = function(){
        ThisFile <- rv_GenerateReport$FileArchive
        ThisFile <- ThisFile[ThisFile$OID == as.numeric(input$rdo_genReport_archivedReport), ]
        sprintf("%s.%s", ThisFile$FileName, ThisFile$FileExtension)
      }, 
      content = function(file){
        File <- queryFromFileArchive(oid = as.numeric(input$rdo_genReport_archivedReport)) 
        writeBin(as.raw(File$FileContent[[1]]), 
                 con = file)
      }
    )
  
  # Generate Report - Archived Reports - Output ---------------------
  
  output$dt_genReport_archivedReport <- 
    DT::renderDataTable({
      DisplayTable <- rv_GenerateReport$FileArchive
      DisplayTable <- DisplayTable[c("OID", "FileName", "FileExtension", 
                                     "Description", "CreatedDateTime")]
      DisplayTable <- DisplayTable[order(DisplayTable$CreatedDateTime, 
                                         decreasing = TRUE), ]
      radioDataTable(DisplayTable, 
                     id_variable = "OID", 
                     element_name = "rdo_genReport_archivedReport") %>% 
        RM_datatable(escape = -1) %>% 
        DT::formatDate(c("CreatedDateTime"),
                       method = 'toLocaleTimeString',
                       params = list('en-gb',
                                     list(year = 'numeric',
                                          month = 'short',
                                          day = 'numeric',
                                          hour = 'numeric',
                                          minute = 'numeric',
                                          second = 'numeric',
                                          timeZone = 'UTC')))
    })
  
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
                     rv_Schedule  = rv_Schedule, 
                     session      = session)

      DForm <- rv_DateFormat$DateFormat
      sel <- DForm$OID
      names(sel) <- sprintf("%s (%s)", 
                            DForm$FormatName, 
                            DForm$Description)

      updateSelectInput(session = session, 
                        inputId = "sel_template_dateReportingFormat", 
                        choices = sel, 
                        selected = as.character(rv_Template$SelectedTemplate$DateReportingFormat))
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
                                      rv_Roles    = rv_Roles,
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
                   rv_Template              = rv_Template,
                   current_user_oid         = CURRENT_USER_OID(),
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
      AddEdit              = "Add",
      User                 = queryUser(),
      SelectedUser         = NULL, 
      SignatureFileInput   = data.frame(), 
      CurrentUserSignature = NULL
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

  observeEvent(input$file_user_signature, 
               rv_User$SignatureFileInput <- input$file_user_signature)
  
  observeEvent(input$btn_user_addEditUser,
               ..btn_user_addEditUser(session          = session,
                                      rv_User          = rv_User,
                                      input            = input,
                                      current_user_oid = CURRENT_USER_OID(),
                                      proxy            = proxy_dt_user,
                                      is_edit          = rv_User$AddEdit == "Edit",
                                      this_login_id    = rv_User$SelectedUser$LoginId, 
                                      signature_file   = rv_User$SignatureFileInput))

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
  
  output$img_current_signature <- 
    renderImage({
      Signature <- rv_User$CurrentUserSignature
      
      write_to_file <- tempfile(fileext = Signature$FileExtension)
      writeBin(as.raw(Signature$FileContent[[1]]), 
               con = write_to_file)
      list(src = write_to_file, 
           width = "300px", 
           height = "75px")
    }, 
    deleteFile = TRUE)

  # AutoDistribute --------------------------------------------------
  # AutoDistribute - Reactive Values --------------------------------
  
  rv_AutoDist <- reactiveValues(
    Config = queryAutoDistribution(), 
    SelectedConfig = data.frame(), 
    ConfigAddEdit = "Add"
  )
  
  # AutoDistribute - Passive Observers ------------------------------
  
  observe({
    toggleState("btn_autodistribution_add", 
                condition = USER_IS_REPORT_ADMIN())
    
    toggleState("btn_autodistribution_edit", 
                condition = USER_IS_REPORT_ADMIN() & 
                  length(input$rdo_autodistribution) > 0)
  })
  
  observe({
    req(nrow(rv_AutoDist$SelectedConfig) > 0)
    toggleState("btn_autodistribution_deactivate", 
                condition = USER_IS_REPORT_ADMIN() & 
                  length(input$rdo_autodistribution) > 0 & 
                  rv_AutoDist$SelectedConfig$IsActive)
    
    toggleState("btn_autodistribution_activate", 
                condition = USER_IS_REPORT_ADMIN() & 
                  length(input$rdo_autodistribution) > 0 & 
                  !rv_AutoDist$SelectedConfig$IsActive)
  })
  
  observe({
    toggle("chk_autodistribution_isEmbedHtml", 
           condition = input$sel_autodistribution_reportFormat == "html")
  })
  
  # AutoDistribute - Event Observers --------------------------------
  
  observeEvent(
    input$rdo_autodistribution, 
    {
      rv_AutoDist$SelectedConfig <- 
        rv_AutoDist$Config[rv_AutoDist$Config$OID == as.numeric(input$rdo_autodistribution), ]  
    }
  )
  
  observeEvent(
    input$btn_autodistribution_add, 
    {
      rv_AutoDist$ConfigAddEdit <- "Add"
      
      template <- rv_Template$Template$OID
      names(template) <- rv_Template$Template$TemplateName
      
      updateSelectInput(session = session, 
                        inputId = "sel_autodistribution_parentReportTemplate", 
                        choices = template)
      
      updateTextInput(session = session, 
                      inputId = "dttm_autodistribution_startDateTime", 
                      value = format(Sys.time(), 
                                     format = "%Y-%m-%d 00:00"))
      
      updateNumericInput(session = session, 
                         inputId = "num_autodistribution_delayAfterInstanceEnd", 
                         value = 0)
      
      updateSelectInput(session = session, 
                        inputId = "sel_autodistribution_delayUnits", 
                        selected = "Hour")
      
      updateSelectInput(session = session, 
                        inputId = "sel_autodistribution_currentOrLastInstance", 
                        selected = "LastCompleted")
      
      updateCheckboxInput(session = session, 
                          inputId = "chk_autodistribution_isActive", 
                          value = TRUE)
      
      updateCheckboxInput(session = session, 
                          inputId = "chk_autodistribution_isAddToArchive", 
                          value = FALSE)
      updateCheckboxInput(session = session, 
                          inputId = "chk_autodistribution_isDistributeInternalOnly", 
                          value = TRUE)
      updateSelectInput(session = session, 
                        inputId = "sel_autodistribution_reportFormat", 
                        selected = "pdf")
      updateCheckboxInput(session = session, 
                          inputId = "chk_autodistribution_isEmbedHtml", 
                          value = TRUE)
      
      toggleModal(session = session, 
                  modalId = "modal_autodistribution_addEdit", 
                  toggle = "open")
    }
  )
  
  observeEvent(
    input$btn_autodistribution_edit, 
    {
      rv_AutoDist$ConfigAddEdit <- "Edit"
      
      template <- rv_Template$Template$OID
      names(template) <- rv_Template$Template$TemplateName
      
      updateSelectInput(session = session, 
                        inputId = "sel_autodistribution_parentReportTemplate", 
                        choices = template, 
                        selected = rv_AutoDist$SelectedConfig$ParentReportTemplate)
      
      updateTextInput(session = session, 
                      inputId = "dttm_autodistribution_startDateTime", 
                      value = format(rv_AutoDist$SelectedConfig$StartDateTime, 
                                     format = "%Y-%m-%d %H:%m"))
      
      updateNumericInput(session = session, 
                         inputId = "num_autodistribution_delayAfterInstanceEnd", 
                         value = rv_AutoDist$SelectedConfig$DelayAfterInstanceEnd)
      
      updateSelectInput(session = session, 
                        inputId = "sel_autodistribution_delayUnits", 
                        selected = rv_AutoDist$SelectedConfig$DelayUnits)
      
      updateSelectInput(session = session, 
                        inputId = "sel_autodistribution_currentOrLastInstance", 
                        selected = rv_AutoDist$SelectedConfig$CurrentOrLastInstance)
      
      updateCheckboxInput(session = session, 
                          inputId = "chk_autodistribution_isActive", 
                          value = rv_AutoDist$SelectedConfig$IsActive)
      
      updateCheckboxInput(session = session, 
                          inputId = "chk_autodistribution_isAddToArchive", 
                          value = rv_AutoDist$SelectedConfig$IsAddToArchive)
      updateCheckboxInput(session = session, 
                          inputId = "chk_autodistribution_isDistributeInternalOnly", 
                          value = rv_AutoDist$SelectedConfig$IsDistributeInternalOnly)
      updateSelectInput(session = session, 
                        inputId = "sel_autodistribution_reportFormat", 
                        selected = rv_AutoDist$SelectedConfig$ReportFormat)
      updateCheckboxInput(session = session, 
                          inputId = "chk_autodistribution_isEmbedHtml", 
                          value = rv_AutoDist$SelectedConfig$IsEmbedHtml)
      
      toggleModal(session = session, 
                  modalId = "modal_autodistribution_addEdit", 
                  toggle = "open")
    }
  )
  
  observeEvent(
    input$btn_autodistribution_saveConfig,
    {
      oid <- switch(rv_AutoDist$ConfigAddEdit,
                    "Add" = numeric(0), 
                    as.numeric(input$rdo_autodistribution))

      date_format <- 
        if (grepl("^\\d{4}", input$dttm_autodistribution_startDateTime)){
          "%Y-%m-%d %H:%M"
        } else {
          "%d-%b-%Y %H:%M"
        }
      
      addEditAutoDistribution(oid = oid,
                              parent_report_template = as.numeric(input$sel_autodistribution_parentReportTemplate),
                              start_date_time = as.POSIXct(input$dttm_autodistribution_startDateTime,
                                                           format = date_format,
                                                           tz = "UTC"),
                              is_active = input$chk_autodistribution_isActive,
                              delay_after_instance_end = input$num_autodistribution_delayAfterInstanceEnd,
                              delay_units = input$sel_autodistribution_delayUnits,
                              current_or_last_instance = input$sel_autodistribution_currentOrLastInstance,
                              is_add_to_archive = input$chk_autodistribution_isAddToArchive,
                              report_format = input$sel_autodistribution_reportFormat,
                              is_distribute_internal_only = input$chk_autodistribution_isDistributeInternalOnly,
                              is_embed_html = input$chk_autodistribution_isEmbedHtml,
                              event_user = CURRENT_USER_OID())
      
      NewData <- queryAutoDistribution()
      
      rv_AutoDist$Config <- NewData
      
      NewData %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_autodistribution", 
                       checked = as.numeric(input$rdo_autodistribution)) %>% 
        DT::replaceData(proxy_dt_autodistribution,
                        ., 
                        resetPaging = FALSE,
                        rownames = FALSE)
      
      rv_AutoDist$SelectedConfig <- 
        rv_AutoDist$Config[rv_AutoDist$Config$OID == as.numeric(input$rdo_autodistribution), ]  
      
      toggleModal(session = session, 
                  modalId = "modal_autodistribution_addEdit", 
                  toggle = "close")
    }
  )
  
  observeEvent(
    input$btn_autodistribution_deactivate, 
    {
      activateRecord(oid = as.numeric(input$rdo_autodistribution), 
                     active = FALSE, 
                     event_user = CURRENT_USER_OID(), 
                     table_name = "AutoDistribution", 
                     event_table_name = "AutoDistributionEvent", 
                     parent_field_name = "ParentAutoDistribution")
      
      NewData <- queryAutoDistribution()
      
      rv_AutoDist$Config <- NewData
      
      NewData %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_autodistribution", 
                       checked = as.numeric(input$rdo_autodistribution)) %>% 
        DT::replaceData(proxy_dt_autodistribution,
                        ., 
                        resetPaging = FALSE,
                        rownames = FALSE)
      
      rv_AutoDist$SelectedConfig <- 
        rv_AutoDist$Config[rv_AutoDist$Config$OID == as.numeric(input$rdo_autodistribution), ]  
    }
  )
  
  observeEvent(
    input$btn_autodistribution_activate, 
    {
      activateRecord(oid = as.numeric(input$rdo_autodistribution), 
                     active = TRUE, 
                     event_user = CURRENT_USER_OID(), 
                     table_name = "AutoDistribution", 
                     event_table_name = "AutoDistributionEvent", 
                     parent_field_name = "ParentAutoDistribution")
      
      NewData <- queryAutoDistribution()
      
      rv_AutoDist$Config <- NewData
      
      NewData %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_autodistribution", 
                       checked = as.numeric(input$rdo_autodistribution)) %>% 
        DT::replaceData(proxy_dt_autodistribution,
                        ., 
                        resetPaging = FALSE,
                        rownames = FALSE)
      
      rv_AutoDist$SelectedConfig <- 
        rv_AutoDist$Config[rv_AutoDist$Config$OID == as.numeric(input$rdo_autodistribution), ]  
    }
  )
  
  # AutoDistribute - Output -----------------------------------------
  
  output$dt_autodistribution <- 
    DT::renderDataTable({
      queryAutoDistribution() %>% 
        radioDataTable(id_variable = "OID", 
                       element_name = "rdo_autodistribution") %>% 
        RM_datatable(escape = -1) %>% 
        DT::formatDate(columns = c("StartDateTime"),
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
  
  proxy_dt_autodistribution <- DT::dataTableProxy("dt_autodistribution")
  
  # Settings - Reactive Values --------------------------------------
  
  rv_Settings <- reactiveValues(
    Settings = SETTINGS
  )
  
  # Settings - Passive Observers ------------------------------------
  
  observe({
    toggleState(id = "btn_setting_openEdit", 
                condition = USER_IS_REPORT_ADMIN())
  })
  
  # Settings - Event Observers --------------------------------------
  
  observeEvent(
    rv_Settings$Settings,
    {
      # Update SMTP Server
      
      SMTP <- SETTINGS$SettingValue[SETTINGS$SettingKey == "smtpServer"]
      
      if (!is.na(SMTP)){
        options(RM_smtpServer = SMTP)
      }
      
      # Update ZIP Executable 
      
      ZIP <- SETTINGS$SettingValue[SETTINGS$SettingKey == "zipExecutable"]
      
      if (!is.na(ZIP) && trimws(ZIP) != ""){
        Sys.setenv("R_ZIPCMD" = ZIP)
      } else {
        Sys.setenv("R_ZIPCMD" ="zip")
      }
      
      RSCRIPT <- trimws(SETTINGS$SettingValue[SETTINGS$SettingKey == "rScript"])
      
      if (!isTRUE(length(RSCRIPT) == 0 | RSCRIPT %in% c(NA, ""))){
        Sys.setenv(PATH = paste0(c(Sys.getenv("PATH"), 
                                   RSCRIPT), 
                                 collapse = ";"))
      }
      
      PANDOC <- trimws(SETTINGS$SettingValue[SETTINGS$SettingKey == "pandocDirectory"])
      
      if (!isTRUE(length(PANDOC) == 0 | PANDOC %in% c(NA, ""))){
        rmarkdown::find_pandoc(dir = PANDOC, 
                               cache = FALSE)
        Sys.setenv(PATH = paste0(c(Sys.getenv("PATH"), 
                                   PANDOC), 
                                 collapse = ";"))
        Sys.setenv(RSTUDIO_PANDOC = PANDOC)
      }
      
      LATEX <- trimws(SETTINGS$SettingValue[SETTINGS$SettingKey == "latexDirectory"])
      
      if (!isTRUE(length(LATEX) == 0 | LATEX %in% c(NA, ""))){
        Sys.setenv(PATH = paste0(c(Sys.getenv("PATH"), 
                                   LATEX), 
                                 collapse = ";"))
      }
    }
  )
  
  observeEvent(
    input$btn_setting_openEdit, 
    {
      disable(id = "btn_setting_openEdit")
      enable(id = "txt_setting_smtpServer")
      enable(id = "sel_setting_defaultReportFormat")
      enable(id = "sel_setting_htmlEmbed")
      enable(id = "txt_setting_zipExecutable")
      enable(id = "txt_setting_rScript")
      enable(id = "txt_setting_pandocDirectory")
      enable(id = "txt_setting_latexDirectory")
      enable(id = "btn_setting_saveSettings")
    }
  )
  
  observeEvent(
    input$btn_setting_saveSettings, 
    {
      SettingData <- data.frame(SettingKey = c("smtpServer", 
                                               "defaultReportFormat", 
                                               "htmlEmbed", 
                                               "zipExecutable", 
                                               "pandocDirectory", 
                                               "latexDirectory", 
                                               "rScript"), 
                                SettingValue = c(input$txt_setting_smtpServer, 
                                                 input$sel_setting_defaultReportFormat, 
                                                 input$sel_setting_htmlEmbed, 
                                                 input$txt_setting_zipExecutable, 
                                                 input$txt_setting_pandocDirectory, 
                                                 input$txt_setting_latexDirectory, 
                                                 input$txt_setting_rScript), 
                                stringsAsFactors = FALSE)
      
      for (i in seq_len(nrow(SettingData))){
        this_oid <-  queryApplicationSettingByKey(SettingData$SettingKey[i])$OID
        addEditApplicationSetting(oid = this_oid, 
                                  setting_key = SettingData$SettingKey[i], 
                                  setting_value = SettingData$SettingValue[i], 
                                  event_user = CURRENT_USER_OID())
      }
      
      rv_Settings$Settings <- queryApplicationSetting()
      
      toggleState(id = "btn_setting_openEdit",
                  condition = USER_IS_REPORT_ADMIN())
      disable(id = "txt_setting_smtpServer")
      disable(id = "sel_setting_defaultReportFormat")
      disable(id = "sel_setting_htmlEmbed")
      disable(id = "txt_setting_rScript")
      disable(id = "txt_setting_zipExecutable")
      disable(id = "txt_setting_pandocDirectory")
      disable(id = "txt_setting_latexDirectory")
      disable(id = "btn_setting_saveSettings")
    }
  )
  
  # Settings - Output -----------------------------------------------
  # Stop App when Session Ends --------------------------------------
  
  observe({
    lapply(
      c("dt_genReport_reportInstanceSubmit_revisionHistory"),
      function(x) outputOptions(output, x, suspendWhenHidden = FALSE)
    )
  })
  
  session$onSessionEnded(function(){ 
    stopApp()
  })
})
