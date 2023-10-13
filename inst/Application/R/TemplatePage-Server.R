# ObserveEvent - btn_template_add -----------------------------------

OE_btn_template_add <- function(session, 
                                rv_Template, 
                                output){
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

# Observe Event - btn_template_edit ---------------------------------

OE_btn_template_edit <- function(session, 
                                 rv_Template){
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

# Observe Event - btn_template_addEdit ------------------------------

OE_btn_template_add_edit <- function(session, 
                                     rv_Template, 
                                     input, 
                                     current_user_oid, 
                                     proxy){
  oid <- if (rv_Template$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_template)
  
  addEditReportTemplate(oid = oid, 
                        template_directory = input$sel_template_directory, 
                        template_file = input$sel_template_file, 
                        title = input$txt_template_title, 
                        title_size = input$sel_template_titleSize, 
                        is_signature_required = input$chk_template_isSignatureRequired, 
                        is_active = input$chk_template_isActive, 
                        logo_oid = as.numeric(input$sel_template_logo), 
                        event_user = current_user_oid)
  
  RM_replaceData(query_fun = queryReportTemplate, 
                 reactive_list = rv_Template, 
                 data_slot = "Template", 
                 selected_slot = "SelectedTemplate", 
                 id_variable = "OID", 
                 element_name = "rdo_template", 
                 oid = oid, 
                 proxy = proxy, 
                 cols = REPORT_TEMPLATE_DISPLAY_PROPERTIES)
  
  toggleModal(session = session, 
              modalId = "modal_template_addEdit", 
              toggle = "close")
}

# Observe Event - sel_template_directory ----------------------------

OE_sel_template_directory <- function(session, 
                                      input){
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

# Observe Event - sel_template_logo ---------------------------------

OE_sel_template_logo <- function(input, 
                                 output){
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

# Observe Event - btn_template_activate/deactivate ------------------

OE_btn_template_activate_deactivate <- function(activate, 
                                                input, 
                                                current_user_oid, 
                                                rv_Template, 
                                                proxy){
  oid <- as.numeric(input$rdo_template)
  
  activateRecord(oid, 
                 active = activate, 
                 event_user = current_user_oid, 
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
                 proxy = proxy, 
                 cols = REPORT_TEMPLATE_DISPLAY_PROPERTIES)
}

# Observe Event - btn_reportTemplate_disclaimer_addEdit -------------

OE_btn_reportTemplate_disclaimer_addEdit <- function(session,
                                                     input, 
                                                     rv_Template,
                                                     current_user_oid,
                                                     proxy){
  Disclaimer <- jsonlite::fromJSON(input$reportTemplate_disclaimer)
  
  Input <- Disclaimer[c("choices", "order", "selected")]
  names(Input) <- c("ParentDisclaimer", "Order", "IsActive")
  
  Input <- merge(Input, 
                 rv_Template$SelectedTemplateDisclaimer[c("OID", "ParentDisclaimer", "ParentReportTemplate")], 
                 by = "ParentDisclaimer", 
                 all.x = TRUE, 
                 all.y = TRUE)
  
  for(i in seq_len(nrow(Input))){
    addEditReportTemplateDisclaimer(
      oid = if (is.na(Input$OID[i])) numeric(0) else Input$OID[i],
      parent_report_template = as.numeric(input$rdo_template),
      parent_disclaimer = as.numeric(Input$ParentDisclaimer[i]),
      order = Input$Order[i],
      is_active = isTRUE(Input$IsActive[i]),
      event_user = current_user_oid
    )
  }
  
  New <- queryReportTemplateDisclaimer(parent_report_template = as.numeric(input$rdo_template))
  rv_Template$SelectedTemplateDisclaimer <- New
  DT::replaceData(proxy = proxy,
                  data = New,
                  resetPaging = FALSE,
                  rownames = FALSE)
  
  toggleModal(session = session, 
              modalId = "modal_templateDisclaimer_edit", 
              toggle = "close")
}

# Observe Event - btn_reportTemplate_footer_addEdit -----------------

OE_btn_reportTemplate_footer_addEdit <- function(session,
                                                 input, 
                                                 rv_Template,
                                                 current_user_oid,
                                                 proxy){
  Footer <- jsonlite::fromJSON(input$reportTemplate_footer)
  
  Input <- Footer[c("choices", "order", "selected")]
  names(Input) <- c("ParentDisclaimer", "Order", "IsActive")
  
  Input <- merge(Input, 
                 rv_Template$SelectedTemplateFooter[c("OID", "ParentFooter", "ParentReportTemplate")], 
                 by = "ParentFooter", 
                 all.x = TRUE, 
                 all.y = TRUE)
  
  for(i in seq_len(nrow(Input))){
    addEditReportTemplateFooter(
      oid = if (is.na(Input$OID[i])) numeric(0) else Input$OID[i],
      parent_report_template = as.numeric(input$rdo_template),
      parent_footer = as.numeric(Input$ParentFooter[i]),
      order = Input$Order[i],
      is_active = isTRUE(Input$IsActive[i]),
      event_user = current_user_oid
    )
  }
  
  New <- queryReportTemplateFooter(parent_report_template = as.numeric(input$rdo_template))
  rv_Template$SelectedTemplateFooter <- New
  DT::replaceData(proxy = proxy,
                  data = New,
                  resetPaging = FALSE,
                  rownames = FALSE)
  
  toggleModal(session = session, 
              modalId = "modal_templateFooter_edit", 
              toggle = "close")
}

# makeTemplateDisclaimerData ----------------------------------------

makeTemplateDisclaimerData <- function(SelectedTemplateDisclaimer, 
                                       Disclaimer){
  Disclaimer <- 
    merge(Disclaimer, 
          SelectedTemplateDisclaimer, 
          by.x = "OID", 
          by.y = "ParentDisclaimer", 
          all.x = TRUE)
  
  is_active <- vapply(Disclaimer$IsActive.x & 
                        Disclaimer$IsActive.y, 
                      isTRUE, 
                      logical(1))
  
  Disclaimer <- Disclaimer[is_active, ]
  Disclaimer <- Disclaimer[order(Disclaimer$Order), ]
  Disclaimer[c("Disclaimer")]
}

# makeTemplateFooterData --------------------------------------------

makeTemplateFooterData <- function(SelectedTemplateFooter, 
                                   Footer){
  Footer <- 
    merge(Footer, 
          SelectedTemplateFooter, 
          by.x = "OID", 
          by.y = "ParentFooter", 
          all.x = TRUE)
  
  is_active <- vapply(Footer$IsActive.x & 
                        Footer$IsActive.y, 
                      isTRUE, 
                      logical(1))
  
  Footer <- Footer[is_active, ]
  Footer <- Footer[order(Footer$Order), ]
  Footer[c("Footer")]
}
