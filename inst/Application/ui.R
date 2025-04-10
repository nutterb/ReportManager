dashboardPage(
  title = "Report Manager",
  # Dashboard Header ------------------------------------------------
  dashboardHeader(
    title = "Report Manager", 
    titleWidth = 250
  ), 
  
  # Dashboard Sidebar -----------------------------------------------
  dashboardSidebar(
    width = 250, 
    UI_SIDEBAR_MENU # Defined in Application/R/ui-sidebar-menu.R
  ), 
  
  # Dashboard Body --------------------------------------------------
  dashboardBody(
    # Styles and javascript -----------------------------------------
    UI_STYLES_AND_SCRIPTS, # Defined in Application/R/ui-styles-scripts.R
    
    # Modals --------------------------------------------------------
    
    MODAL_REPORT_INSTANCE_DISTRIBUTE,
    MODAL_START_REVISION,
    MODAL_REPORT_TEMPLATE,
    MODAL_REPORT_TEMPLATE_DISCLAIMER,
    MODAL_REPORT_TEMPLATE_FOOTER,
    MODAL_REPORT_TEMPLATE_SIGNATURE,
    MODAL_REPORT_TEMPLATE_DISTRIBUTION,
    MODAL_SCHEDULE,
    MODAL_DATE_REPORT_FORMAT,
    MODAL_DISCLAIMER,
    MODAL_FOOTER,
    MODAL_LOGO_ADD,
    MODAL_ROLES,
    MODAL_USER_ROLE,
    MODAL_USER,
    
    bsModal(
      id = "modal_templatePermission_addEdit",
      title = "Edit Template Permissions",
      trigger = "trg_none",
      size = "large",

      selectInput(inputId = "sel_templatePermissionRole",
                  label = "Role", 
                  choices = character(0)),

      checkboxGroupInput(inputId = "chkgrp_templatePermission",
                         label = "Permissions for Role",
                         choices = c("View" = "CanView",
                                     "Add Notes" = "CanAddNotes",
                                     "Edit Narrative" = "CanEditNarrative",
                                     "Submit" = "CanSubmit",
                                     "Start Revision" = "CanStartRevision"),
                         selected = "CanView"),

      actionButton(inputId = "btn_saveTemplatePermission",
                   label = "Save")
    ),
    
    bsModal(
      id = "modal_autodistribution_addEdit", 
      title = "Add/Edit Auto Distribution Configuration", 
      trigger = "trg_none", 
      size = "large", 
      fluidRow(
        column(
          width = 6, 
          selectInput(inputId = "sel_autodistribution_parentReportTemplate", 
                      label = "Report Template", 
                      choices = character(0)), 
          dateTimeInput(inputId = "dttm_autodistribution_startDateTime", 
                        label = "Date/Time to Start Distribution"), 
          numericInput(inputId = "num_autodistribution_delayAfterInstanceEnd", 
                       label = "Delay After End of Instance", 
                       value = 0),
          selectInput(inputId = "sel_autodistribution_delayUnits", 
                      label = "Delay Units", 
                      choices = UNIT_OF_TIME, 
                      selected = "Hour"),
          selectInput(inputId = "sel_autodistribution_currentOrLastInstance", 
                      label = "Current or Last Completed Instance", 
                      choices = c("Current", "LastCompleted"), 
                      selected = "LastCompleted"),
          checkboxInput(inputId = "chk_autodistribution_isActive", 
                        label = "Is Active", 
                        value = TRUE), 
          actionButton(inputId = "btn_autodistribution_saveConfig", 
                       label = "Save")
        ), 
        column(
          width = 6, 
          checkboxInput(inputId = "chk_autodistribution_isAddToArchive", 
                        label = "Add to Archive"), 
          checkboxInput(inputId = "chk_autodistribution_isDistributeInternalOnly", 
                        label = "Distribute Internally Only", 
                        value = TRUE),
          selectInput(inputId = "sel_autodistribution_reportFormat", 
                      label = "Report Format", 
                      choices = c("html", "pdf"), 
                      selected = "pdf"), 
          checkboxInput(inputId = "chk_autodistribution_isEmbedHtml", 
                        label = "Embed HTML into Email", 
                        value = TRUE)
        )
      )
    ),
    
    # Menu Pages ----------------------------------------------------
    
    add_busy_spinner(spin = "fading-circle", 
                     position = "top-right"),
    
    # Generate a Report ---------------------------------------------
    tabItems(
      tabItem("tab_generateReport", UI_TAB_GENERATE_REPORT), 
      
      tabItem("tab_reportTemplate", UI_TAB_REPORT_TEMPLATE_PAGE), 
      
      tabItem("tab_scheduleManagement", UI_TAB_SCHEDULE_PAGE), 
      
      tabItem("tab_reportLayout", UI_TAB_REPORT_LAYOUT), 
      
      tabItem("tab_roles", UI_TAB_ROLES), 
      
      tabItem("tab_users", UI_TAB_REPORT_USER), 
      
      tabItem("tab_autoDistribute", UI_TAB_AUTODISTRIBUTE), 
      
      tabItem("tab_setting", 
              tagList(
                disabled(actionButton(inputId = "btn_setting_openEdit", 
                                      label = "Edit Settings")),
                disabled(textInput(inputId = "txt_setting_smtpServer", 
                                   label = "SMTP server", 
                                   value = SETTINGS$SettingValue[SETTINGS$SettingKey == "smtpServer"])), 
                disabled(selectInput(inputId = "sel_setting_defaultReportFormat", 
                                     label = "Default Report Format", 
                                     choices = c("PDF" = "pdf", 
                                                 "HTML" = "html"),
                                     selected = SETTINGS$SettingValue[SETTINGS$SettingKey == "defaultReportFormat"])), 
                disabled(selectInput(inputId = "sel_setting_htmlEmbed", 
                                     label = "E-mail HTML files as:", 
                                     choices = c("Embedded in Email" = "embed", 
                                                 "Attached to Email" = "attach"),
                                     selected = SETTINGS$SettingValue[SETTINGS$SettingKey == "htmlEmbed"])), 
                disabled(textInput(inputId = "txt_setting_rScript", 
                                   label = "Location of R Script executable", 
                                   value = SETTINGS$SettingValue[SETTINGS$SettingKey == "rScript"],
                                   placeholder = "Leave blank if R is on the System PATH")),
                disabled(textInput(inputId = "txt_setting_zipExecutable", 
                                   label = "Location of ZIP Executable", 
                                   value = SETTINGS$SettingValue[SETTINGS$SettingKey == "zipExecutable"],
                                   placeholder = "Leave blank if ZIP is on the System PATH")), 
                disabled(textInput(inputId = "txt_setting_pandocDirectory", 
                                   label = "Directory of Pandoc", 
                                   value = SETTINGS$SettingValue[SETTINGS$SettingKey == "pandocDirectory"],
                                   placeholder = "Leave blank if PANDOC is on the System PATH")),
                disabled(textInput(inputId = "txt_setting_latexDirectory", 
                                   label = "Directory of LaTeX", 
                                   value = SETTINGS$SettingValue[SETTINGS$SettingKey == "latexDirectory"],
                                   placeholder = "Leave blank if LaTeX is on the System PATH")),
                disabled(actionButton(inputId = "btn_setting_saveSettings", 
                                      label = "Save")))
      )
    )
  )
)