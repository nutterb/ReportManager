UI_TAB_REPORT_TEMPLATE_PAGE <- 
  tabsetPanel(
    id = "tabset_reportTemplate",
    type = "pills",
    # header = uiOutput("title_reportTemplateTabset"),
    tabPanel(
      title = "Template",
      RM_tabLayout("template")
    ),
    tabPanel(
      title = "Schedule", 
      actionButton(inputId = "btn_templateSchedule_edit", 
                   label = "Edit", 
                   style = "float:right;"),
      selectInput(inputId = "sel_templateSchedule", 
                           label = "Schedule", 
                           width = "300px", 
                           choices = character(0),
                           selected = character(0)),
      airDatepickerInput(inputId = "dttm_templateSchedule",
                                  label = "Schedule Start Time",
                                  timepicker = TRUE,
                                  value = Sys.Date()),
      disabled(actionButton(inputId = "btn_templateSchedule_save", 
                            label = "Save")),
    ),
    tabPanel(
      title = "Layout", 
      tabsetPanel(
        id = "tabset_reportTemplateLayout", 
        type = "pills", 
        tabPanel(
          title = "Disclaimers",
          RM_tabLayout("templateDisclaimer", 
                       add = FALSE, 
                       activate = FALSE)
        ),
        tabPanel(
          title = "Footers", 
          RM_tabLayout("templateFooter", 
                       add = FALSE, 
                       activate = FALSE)
        )
      )
    ),
    tabPanel(
      title = "Signatures", 
      RM_tabLayout("templateSignature", 
                   add = FALSE, 
                   edit = TRUE, 
                   activate = FALSE)
    ),
    tabPanel(
      title = "Distribution", 
      RM_tabLayout("templateDistribution", 
                   add = FALSE, 
                   edit = TRUE, 
                   activate = FALSE)
    ),
    tabPanel(
      title = "Permissions"
    )
  )
