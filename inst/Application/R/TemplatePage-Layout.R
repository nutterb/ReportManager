UI_TAB_REPORT_TEMPLATE_PAGE <- 
  tabsetPanel(
    id = "tabset_reportTemplate",
    type = "pills",
    header = uiOutput("title_reportTemplateTabset"),
    tabPanel(
      title = "Template",
      RM_tabLayout("template")
    ),
    tabPanel(
      title = "Schedule"
    ),
    tabPanel(
      title = "Layout"
    ),
    tabPanel(
      title = "Signatures"
    ),
    tabPanel(
      title = "Distribution"
    ),
    tabPanel(
      title = "Permissions"
    )
  )

MODAL_REPORT_TEMPLATE <- 
  bsModal(
    id = "modal_reportTemplate", 
    title = uiOutput("title_reportTemplateModal"), 
    size = "large", 
    trigger = "trg_none"
  )