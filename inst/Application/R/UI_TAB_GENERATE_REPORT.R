UI_TAB_GENERATE_REPORT <- 
  tabsetPanel(
    id = "tabset_generateReport", 
    type = "pills",
    tabPanel(title = "Report", 
             radioDataTableOutput("dt_genReport_template", 
                                  "rdo_genReport_template")),
    tabPanel(title = "Instance"),
    tabPanel(title = "Notes"),
    tabPanel(title = "Signatures"),
    tabPanel(title = "Narrative"),
    tabPanel(title = "Preview"),
    tabPanel(title = "Archival & Submission"),
    tabPanel(title = "Archived Reports")
  )