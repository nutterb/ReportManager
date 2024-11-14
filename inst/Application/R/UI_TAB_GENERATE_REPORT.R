UI_TAB_GENERATE_REPORT <- 
  tabsetPanel(
    id = "tabset_generateReport", 
    type = "pills",
    tabPanel(title = "Report", 
             radioDataTableOutput("dt_genReport_template", 
                                  "rdo_genReport_template")),
    tabPanel(title = "Instance", 
             collapsibleDiv(title = "Scheduled Reports",
                            id = "cd_genReport_scheduledReport", 
                            checked = TRUE), 
             collapsibleDiv(title = "Unscheduled Reports", 
                            id = "cd_genReport_unscheduledReport"), 
             collapsibleDiv(title = "Adhoc Reporting", 
                            id = "ch_genReport_adhocReport")),
    tabPanel(title = "Notes"),
    tabPanel(title = "Signatures"),
    tabPanel(title = "Narrative"),
    tabPanel(title = "Preview"),
    tabPanel(title = "Archival & Submission"),
    tabPanel(title = "Archived Reports")
  )