UI_TAB_GENERATE_REPORT <- 
  tabsetPanel(
    id = "tabset_generateReport", 
    type = "pills",
    # REPORT TEMPLATE TAB -------------------------------------------
    tabPanel(title = "Report", 
             radioDataTableOutput("dt_genReport_template", 
                                  "rdo_genReport_template")),
    # INSTANCE TAB --------------------------------------------------
    tabPanel(title = "Instance", 
             h3(id = "h3_genReport_reportInstance_noTemplateSelected", 
               "No Report Template Selected"),
             hidden(div(
               id = "div_genReport_reportInstance",
               collapsibleDiv(
                 title = "Scheduled Reports",
                 id = "cd_genReport_scheduledReport", 
                 checked = TRUE, 
                 radioDataTableOutput(outputId = "dt_instance_scheduled", 
                                      radioId = "rdo_report_instance_scheduled")), 
               collapsibleDiv(
                 title = "Unscheduled Reports", 
                 id = "cd_genReport_unscheduledReport",
                 fluidRow(
                   column(width = 4,
                          dateTimeInput(inputId = "dttm_genReport_newUnscheduledInstance", 
                                        label = "Date Range", 
                                        start = NULL, 
                                        end = NULL, 
                                        single_date = FALSE, 
                                        timepicker = TRUE, 
                                        style = "width:250px;")
                   ),
                   column(width = 4,
                          checkboxInput(inputId = "chk_genReport_unscheduled_isSignatureRequired", 
                                        label = "Require signatures", 
                                        value = FALSE)
                   ),
                   column(width = 4,
                          actionButton(inputId = "btn_genReport_unscheduled_addUnscheduledReport", 
                                       label = "Save New Instance", 
                                       style = "float:right;"),
                          disabled(actionButton(inputId = "btn_genReport_unscheduled_changeSignatureRequirement", 
                                                label = "Change Signature Requirement", 
                                                style = "float:right;"))
                   )
                 ),
                 radioDataTableOutput(outputId = "dt_instance_unscheduled", 
                                      radioId = "rdo_report_instance_unscheduled")), 
               collapsibleDiv(title = "Adhoc Reporting", 
                              id = "cd_genReport_adhocReport", 
                              dateTimeInput(inputId = "dttm_genReport_adhocInstance", 
                                            label = "Date Range", 
                                            start = NULL, 
                                            end = NULL, 
                                            single_date = FALSE, 
                                            timepicker = TRUE, 
                                            style = "width:250px;"))
             ))
    ),
    # NOTES TAB -----------------------------------------------------
    tabPanel(title = "Notes",
             h3(id = "h3_genReport_reportInstanceNote_noInstanceSelected", 
                "No Report Instance Selected"),
             hidden(
               div(
                 id = "div_genReport_reportInstanceNote",
                 fluidRow(
                   column(width = 4, 
                          hidden(textAreaInput(inputId = "txt_reportInstanceNote", 
                                               label = "Add a Note", 
                                               width = "100%", 
                                               height = "200px")),
                          hidden(actionButton(inputId = "btn_addReportInstanceNote", 
                                              label = "Save"))),
                   
                   column(width = 8, 
                          DT::dataTableOutput("dt_reportInstanceNote"))
                 )
               )
             )
    ),
    tabPanel(title = "Narrative"),
    tabPanel(title = "Signatures"),
    tabPanel(title = "Preview"),
    tabPanel(title = "Archival & Submission"),
    tabPanel(title = "Archived Reports")
  )