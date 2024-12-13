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
    # NARRATIVE TAB -------------------------------------------------
    tabPanel(title = "Narrative",
             h3(id = "h3_genReport_reportInstanceNarrative_noInstanceSelected", 
                "No Report Instance Selected"),
             hidden(
               div(
                 id = "div_genReport_reportInstanceNarrative",
                 disabled(actionButton(inputId = "btn_reportInstanceNarrativeDiscard", 
                                       label = "Discard changes",
                                       style = "float:right")),
                 disabled(actionButton(inputId = "btn_reportInstanceNarrativeSave", 
                                       label = "Save",
                                       style = "float:right")),
                 disabled(actionButton(inputId = "btn_reportInstanceNarrativeEdit", 
                                       label = "Edit",
                                       style = "float:right")),
                 disabled(textAreaInput(inputId = "txt_reportInstanceNarrative", 
                                        label = "Narrative", 
                                        width = "100%", 
                                        height = "500px")), 
                 collapsibleDiv(
                   title = "Narrative History",
                   id = "cd_genReport_narrativeHistory", 
                   checked = FALSE, 
                   DT::dataTableOutput("dt_reportInstanceNarrativeHistory")
                 )
               )
             )
    ),
    
    # SIGNATURES TAB ------------------------------------------------
    tabPanel(title = "Signatures",
             h3(id = "h3_genReport_reportInstanceSignature_noInstanceSelected", 
                "No Report Instance Selected"),
             hidden(
               div(
                 id = "div_genReport_reportInstanceSignature", 
                 br(),
                 uiOutput("tbl_genReport_reportInstanceSignature"),
                 
                 collapsibleDiv(
                   title = "Signature History", 
                   id = "cd_genReport_reportInstanceSignatureHistory", 
                   checked = FALSE, 
                   DT::dataTableOutput("dt_reportInstanceSignatureHistory")
                 )
               )
             )
    ),
    
    # PREVIEW TAB ---------------------------------------------------
    tabPanel(title = "Preview", 
             h3(id = "h3_genReport_reportInstancePreview_noInstanceSelected", 
                "No Report Instance Selected"), 
             hidden(
               div(
                 id = "div_genReport_reportInstancePreview",
                 div(
                   downloadButton(outputId = "btn_genReport_reportInstancePreview_html", 
                                  label = "HTML", 
                                  style = "float:right;"),
                   downloadButton(outputId = "btn_genReport_reportInstancePreview_pdf", 
                                  label = "PDF", 
                                  style = "float:right;"), 
                   actionButton(inputId = "btn_genReport_reportInstancePreview_preview", 
                                label = "Preview", 
                                style = "float:right;"), 
                   hidden(actionButton(inputId = "btn_genReport_reportInstancePreview_shiny", 
                                       label = "Configuration", 
                                       style = "float:right;")),
                   checkboxGroupInput(inputId = "chkgrp_genReport_reportInstancePreview_supplementalFile", 
                                      label = "Include with Download", 
                                      choices = c("Data", 
                                                  "Images"), 
                                      width = "100%"),
                   style = "float:right"), 
                 br(), 
                 div(
                   htmlOutput("html_genReport_reportInstancePreview")
                 )
               )
             )
    ),
    
    # SUBMISSION TAB ------------------------------------------------
    tabPanel(title = "Archival & Submission",
             h3(id = "h3_genReport_reportInstanceSubmit_noInstanceSelected", 
                "No Report Instance Selected"),
             hidden(
               div(
                 id = "div_genReport_reportInstanceSubmit", 
                 fluidRow(
                   column(
                     width = 5, 
                     div(
                       style = "border:solid gray 1px;border-radius:10px;padding-left:10px;padding-right:10px;height:2.2in;overflow-y:scroll; max-height:200px; width:100%;",
                       fluidRow(
                         column(
                           width = 6,
                           checkboxGroupInput(
                             inputId = "chk_genReport_reportInstanceSubmit_archiveDistribute", 
                             label = "Archive / Distribute", 
                             choices = c("Add to Archive", 
                                         "Distribute Internally")
                           )
                         ),
                         column(
                           width = 6,
                           actionButton(inputId = "btn_genReport_reportInstanceSubmit_archiveDistribute", 
                                        label = "Add and/or Distribute", 
                                        style = "float:right")
                         )
                       ),
                       hr(style = "border-top:solid black 1pt"), 
                       disabled(actionButton(inputId = "btn_genReport_reportInstanceSubmit_submitToClient", 
                                             label = "Submit to Client")),
                       disabled(actionButton(inputId = "btn_genReport_reportInstanceSubmit_startRevision", 
                                             label = "Start Revision", 
                                             style = "float:right"))
                     )
                   ), 
                   column(
                     width = 7, 
                     div(
                       style = "border:solid black 1px;border-radius:10px;padding-left:10px;padding-right:10px;height:2.2in;overflow-y:scroll; max-height:200px; width:100%;"
                     )
                   )
                 ), 
                 collapsibleDiv(title = "Distribution",
                                id = "cd_genReport_reportInstanceSubmit_archiveReviseHistory",
                                checked = TRUE,
                                fluidRow(
                                  actionButton(inputId = "btn_genReport_reportInstanceSubmission_editDistributionList", 
                                               label = "Edit Distribution List", 
                                               style = "float:right", 
                                               width = "175px")
                                ), 
                                fluidRow(
                                  p("Changes made using the actions below will not peresist beyond this session.",
                                    style = "float:right;")
                                ),
                                fluidRow(
                                  disabled(actionButton(inputId = "btn_genReport_reportInstanceSubmission_changeTestEmailStatus", 
                                                        label = "Toggle Test on Selected", 
                                                        style = "float:right;", 
                                                        width = "175px")),
                                  actionButton(inputId = "btn_genReport_reportInstanceSubmission_removeAllIncludeInTest", 
                                               label = "Remove All From Testing", 
                                               style = "float:right;", 
                                               width = "175px"),
                                  actionButton(inputId = "btn_genReport_reportInstanceSubmission_addAllIncludeInTest", 
                                               label = "Add All to Testing", 
                                               style = "float:right;", 
                                               width = "175px"),
                                  
                                ),
                                fluidRow(
                                  disabled(actionButton(inputId = "btn_genReport_reportInstanceSubmission_changeActiveStatus", 
                                                        label = "Toggle Active on Selected", 
                                                        style = "float:right;", 
                                                        width = "175px")),
                                  actionButton(inputId = "btn_genReport_reportInstanceSubmission_deactivateAll", 
                                               label = "Deactivate All", 
                                               style = "float:right;", 
                                               width = "175px"),
                                  actionButton(inputId = "btn_genReport_reportInstanceSubmission_activateAll", 
                                               label = "Activate All", 
                                               style = "float:right;", 
                                               width = "175px"),
                                  
                                ),
                                radioDataTableOutput(outputId = "dt_genReport_reportInstanceSubmit_distribution", 
                                                     radioId = "rdo_genReport_reportInstanceSubmit_distribution"))
               )
             )
    ),
    
    # ARCHIVED REPORTS TAB ------------------------------------------
    tabPanel(title = "Archived Reports", 
             h3(id = "h3_genReport_archivedReport_noTemplateSelected", 
                "No Report Template Selected"), 
             hidden(
               div(id = "div_genReport_archivedReport", 
                   disabled(downloadButton(outputId = "dwn_genReport_archivedReport", 
                                           label = "Download Selected Report", 
                                           style = "float:right;")),
                   radioDataTableOutput(outputId = "dt_genReport_archivedReport", 
                                        radioId = "rdo_genReport_archivedReport")
               )
             )
    )
  )