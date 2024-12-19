MODAL_REPORT_INSTANCE_DISTRIBUTE <- 
  bsModal(
    id = "modal_genReport_reportInstance_distribute", 
    title = textOutput("txt_genReport_reportInstance_distributeTitle"), 
    trigger = "trg_none", 
    size = "large", 
    fluidRow(
      column(width = 2, 
             radioButtons(inputId = "rdo_genReport_reportInstance_format", 
                          label = "Report Format", 
                          choices = c("PDF", "HTML"), 
                          selected = "PDF")),
      column(width = 2,
             disabled(radioButtons(inputId = "rdo_genReport_reportInstance_embedHtml", 
                                   label = "HTML Format", 
                                   choices = c("Attached", "Embedded"), 
                                   selected = "Attached")))
    ), 
    textAreaInput(inputId = "txt_genReport_reportInstance_emailMessage", 
                  label = "Email Message", 
                  width = "70%", 
                  height = "400px"), 
    actionButton(inputId = "btn_genReport_reportInstance_sendReport", 
                 label = textOutput("txt_genReport_reportInstance_sendReportButtonLabel")),
    hidden(p(id = "p_genReport_reportInstanceSubmit_makingReport", 
             "Generating report...", 
             style = "font-weight:bold;color:#3F75A1;float:right;"))
  )