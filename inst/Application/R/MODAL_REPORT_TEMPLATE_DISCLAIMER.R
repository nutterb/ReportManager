MODAL_REPORT_TEMPLATE_DISCLAIMER <- 
  bsModal(
    id = "modal_templateDisclaimer_edit", 
    title = "Assign Disclaimers to Template", 
    trigger = "trg_none", 
    size = "large", 
    multiSelect(inputId = "templateDisclaimer", 
                label = "Disclaimers", 
                choices = character(0), 
                selected = character(0), 
                size = 10,
                up_down = TRUE), 
    br(),
    br(),
    actionButton(inputId = "btn_templateDisclaimer_addEdit", 
                 label = "Save")
  )
