MODAL_REPORT_TEMPLATE_DISTRIBUTION <- 
  bsModal(
    id = "modal_templateDistribution_edit", 
    title = "Edit Template Distribution", 
    trigger = "trg_none", 
    size = "large", 
    multiSelect(inputId = "templateDistribution", 
                label = "Recipients", 
                choices = character(0), 
                selected = character(0), 
                size = 15,
                up_down = TRUE), 
    br(),
    br(),
    actionButton(inputId = "btn_templateDistribution_addEdit", 
                 label = "Save")
  )
