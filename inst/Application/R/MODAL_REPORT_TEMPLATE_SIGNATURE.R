MODAL_REPORT_TEMPLATE_SIGNATURE <- 
  bsModal(
    id = "modal_templateSignature_edit", 
    title = "Edit Template Signatures", 
    trigger = "trg_none", 
    size = "large", 
    multiSelect(inputId = "templateSignature", 
                label = "Required Signatures", 
                choices = character(0), 
                selected = character(0), 
                size = 15,
                up_down = TRUE), 
    br(),
    br(),
    actionButton(inputId = "btn_templateSignature_addEdit", 
                 label = "Save")
  )
