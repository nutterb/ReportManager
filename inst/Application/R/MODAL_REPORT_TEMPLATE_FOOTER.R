MODAL_REPORT_TEMPLATE_FOOTER <- 
  bsModal(
    id = "modal_templateFooter_edit", 
    title = "Assign Footers to Template", 
    trigger = "trg_none", 
    size = "large", 
    multiSelect(inputId = "templateFooter", 
                label = "Footers", 
                choices = character(0), 
                selected = character(0), 
                size = 10,
                up_down = TRUE), 
    br(),
    br(),
    actionButton(inputId = "btn_templateFooter_addEdit", 
                 label = "Save")
  )
