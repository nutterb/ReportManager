MODAL_REPORT_TEMPLATE_DISTRIBUTION <- 
  bsModal(
    id = "modal_templateDistribution_edit", 
    title = "Edit Distribution List", 
    trigger = "trg_none", 
    size = "large", 
    tabsetPanel(
      tabPanel(
        title = "Users",
        multiSelect(inputId = "templateDistributionUser", 
                    label = "Recipients", 
                    choices = character(0), 
                    selected = character(0), 
                    size = 15,
                    up_down = FALSE)
      ), 
      tabPanel(
        title = "Roles", 
        multiSelect(inputId = "templateDistributionRole", 
                    label = "Roles", 
                    choices = character(0), 
                    selected = character(0), 
                    size = 15, 
                    up_down = FALSE)
      )
    ), 
    br(),
    br(),
    actionButton(inputId = "btn_templateDistribution_addEdit", 
                 label = "Save"), 
    hidden(actionButton(inputId = "btn_instanceDistribution_addEdit", 
                        label = "Save"))
  )
