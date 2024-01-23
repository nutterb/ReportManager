MODAL_USER_ROLE <-
  bsModal(
    id = "modal_userRole_edit", 
    title = textOutput("title_userRole_edit"), 
    size = "large", 
    trigger = "trg_none", 
    multiSelect(inputId = "multi_userRole", 
                label = "User Role Assignments", 
                choices = character(0), 
                selected = character(0), 
                size = 15), 
    br(),
    actionButton(inputId = "btn_userRole_save", 
                 label = "Save")
  )
