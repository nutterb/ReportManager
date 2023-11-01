UI_TAB_ROLES <- 
  tagList(
    disabled(actionButton(inputId = "btn_role_add", 
                          label = "Add Role", 
                          style = "float:right")),
    disabled(actionButton(inputId = "btn_role_edit", 
                          label = "Edit Role", 
                          style = "float:right")),
    disabled(actionButton(inputId = "btn_role_activate", 
                          label = "Activate Role", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_role_deactivate", 
                          label = "Deactivate Role", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_role_viewEdit", 
                          label = "View/Edit Assignments", 
                          style = "float:right")), 
    radioDataTableOutput(outputId = "dt_role", 
                         radioId = "rdo_role")
  )
