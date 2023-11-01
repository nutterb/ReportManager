UI_TAB_REPORT_USER <- 
  tagList(
    disabled(actionButton(inputId = "btn_user_add", 
                          label = "Add User", 
                          style = "float:right")),
    disabled(actionButton(inputId = "btn_user_edit", 
                          label = "Edit User", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_user_activate", 
                          label = "Activate User", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_user_deactivate", 
                          label = "Deactivate User", 
                          style = "float:right")), 
    radioDataTableOutput(outputId = "dt_user", 
                         radioId = "rdo_user")
  )
