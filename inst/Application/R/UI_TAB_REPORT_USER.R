UI_TAB_REPORT_USER <- 
  tagList(
    disabled(actionButton(inputId = "btn_reportUser_add", 
                          label = "Add User", 
                          style = "float:right")),
    disabled(actionButton(inputId = "btn_reportUser_edit", 
                          label = "Edit User", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_reportUser_activate", 
                          label = "Activate User", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_reportUser_deactivate", 
                          label = "Deactivate User", 
                          style = "float:right")), 
    radioDataTableOutput(outputId = "dt_reportUser", 
                         radioId = "rdo_reportUser")
  )
