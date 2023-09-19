UI_TAB_USERS <- 
  tagList(
    actionButton(inputId = "btn_activateUser", 
                 label = "Activate User", 
                 style = "float:right"), 
    actionButton(inputId = "btn_deactivateUser", 
                 label = "Deactivate User", 
                 style = "float:right"), 
    actionButton(inputId = "btn_editUser", 
                 label = "Edit User", 
                 style = "float:right"), 
    actionButton(inputId = "btn_addUser", 
                 label = "Add User", 
                 style = "float:right"), 
    radioDataTableOutput(outputId = "dt_reportUser", 
                         radioId = "rdo_reporUser")
  )
