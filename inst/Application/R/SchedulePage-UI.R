UI_TAB_SCHEDULE_PAGE <- 
  tabsetPanel(
    id = "tabpan_scheduleManagement", 
    type = "pills", 
    tabPanel(
      title = "Schedules", 
      disabled(actionButton(inputId = "btn_schedule_addSchedule", 
                            label = "Add Schedule", 
                            style = "float:right;")), 
      disabled(actionButton(inputId = "btn_schedule_editSchedule", 
                            label = "Edit Schedule", 
                            style = "float:right;")), 
      disabled(actionButton(inputId = "btn_schedule_deactivate", 
                            label = "Disable", 
                            style = "float:right;")), 
      disabled(actionButton(inputId = "btn_schedule_activate", 
                            label = "Enable", 
                            style = "float:right;")), 
      radioDataTableOutput(outputId = "dt_schedule", 
                           radioId = "rdo_schedule")
    ), 
    tabPanel(
      title = "Date Reporting Formats", 
      disabled(actionButton(inputId = "btn_dateFormat_addFormat", 
                            label = "Add Format", 
                            style = "float:right;")), 
      disabled(actionButton(inputId = "btn_dateFormat_editFormat", 
                            label = "Edit Format", 
                            style = "float:right;")), 
      disabled(actionButton(inputId = "btn_dateFormat_deactivate", 
                            label = "Disable", 
                            style = "float:right;")), 
      disabled(actionButton(inputId = "btn_dateFormat_activate", 
                            label = "Enable", 
                            style = "float:right;")), 
      radioDataTableOutput(outputId = "dt_dateFormat", 
                           radioId = "rdo_dateFormat")
    )
  )
