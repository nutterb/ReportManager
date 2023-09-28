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

# Modal - Schedules -------------------------------------------------

MODAL_SCHEDULE <- 
  bsModal(
    id = "modal_schedule_addEdit", 
    title = textOutput("title_schedule_addEdit"), 
    size = "large", 
    trigger = "trg_none", 
    textInput(inputId = "txt_schedule_scheduleName", 
              label = "Schedule Name"), 
    splitLayout(numericInput(inputId = "num_schedule_frequency", 
                             label = "Frequency", 
                             value = 0), 
                selectInput(inputId = "sel_schedule_frequencyUnit", 
                            label = "Unit of Time", 
                            choices = UNIT_OF_TIME, 
                            selected = "Day"), 
                cellWidths = c("15%", "50%")), 
    splitLayout(numericInput(inputId = "num_schedule_offset", 
                             label = "Offset", 
                             value = 0), 
                selectInput(inputId = "sel_schedule_offsetUnit", 
                            label = "Unit of Time", 
                            choices = UNIT_OF_TIME, 
                            selected = "Day"), 
                cellWidths = c("15%", "50%")), 
    checkboxInput(inputId = "chk_schedule_isActive", 
                  label = "Active", 
                  value = TRUE),
    actionButton(inputId = "btn_schedule_addEditSchedule", 
                 label = "Save")
  )

# Modal - Date Reporting Format -------------------------------------

MODAL_DATE_REPORT_FORMAT <- 
  bsModal(
    id = "modal_dateFormat_addEdit", 
    title = uiOutput("title_dateFormat"), 
    size = "large", 
    trigger = "trg_none", 
    textInput(inputId = "txt_dateFormat_formatName", 
              label = "Format Name"), 
    textInput(inputId = "txt_dateFormat_description", 
              label = "Description"),
    textInput(inputId = "txt_dateFormat_formatCode", 
              label = "Format Code"), 
    splitLayout(
      numericInput(inputId = "num_dateFormat_incrementStart", 
                   label = "Increment Start", 
                   value = 0), 
      selectInput(inputId = "sel_dateFormat_incrementStartUnit", 
                  label = "Unit of Time", 
                  choices = UNIT_OF_TIME, 
                  selected = "Day"), 
      cellWidths = c("15%", "50%")
    ), 
    splitLayout(
      numericInput(inputId = "num_dateFormat_incrementEnd", 
                   label = "Increment End", 
                   value = 0), 
      selectInput(inputId = "sel_dateFormat_incrementEndUnit", 
                  label = "Unit of Time", 
                  choices = UNIT_OF_TIME, 
                  selected = "Day"), 
      cellWidths = c("15%", "50%")
    ),
    checkboxInput(inputId = "chk_dateFormat_isActive", 
                  label = "Active", 
                  value = TRUE),
    actionButton(inputId = "btn_dateFormat_addEditFormat", 
                 label = "Save")
    
  )
