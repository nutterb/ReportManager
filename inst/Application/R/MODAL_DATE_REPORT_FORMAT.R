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
