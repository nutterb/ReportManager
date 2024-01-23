MODAL_REPORT_TEMPLATE <- 
  bsModal(
    id = "modal_template_addEdit", 
    title = uiOutput("title_reportTemplateModal"), 
    size = "large", 
    trigger = "trg_none",
    fluidPage(
      column(
        width = 6,
        textInput(inputId = "txt_template_title", 
                  label = "Title"), 
        selectInput(inputId = "sel_template_directory", 
                    label = "Template Directory", 
                    choices = TEMPLATE_FOLDERS), 
        selectInput(inputId = "sel_template_file", 
                    label = "Primary Template File", 
                    choices = character(0)), 
        checkboxInput(inputId = "chk_template_includeTableOfContents", 
                      label = "Include Table of Contents"),
        checkboxInput(inputId = "chk_template_isSignatureRequired", 
                      label = "Signature Required"), 
        checkboxInput(inputId = "chk_template_isActive", 
                      label = "Active", 
                      value = TRUE), 
        actionButton(inputId = "btn_template_addEdit", 
                     label = "Save")
      ), 
      column(
        width = 6, 
        textAreaInput(inputId = "txt_template_defaultEmailText", 
                      label = "Default E-mail Text", 
                      height = 200, 
                      width = 600),
        selectInput(inputId = "sel_template_titleSize", 
                    label = "Title Size", 
                    choices = FONT_SIZES, 
                    selected = "LARGE"), 
        selectInput(inputId = "sel_template_logo", 
                    label = "Logo", 
                    choices = character(0)), 
        imageOutput("img_template_logo_preview")
      )
    )
  )
