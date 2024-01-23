MODAL_DISCLAIMER <- 
  bsModal(
    id = "modal_disclaimer_addEdit", 
    title = uiOutput("title_disclaimer_addEdit"), 
    size = "large", 
    trigger = "trg_none", 
    textAreaInput(inputId = "txt_disclaimer_text", 
                  label = "Disclaimer", 
                  width = "400px", 
                  height = "100px"),
    checkboxInput(inputId = "chk_disclaimer_isActive", 
                  label = "Active", 
                  value = TRUE),
    actionButton(inputId = "btn_disclaimer_addEditDisclaimer", 
                 label = "Save")
  )
