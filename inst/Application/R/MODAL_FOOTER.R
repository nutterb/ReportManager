MODAL_FOOTER <- 
  bsModal(
    id = "modal_footer_addEdit", 
    title = uiOutput("title_footer_addEdit"), 
    size = "large", 
    trigger = "trg_none",
    textInput(inputId = "txt_footer_text", 
              label = "Footer"),
    checkboxInput(inputId = "chk_footer_isActive", 
                  label = "Active", 
                  value = TRUE),
    actionButton(inputId = "btn_footer_addEditFooter", 
                 label = "Save")
  )  
