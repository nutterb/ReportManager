..btn_template_edit <- function(session, 
                                 output,
                                 rv_Template){
  rv_Template$AddEdit <- "Edit"
  updateTextInput(session = session, 
                  inputId = "txt_template_title", 
                  value = rv_Template$SelectedTemplate$Title)
  updateSelectInput(session = session, 
                    inputId = "sel_template_directory", 
                    selected = rv_Template$SelectedTemplate$TemplateDirectory)
  updateSelectInput(session = session, 
                    inputId = "sel_template_file", 
                    selected = rv_Template$SelectedTemplate$TemplateFile)
  updateCheckboxInput(session = session, 
                      inputId = "chk_template_includeTableOfContents", 
                      value = rv_Template$SelectedTemplate$IncludeTableOfContents)
  updateCheckboxInput(session = session, 
                      inputId = "chk_template_isSignatureRequired", 
                      value = rv_Template$SelectedTemplate$IsSignatureRequired)
  updateCheckboxInput(session = session, 
                      inputId = "chk_template_isActive", 
                      value = rv_Template$SelectedTemplate$IsActive)
  updateSelectInput(session = session, 
                    inputId = "sel_template_titleSize", 
                    selected = rv_Template$SelectedTemplate$TitleSize)
  updateTextInput(session = session, 
                  inputId = "txt_template_defaultEmailText", 
                  value = rv_Template$SelectedTemplate$DefaultEmailText)
  updateTextInput(session = session, 
                  inputId = "txt_template_supportingDataFile", 
                  value = rv_Template$SelectedTemplate$SupportingDataFile)
  updateSelectInput(session = session, 
                    inputId = "sel_template_logo", 
                    selected = rv_Template$SelectedTemplate$LogoFileArchive)
  if (is.na(rv_Template$SelectedTemplate$LogoFileArchive)){
    output$img_template_logo_preview <- NULL
  }
  toggleModal(session = session, 
              modalId = "modal_template_addEdit", 
              toggle = "open")
}
