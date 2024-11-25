..btn_templateFooter_edit <- function(rv_Template, 
                                      rv_Footer, 
                                      session){
  Selected <- rv_Template$SelectedTemplateFooter
  Selected <- Selected[order(Selected$Order), ]
  Selected <- Selected[Selected$IsActive, ]
  
  replaceMultiSelect(session = session, 
                     inputId = "templateFooter", 
                     choices = as.character(rv_Footer$Footer$OID), 
                     selected = as.character(Selected$OID), 
                     names = rv_Footer$Footer$Footer)
  
  toggleModal(session = session, 
              modalId = "modal_templateFooter_edit", 
              toggle = "open")
}
