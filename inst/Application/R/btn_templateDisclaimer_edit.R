..btn_templateDisclaimer_edit <- function(rv_Template, 
                                          rv_Disclaimer,
                                          session){
  Selected <- rv_Template$SelectedTemplateDisclaimer
  Selected <- Selected[order(Selected$Order), ]
  Selected <- Selected[Selected$IsActive, ]
  
  replaceMultiSelect(session = session, 
                     inputId = "templateDisclaimer", 
                     choices = as.character(rv_Disclaimer$Disclaimer$OID), 
                     selected = as.character(Selected$OID), 
                     names = rv_Disclaimer$Disclaimer$Disclaimer)
  
  toggleModal(session = session, 
              modalId = "modal_templateDisclaimer_edit", 
              toggle = "open")
}
