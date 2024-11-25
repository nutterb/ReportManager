..btn_templateSignature_edit <- function(rv_Template, 
                                         rv_Roles, 
                                         session){
  Selected <- rv_Template$SelectedTemplateSignature
  Selected <- Selected[order(Selected$Order), ]
  Selected <- Selected[Selected$IsActive, ]
  
  replaceMultiSelect(session = session, 
                     inputId = "templateSignature", 
                     choices = as.character(rv_Roles$Roles$OID), 
                     selected = as.character(Selected$OID), 
                     names = rv_Roles$Roles$RoleName)
  
  toggleModal(session = session, 
              modalId = "modal_templateSignature_edit", 
              toggle = "open")
}
