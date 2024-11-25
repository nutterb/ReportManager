..btn_templateDistribution_edit <- function(rv_Template, 
                                            rv_User, 
                                            session){
  Selected <- rv_Template$SelectedTemplateDistribution
  SelectedUser <- Selected[!is.na(Selected$ParentUser), ]
  SelectedUser <- SelectedUser[SelectedUser$IsActive, ]
  
  SelectedRole <- Selected[!is.na(Selected$ParentRole), ]
  SelectedRole <- SelectedRole[SelectedRole$IsActive, ]
  
  
  replaceMultiSelect(session = session,
                     inputId = "templateDistributionUser",
                     choices = as.character(rv_User$User$OID),
                     selected = as.character(Selected$ParentUser),
                     names = sprintf("%s, %s (%s)", 
                                     rv_User$User$LastName, 
                                     rv_User$User$FirstName, 
                                     rv_User$User$LoginId))
  
  replaceMultiSelect(session = session,
                     inputId = "templateDistributionRole",
                     choices = as.character(rv_Roles$Roles$OID),
                     selected = as.character(Selected$ParentUser),
                     names = rv_Roles$Roles$RoleName)
  
  toggleModal(session = session, 
              modalId = "modal_templateDistribution_edit", 
              toggle = "open")
}