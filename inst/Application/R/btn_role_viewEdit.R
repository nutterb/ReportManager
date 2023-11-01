..btn_role_viewEdit <- function(rv_User, 
                                 rv_Roles, 
                                 session){
  choices <- as.character(rv_User$User$OID)
  selected <- choices[choices %in% rv_Roles$UserRole$ParentUser]
  replaceMultiSelect(session = session, 
                     inputId = "multi_userRole", 
                     choices = choices, 
                     selected = selected, 
                     names = sprintf("%s, %s (%s)", 
                                     rv_User$User$LastName, 
                                     rv_User$User$FirstName, 
                                     rv_User$User$LoginId))
  toggleModal(session = session, 
              modalId = "modal_userRole_edit", 
              toggle = "open")
}
