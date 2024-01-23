..rdo_role <- function(rv_Roles, input){
  oid <- as.numeric(input$rdo_role)
  ThisRole <- rv_Roles$Roles
  ThisRole <- ThisRole[ThisRole$OID == oid, ]
  rv_Roles$SelectedRole <- ThisRole
  rv_Roles$UserRole <- queryUserRole(role_oid = oid)
}
