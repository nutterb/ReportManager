..rdo_user <- function(rv_User, input){
  oid <- as.numeric(input$rdo_user)
  ThisUser <- rv_User$User
  ThisUser <- ThisUser[ThisUser$OID == oid, ]
  rv_User$SelectedUser <- ThisUser
}
