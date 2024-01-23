validateRoleInputs <- function(rv_Roles, 
                               input, 
                               is_edit = FALSE, 
                               this_role_name){
  val <- inputValidationCollection()
  role_name <- trimws(input$txt_role_roleName)
  role_description <- trimws(input$txt_role_roleDescription)
  duplicate_role_name_message <- 
    sprintf("Role Name '%s' already exists in the database. Duplicates are not allowed.", 
            role_name)
  if (is_edit){
    if ((this_role_name != role_name) &&
        role_name %in% rv_Roles$Roles$RoleName){
      val$invalidate(duplicate_role_name_message)
    } 
  } else {
    if (role_name %in% rv_Roles$Roles$RoleName){
      val$invalidate(duplicate_role_name_message)
    }
  }
  if (role_name == ""){
    val$invalidate("Role Name is empty or only whitespace.")
  }
  val
}
