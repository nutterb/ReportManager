validateUserInputs <- function(rv_User, 
                                     input, 
                                     is_edit = FALSE, 
                                     this_login_id){
  val <- inputValidationCollection()
  last_name <- trimws(input$txt_user_lastName)
  first_name <- trimws(input$txt_user_firstName)
  login_id <- trimws(input$txt_user_loginId)
  email <- trimws(input$txt_user_emailAddress)
  duplicate_login_message <- 
    sprintf("Login ID '%s' already exists in the database. Duplicates are not allowed.", 
            login_id)
  if (is_edit){
    if ((this_login_id != login_id) &&
        login_id %in% rv_User$User$LoginId){
      val$invalidate(duplicate_login_message)
    } 
  } else {
    if (isTRUE(login_id %in% rv_User$User$LoginId)){
      val$invalidate(duplicate_login_message)
    }
  }
  if (last_name == ""){
    val$invalidate("Last Name is empty or only whitespace.")
  }
  if (first_name == ""){
    val$invalidate("First Name is empty or only whitespace.")
  }
  if (login_id == ""){
    val$invalidate("Login ID is empty or only whitespace.")
  }
  if (email == ""){
    val$invalidate("E-mail Address is empty or only whitespace.")
  }
  val
}
