# Event Observer - rdo_user -----------------------------------------

OE_rdo_user <- function(rv_User, input){
  oid <- as.numeric(input$rdo_user)
  ThisUser <- rv_User$User
  ThisUser <- ThisUser[ThisUser$OID == oid, ]
  
  rv_User$SelectedUser <- ThisUser
}

# Event Observer - btn_user_add -------------------------------

OE_btn_user_add <- function(session, rv_User, input){
  rv_User$AddEdit <- "Add"
  lapply(c("txt_user_lastName", 
           "txt_user_firstName", 
           "txt_user_loginId", 
           "txt_user_emailAddress"), 
         function(ctrl) updateTextInput(session = session, 
                                        inputId = ctrl, 
                                        value = ""))
  
  updateCheckboxInput(session = session, 
                      inputId = "chk_user_isInternal", 
                      value = FALSE)
  updateCheckboxInput(session = session, 
                      inputId = "chk_user_isActive", 
                      value = TRUE)
  
  toggleModal(session = session, 
              modalId = "modal_user_addEdit", 
              toggle = "open")
}

# Event Observer - btn_user_edit ------------------------------

OE_btn_user_edit <- function(session, rv_User, input){
  rv_User$AddEdit <- "Edit"
  
  updateTextInput(session = session, 
                  inputId = "txt_user_lastName", 
                  value = rv_User$SelectedUser$LastName)
  updateTextInput(session = session, 
                  inputId = "txt_user_firstName", 
                  value = rv_User$SelectedUser$FirstName)
  updateTextInput(session = session, 
                  inputId = "txt_user_loginId", 
                  value = rv_User$SelectedUser$LoginId)
  updateTextInput(session = session, 
                  inputId = "txt_user_emailAddress", 
                  value = rv_User$SelectedUser$EmailAddress)
  
  updateCheckboxInput(session = session, 
                      inputId = "chk_user_isInternal", 
                      value = rv_User$SelectedUser$IsInternal)
  updateCheckboxInput(session = session, 
                      inputId = "chk_user_isActive", 
                      value = rv_User$SelectedUser$IsActive)
  
  toggleModal(session = session, 
              modalId = "modal_user_addEdit", 
              toggle = "open")
}

# Event Observer - btn_user_addEditUser -----------------

OE_btn_user_addEditUser <- function(session, 
                                                rv_User, 
                                                input, 
                                                current_user_oid, 
                                                proxy, 
                                                is_edit = FALSE, 
                                                this_login_id){
  oid <- if (rv_User$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_user)

  val <- validateUserInputs(rv_User, 
                                  input, 
                                  is_edit = is_edit, 
                                  this_login_id = this_login_id)

  if (!val$is_ok()) {
    alert(val$report())
  } else {
    addEditUser(oid = oid,
                      last_name = trimws(input$txt_user_lastName),
                      first_name = trimws(input$txt_user_firstName),
                      login_id = trimws(input$txt_user_loginId),
                      email = trimws(input$txt_user_emailAddress),
                      is_internal = input$chk_user_isInternal,
                      is_active = input$chk_user_isActive,
                      event_user = current_user_oid)
  
    RM_replaceData(query_fun = queryUser, 
                   reactive_list = rv_User, 
                   data_slot = "User", 
                   selected_slot = "SelectedUser", 
                   id_variable = "OID", 
                   element_name = "rdo_user", 
                   oid = oid, 
                   proxy = proxy)
    
    toggleModal(session = session, 
                modalId = "modal_user_addEdit", 
                toggle = "close")
  }
}


# Event Observer - btn_user_activate/deactivate ---------------

OE_btn_user_activate <- function(active, 
                                       rv_User, 
                                       input, 
                                       current_user_oid, 
                                       proxy){
  oid <- as.numeric(input$rdo_user)
  
  activateRecord(oid, 
                 active, 
                 current_user_oid, 
                 table_name = "User", 
                 event_table_name = "UserEvent", 
                 parent_field_name = "ParentUser")
  
  RM_replaceData(query_fun = queryUser, 
                 reactive_list = rv_User, 
                 data_slot = "User", 
                 selected_slot = "SelectedUser", 
                 id_variable = "OID", 
                 element_name = "rdo_user", 
                 oid = oid, 
                 proxy = proxy)
}

# Function - validateUserInputs -------------------------------

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
    if (login_id %in% rv_User$User$LoginId){
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

