# Event Observer - rdo_reportUser -----------------------------------

OE_rdo_reportUser <- function(rv_ReportUser, input){
  oid <- as.numeric(input$rdo_reportUser)
  ThisUser <- rv_ReportUser$ReportUser
  ThisUser <- ThisUser[ThisUser$OID == oid, ]
  
  rv_ReportUser$SelectedReportUser <- ThisUser
}

# Event Observer - btn_reportUser_add -------------------------------

OE_btn_reportUser_add <- function(session, rv_ReportUser, input){
  rv_ReportUser$AddEdit <- "Add"
  lapply(c("txt_reportUser_lastName", 
           "txt_reportUser_firstName", 
           "txt_reportUser_loginId", 
           "txt_reportUser_emailAddress"), 
         function(ctrl) updateTextInput(session = session, 
                                        inputId = ctrl, 
                                        value = ""))
  
  updateCheckboxInput(session = session, 
                      inputId = "chk_reportUser_isInternal", 
                      value = FALSE)
  updateCheckboxInput(session = session, 
                      inputId = "chk_reportUser_isActive", 
                      value = TRUE)
  
  toggleModal(session = session, 
              modalId = "modal_reportUser_addEdit", 
              toggle = "open")
}

# Event Observer - btn_reportUser_edit ------------------------------

OE_btn_reportUser_edit <- function(session, rv_ReportUser, input){
  rv_ReportUser$AddEdit <- "Edit"
  
  updateTextInput(session = session, 
                  inputId = "txt_reportUser_lastName", 
                  value = rv_ReportUser$SelectedReportUser$LastName)
  updateTextInput(session = session, 
                  inputId = "txt_reportUser_firstName", 
                  value = rv_ReportUser$SelectedReportUser$FirstName)
  updateTextInput(session = session, 
                  inputId = "txt_reportUser_loginId", 
                  value = rv_ReportUser$SelectedReportUser$LoginId)
  updateTextInput(session = session, 
                  inputId = "txt_reportUser_emailAddress", 
                  value = rv_ReportUser$SelectedReportUser$EmailAddress)
  
  updateCheckboxInput(session = session, 
                      inputId = "chk_reportUser_isInternal", 
                      value = rv_ReportUser$SelectedReportUser$IsInternal)
  updateCheckboxInput(session = session, 
                      inputId = "chk_reportUser_isActive", 
                      value = rv_ReportUser$SelectedReportUser$IsActive)
  
  toggleModal(session = session, 
              modalId = "modal_reportUser_addEdit", 
              toggle = "open")
}

# Event Observer - btn_reportUser_addEditReportUser -----------------

OE_btn_reportUser_addEditReportUser <- function(session, 
                                                rv_ReportUser, 
                                                input, 
                                                current_user_oid, 
                                                proxy, 
                                                is_edit = FALSE, 
                                                this_login_id){
  oid <- if (rv_ReportUser$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_reportUser)

  val <- validateReportUserInputs(rv_ReportUser, 
                                  input, 
                                  is_edit = is_edit, 
                                  this_login_id = this_login_id)

  if (!val$is_ok()) {
    alert(val$report())
  } else {
    addEditReportUser(oid = oid,
                      last_name = trimws(input$txt_reportUser_lastName),
                      first_name = trimws(input$txt_reportUser_firstName),
                      login_id = trimws(input$txt_reportUser_loginId),
                      email = trimws(input$txt_reportUser_emailAddress),
                      is_internal = input$chk_reportUser_isInternal,
                      is_active = input$chk_reportUser_isActive,
                      event_user = current_user_oid)
  
    RM_replaceData(query_fun = queryReportUser, 
                   reactive_list = rv_ReportUser, 
                   data_slot = "ReportUser", 
                   selected_slot = "SelectedReportUser", 
                   id_variable = "OID", 
                   element_name = "rdo_reportUser", 
                   oid = oid, 
                   proxy = proxy)
    
    toggleModal(session = session, 
                modalId = "modal_reportUser_addEdit", 
                toggle = "close")
  }
}


# Event Observer - btn_reportUser_activate/deactivate ---------------

OE_btn_reportUser_activate <- function(active, 
                                       rv_ReportUser, 
                                       input, 
                                       current_user_oid, 
                                       proxy){
  oid <- as.numeric(input$rdo_reportUser)
  
  activateRecord(oid, 
                 active, 
                 current_user_oid, 
                 table_name = "ReportUser", 
                 event_table_name = "ReportUserEvent", 
                 parent_field_name = "ParentReportUser")
  
  RM_replaceData(query_fun = queryReportUser, 
                 reactive_list = rv_ReportUser, 
                 data_slot = "ReportUser", 
                 selected_slot = "SelectedReportUser", 
                 id_variable = "OID", 
                 element_name = "rdo_reportUser", 
                 oid = oid, 
                 proxy = proxy)
}

# Function - validateReportUserInputs -------------------------------

validateReportUserInputs <- function(rv_ReportUser, 
                                     input, 
                                     is_edit = FALSE, 
                                     this_login_id){
  val <- inputValidationCollection()
  
  last_name <- trimws(input$txt_reportUser_lastName)
  first_name <- trimws(input$txt_reportUser_firstName)
  login_id <- trimws(input$txt_reportUser_loginId)
  email <- trimws(input$txt_reportUser_emailAddress)
  
  duplicate_login_message <- 
    sprintf("Login ID '%s' already exists in the database. Duplicates are not allowed.", 
            login_id)
  
  if (is_edit){
    if ((this_login_id != login_id) &&
        login_id %in% rv_ReportUser$ReportUser$LoginId){
      val$invalidate(duplicate_login_message)
    } 
  } else {
    if (login_id %in% rv_ReportUser$ReportUser$LoginId){
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

