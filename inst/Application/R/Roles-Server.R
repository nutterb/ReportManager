# Observe Event - rdo_role ------------------------------------------

OE_rdo_role <- function(rv_Roles, input){
  oid <- as.numeric(input$rdo_role)
  
  ThisRole <- rv_Roles$Roles
  ThisRole <- ThisRole[ThisRole$OID == oid, ]
  rv_Roles$SelectedRole <- ThisRole
}

# Observe Event - btn_role_add --------------------------------------

OE_btn_role_add <- function(session, rv_Roles){
  rv_Roles$AddEdit <- "Add"
  
  updateTextInput(session = session, 
                  inputId = "txt_role_roleName", 
                  value = "")
  updateTextInput(session = session, 
                  inputId = "txt_role_roleDescription", 
                  value = "")
  updateCheckboxInput(session = session, 
                      inputId = "chk_role_isActive", 
                      value = TRUE)
  
  toggleModal(session = session, 
              modalId = "modal_role_addEdit", 
              toggle = "open")
}

# Observe Event - btn_role_edit -------------------------------------

OE_brn_role_edit <- function(session, rv_Roles, input){
  rv_Roles$AddEdit <- "Edit"
  
  updateTextInput(session = session, 
                  inputId = "txt_role_roleName", 
                  value = rv_Roles$SelectedRole$RoleName)
  updateTextInput(session = session, 
                  inputId = "txt_role_roleDescription", 
                  value = rv_Roles$SelectedRole$RoleDescription)
  updateCheckboxInput(session = session, 
                      inputId = "chk_role_isActive", 
                      value = rv_Roles$SelectedRole$IsActive)
  
  toggleModal(session = session, 
              modalId = "modal_role_addEdit", 
              toggle = "open")
}

# Observe Event - btn_role_addEditRole ------------------------------

OE_btn_role_addEditRole <- function(session, 
                                    rv_Roles, 
                                    input, 
                                    is_edit, 
                                    this_role_name, 
                                    current_user_oid, 
                                    proxy){
  oid <- if (rv_Roles$AddEdit == "Add") numeric(0) else as.numeric(input$rdo_role)
  
  val <- validateRoleInputs(rv_Roles, 
                            input, 
                            is_edit = is_edit, 
                            this_role_name = this_role_name)
  
  if (!val$is_ok()){
    alert(val$report())
  } else {
    addEditRole(oid = oid,
                role_name = trimws(input$txt_role_roleName),
                role_description = trimws(input$txt_role_roleDescription),
                is_active = input$chk_role_isActive,
                event_user = current_user_oid)
    
    RM_replaceData(query_fun = queryRole, 
                   reactive_list = rv_Roles, 
                   data_slot = "Roles", 
                   selected_slot = "SelectedRole", 
                   id_variable = "OID", 
                   element_name = "rdo_role", 
                   oid = oid, 
                   proxy = proxy)
    
    toggleModal(session = session, 
                modalId = "modal_role_addEdit", 
                toggle = "close")
  }
}

# Observe Event - btn_role_activate/deactivate ----------------------

OE_btn_role_activateDeactivate <- function(active, 
                                           rv_Roles,
                                           input, 
                                           current_user_oid, 
                                           proxy){
  oid <- as.numeric(input$rdo_role)
  
  activateRecord(oid, 
                 active, 
                 current_user_oid, 
                 table_name = "Role", 
                 event_table_name = "RoleEvent", 
                 parent_field_name = "ParentRole")
  
  RM_replaceData(query_fun = queryRole, 
                 reactive_list = rv_Roles, 
                 data_slot = "Roles", 
                 selected_slot = "SelectedRole", 
                 id_variable = "OID", 
                 oid = oid, 
                 element_name = "rdo_role", 
                 proxy = proxy)
}

# validateRoleInputs ------------------------------------------------

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