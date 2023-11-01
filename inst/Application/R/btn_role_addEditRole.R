..btn_role_addEditRole <- function(session, 
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
