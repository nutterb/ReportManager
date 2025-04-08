..btn_user_addEditUser <- function(session, 
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
    print(input$file_user_signature)
    addEditUser(oid = oid,
                last_name = trimws(input$txt_user_lastName),
                first_name = trimws(input$txt_user_firstName),
                login_id = trimws(input$txt_user_loginId),
                email = trimws(input$txt_user_emailAddress),
                is_internal = input$chk_user_isInternal,
                is_active = input$chk_user_isActive,
                event_user = current_user_oid, 
                signature_file = input$file_user_signature$datapath)
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
