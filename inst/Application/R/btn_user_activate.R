..btn_user_activate <- function(active, 
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
