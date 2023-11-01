..btn_role_activateDeactivate <- function(active, 
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
