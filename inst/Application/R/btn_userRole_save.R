..btn_userRole_save <- function(input, current_user_oid){
  UR_current <- queryUserRole(role_oid = as.numeric(input$rdo_role))
  UR_ui <- jsonlite::fromJSON(input$multi_userRole)
  UR_upload <- merge(UR_current[c("OID", "ParentUser", "ParentRole")], 
                     UR_ui[c("choices", "selected")], 
                     by.x = "ParentUser", 
                     by.y = "choices",
                     all.x = TRUE, 
                     all.y = TRUE)
  UR_upload <- UR_upload[c("OID", "ParentUser", "ParentRole", "selected")]
  UR_upload$selected <- UR_upload$selected %in% TRUE
  UR_upload$ParentRole <- ifelse(is.na(UR_upload$ParentRole), 
                                 as.numeric(input$rdo_role), 
                                 UR_upload$ParentRole)
  UR_upload <- UR_upload[!(is.na(UR_upload$OID) & !UR_upload$selected), ]
  for (i in seq_len(nrow(UR_upload))){
    addEditUserRole(oid = if (is.na(UR_upload$OID[i])) numeric(0) else as.numeric(UR_upload$OID[i]), 
                    parent_user = as.numeric(UR_upload$ParentUser[i]), 
                    parent_role = as.numeric(UR_upload$ParentRole[i]), 
                    is_active = UR_upload$selected[i], 
                    event_user = current_user_oid)
  }
}
