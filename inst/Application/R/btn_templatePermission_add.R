..btn_templatePermission_add <- function(rdo_template, 
                                         rv_Template, 
                                         rv_Roles,
                                         session){
  rv_Template$TemplatePermissionAddEdit <- "Add"
  
  CurrentRole <- 
    queryReportTemplatePermission(parent_report_template = 
                                    as.numeric(rdo_template))
  CurrentRole <- CurrentRole[CurrentRole$IsActive, ]
  
  role <- rv_Roles$Roles$OID
  names(role) <- rv_Roles$Roles$RoleName
  role <- role[!role %in% CurrentRole$ParentRole]
  updateSelectInput(session = session, 
                    inputId = "sel_templatePermissionRole", 
                    choices = role)
  
  enable("sel_templatePermissionRole")
  
  updateCheckboxGroupInput(session = session, 
                           inputId = "chkgrp_templatePermission",
                           choices = c("View" = "CanView",
                                       "Add Notes" = "CanAddNotes",
                                       "Edit Narrative" = "CanEditNarrative",
                                       "Submit" = "CanSubmit",
                                       "Start Revision" = "CanStartRevision"),
                           selected = "CanView")
  
  toggleModal(session = session, 
              modalId = "modal_templatePermission_addEdit", 
              toggle = "open")
}

..btn_templatePermission_edit <- function(rv_Template, 
                                          rv_Roles, 
                                          session){
  rv_Template$TemplatePermissionAddEdit <- "Edit"
  
  CurrentPermission <- queryReportTemplatePermission(oid = rv_Template$SelectedTemplatePermission)
  CurrentRole <- rv_Roles$Roles
  CurrentRole <- CurrentRole[CurrentRole$OID == CurrentPermission$ParentRole, ]
  
  role <- CurrentRole$OID
  names(role) <- CurrentRole$RoleName
  
  updateSelectInput(session = session, 
                    inputId = "sel_templatePermissionRole", 
                    choices = role, 
                    selected = role)
  disable("sel_templatePermissionRole")
  
  permission <- names(CurrentPermission)[vapply(CurrentPermission, 
                                                is.logical, 
                                                logical(1))]
  permission <- permission[!permission %in% "IsActive"]
  permission <- permission[unlist(CurrentPermission[permission])]
  
  updateCheckboxGroupInput(session = session, 
                           inputId = "chkgrp_templatePermission",
                           choices = c("View" = "CanView",
                                       "Add Notes" = "CanAddNotes",
                                       "Edit Narrative" = "CanEditNarrative",
                                       "Submit" = "CanSubmit",
                                       "Start Revision" = "CanStartRevision"),
                           selected = permission)
  
  toggleModal(session = session, 
              modalId = "modal_templatePermission_addEdit", 
              toggle = "open")
}

..btn_saveTemplatePermission <- function(rdo_template,
                                         rdo_templatePermission, 
                                         sel_templatePermissionRole, 
                                         chkgrp_templatePermission,
                                         rv_Template, 
                                         proxy_dt_templatePermission, 
                                         current_user_oid,
                                         session){
  oid <- if (rv_Template$TemplatePermissionAddEdit == "Add") numeric(0) else as.numeric(rdo_templatePermission)
  
  addEditReportTemplatePermission(oid = oid, 
                                  parent_report_template = rv_Template$SelectedTemplate$OID, 
                                  parent_role = as.numeric(sel_templatePermissionRole),
                                  can_view = "CanView" %in% chkgrp_templatePermission, 
                                  can_add_notes = "CanAddNotes" %in% chkgrp_templatePermission, 
                                  can_edit_narrative = "CanEditNarrative"  %in% chkgrp_templatePermission, 
                                  can_submit = "CanSubmit" %in% chkgrp_templatePermission,
                                  can_start_revision = "CanStartRevision" %in% chkgrp_templatePermission,
                                  is_active = TRUE, 
                                  event_user = current_user_oid)
  
  
  replaceData(proxy = proxy_dt_templatePermission, 
              data = makeTemplatePermissionData(as.numeric(rdo_template)) %>% 
                radioDataTable(id_variable = "OID", 
                               element_name = "rdo_templatePermission", 
                               checked = as.numeric(rdo_templatePermission)),
              resetPaging = FALSE,
              rownames = FALSE)
  
  toggleModal(session = session, 
              modalId = "modal_templatePermission_addEdit", 
              toggle = "close")
}