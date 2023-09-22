# UI Elements on the Roles Page -------------------------------------

UI_TAB_ROLES <- 
  tagList(
    disabled(actionButton(inputId = "btn_role_add", 
                          label = "Add Role", 
                          style = "float:right")),
    disabled(actionButton(inputId = "btn_role_edit", 
                          label = "Edit Role", 
                          style = "float:right")),
    disabled(actionButton(inputId = "btn_role_activate", 
                          label = "Activate Role", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_role_deactivate", 
                          label = "Deactivate Role", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_role_viewEdit", 
                          label = "View/Edit Assignments", 
                          style = "float:right")), 
    radioDataTableOutput(outputId = "dt_role", 
                         radioId = "rdo_role")
  )

# Modal - role_addEdit ----------------------------------------

MODAL_ROLES <- bsModal(
  id = "modal_role_addEdit", 
  title = textOutput("title_addEditRole"), 
  size = "large", 
  trigger = "trg_none", 
  textInput(inputId = "txt_role_roleName", 
            label = "Role Name"), 
  textAreaInput(inputId = "txt_role_roleDescription", 
                label = "Role Description", 
                width = "400px", 
                height = "80px"), 
  checkboxInput(inputId = "chk_role_isActive", 
                label = "Is Active", 
                value = TRUE), 
  actionButton(inputId = "btn_role_addEditRole", 
               label = "Save")
)
