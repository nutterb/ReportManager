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

# Modal - modal_userRole_edit ---------------------------------------

MODAL_USER_ROLE <-
  bsModal(
    id = "modal_userRole_edit", 
    title = textOutput("title_userRole_edit"), 
    size = "large", 
    trigger = "trg_none", 
    multiSelect(inputId = "multi_userRole", 
                label = "User Role Assignments", 
                choices = character(0), 
                selected = character(0), 
                size = 15), 
    br(),
    actionButton(inputId = "btn_userRole_save", 
                 label = "Save")
  )
