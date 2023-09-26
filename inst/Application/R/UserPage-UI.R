# UI Elements in the Users Tab --------------------------------------

UI_TAB_REPORT_USER <- 
  tagList(
    disabled(actionButton(inputId = "btn_user_add", 
                          label = "Add User", 
                          style = "float:right")),
    disabled(actionButton(inputId = "btn_user_edit", 
                          label = "Edit User", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_user_activate", 
                          label = "Activate User", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_user_deactivate", 
                          label = "Deactivate User", 
                          style = "float:right")), 
    radioDataTableOutput(outputId = "dt_user", 
                         radioId = "rdo_user")
  )

# Modal - user_addEdit ----------------------------------------

MODAL_USER <- bsModal(
  id = "modal_user_addEdit", 
  title = textOutput("title_addEditUser"), 
  size = "large", 
  trigger = "trg_none", 
  textInput(inputId = "txt_user_lastName", 
            label = "Last Name"), 
  textInput(inputId = "txt_user_firstName", 
            label = "First Name"), 
  textInput(inputId = "txt_user_loginId", 
            label = "Login ID"), 
  textInput(inputId = "txt_user_emailAddress", 
            label = "E-mail Address"), 
  checkboxInput(inputId = "chk_user_isInternal", 
                label = "Is Internal User", 
                value = FALSE), 
  checkboxInput(inputId = "chk_user_isActive", 
                label = "Is Active", 
                value = TRUE), 
  actionButton(inputId = "btn_user_addEditUser", 
               label = "Save")
)
