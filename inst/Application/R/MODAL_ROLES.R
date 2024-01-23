MODAL_ROLES <- 
  bsModal(
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
