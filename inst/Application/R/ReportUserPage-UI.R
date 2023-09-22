# UI Elements in the Users Tab --------------------------------------

UI_TAB_REPORT_USER <- 
  tagList(
    disabled(actionButton(inputId = "btn_reportUser_add", 
                          label = "Add User", 
                          style = "float:right")),
    disabled(actionButton(inputId = "btn_reportUser_edit", 
                          label = "Edit User", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_reportUser_activate", 
                          label = "Activate User", 
                          style = "float:right")), 
    disabled(actionButton(inputId = "btn_reportUser_deactivate", 
                          label = "Deactivate User", 
                          style = "float:right")), 
    radioDataTableOutput(outputId = "dt_reportUser", 
                         radioId = "rdo_reportUser")
  )

# Modal - reportUser_addEdit ----------------------------------------

MODAL_REPORT_USER <- bsModal(
  id = "modal_reportUser_addEdit", 
  title = textOutput("title_addEditReportUser"), 
  size = "large", 
  trigger = "trg_none", 
  textInput(inputId = "txt_reportUser_lastName", 
            label = "Last Name"), 
  textInput(inputId = "txt_reportUser_firstName", 
            label = "First Name"), 
  textInput(inputId = "txt_reportUser_loginId", 
            label = "Login ID"), 
  textInput(inputId = "txt_reportUser_emailAddress", 
            label = "E-mail Address"), 
  checkboxInput(inputId = "chk_reportUser_isInternal", 
                label = "Is Internal User", 
                value = FALSE), 
  checkboxInput(inputId = "chk_reportUser_isActive", 
                label = "Is Active", 
                value = TRUE), 
  actionButton(inputId = "btn_reportUser_addEditReportUser", 
               label = "Save")
)
