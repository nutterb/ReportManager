MODAL_USER <- 
  bsModal(
    id = "modal_user_addEdit", 
    title = textOutput("title_addEditUser"), 
    size = "large", 
    trigger = "trg_none",
    fluidRow(
      column(
        width = 6,
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
                      value = TRUE)
      ), 
      column(
        width = 6, 
        fileInput(inputId = "file_user_signature", 
                  label = "Signature Image", 
                  accept = ".png"),
        h4("Current User Signature Image"),
        imageOutput(outputId = "img_current_signature", 
                    width = "8cm", 
                    height = "1.75cm")
      )
    ), 
    actionButton(inputId = "btn_user_addEditUser", 
                 label = "Save")
  )
