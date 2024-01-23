MODAL_LOGO_ADD <- 
  bsModal(
    id = "modal_logo_addEdit", 
    title = "Add a New Logo File", 
    size = "large", 
    trigger = "trg_none", 
    fluidRow(
      column(width = 6,
             fileInput(inputId = "file_logo_add", 
                       label = "Select a File"), 
             textInput(inputId = "txt_logo_add_fileName", 
                       label = "File Name"), 
             textInput(inputId = "txt_logo_add_description", 
                       label = "Description"),
             disabled(textInput(inputId = "txt_logo_add_extension", 
                                label = "File Extension")), 
             actionButton(inputId = "btn_logo_addEdit", 
                          label = "Save")
      ),
      column(width = 6, 
             imageOutput("img_logo_preview")
      )
    )
  )
