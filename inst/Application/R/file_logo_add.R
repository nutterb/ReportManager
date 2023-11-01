..file_logo_add <- function(session, 
                             input){
  updateTextInput(session = session, 
                  inputId = "txt_logo_add_fileName", 
                  value = tools::file_path_sans_ext(basename(input$file_logo_add$name[1])))
  updateTextInput(session = session, 
                  inputId = "txt_logo_add_extension", 
                  value = tools::file_ext(input$file_logo_add$datapath[1]))
}
