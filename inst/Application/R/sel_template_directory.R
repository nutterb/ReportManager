..sel_template_directory <- function(session, 
                                      input){
  req(input$sel_template_directory)
  dir <- system.file("ReportTemplate", package = "ReportManager")
  dir <- file.path(dir, input$sel_template_directory)
  files <- list.files(dir, 
                      pattern = ".Rmd$")
  updateSelectInput(session = session, 
                    inputId = "sel_template_file", 
                    choices = files, 
                    selected = files[1])
}
