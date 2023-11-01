..sel_template_logo <- function(input, 
                                 output){
  display <- input$sel_template_logo != ""
  if (display){
    output$img_template_logo_preview <- 
      renderImage({
        Logo <- queryFromFileArchive(oid = as.numeric(input$sel_template_logo),
                                     file_dir = tempdir())
        list(src = Logo$SavedTo,
             height = "100px",
             width = "100px")
      }, deleteFile = TRUE)
  } else {
    NULL
  }
}
