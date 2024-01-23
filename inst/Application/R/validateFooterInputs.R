validateFooterInputs <- function(rv_Footer, 
                                 input){
  footer <- trimws(input$txt_footer_text)
  val <- inputValidationCollection()
  if (footer == ""){
    val$invalidate("Footer is empty or all whitespace.")
  }
  if (nchar(footer) > 2000){
    val$invalidate("Footer cannot exceed 2,000 characters.")
  }
  val
}
