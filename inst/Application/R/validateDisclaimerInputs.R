validateDisclaimerInputs <- function(rv_Disclaimer, 
                                     input){
  disclaimer <- trimws(input$txt_disclaimer_text)
  val <- inputValidationCollection()
  if (disclaimer == ""){
    val$invalidate("Disclaimer is empty or all whitespace.")
  }
  if (nchar(disclaimer) > 2000){
    val$invalidate("Disclaimer cannot exceed 2,000 characters.")
  }
  val
}
