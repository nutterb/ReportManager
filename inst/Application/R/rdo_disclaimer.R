# Update the rv_Disclaimer$SelectedDisclaimer value 
# when the radio button rdo_disclaimer is changed

..rdo_disclaimer <- function(rv_Disclaimer, input){
  oid <- as.numeric(input$rdo_disclaimer)
  
  rv_Disclaimer$SelectedDisclaimer <- 
    rv_Disclaimer$Disclaimer[rv_Disclaimer$Disclaimer$OID == oid, ]
}
