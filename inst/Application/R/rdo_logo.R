..rdo_logo <- function(rv_Logo, input){
  oid <- as.numeric(input$rdo_logo)
  rv_Logo$SelectedLogo <- 
    rv_Logo$Logo[rv_Logo$Logo$OID == oid, ]
}
