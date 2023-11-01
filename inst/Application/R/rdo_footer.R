..rdo_footer <- function(rv_Footer, input){
  oid <- as.numeric(input$rdo_footer)
  rv_Footer$SelectedFooter <- 
    rv_Footer$Footer[rv_Footer$Footer$OID == oid, ]
}
