..rdo_dateFormat <- function(rv_DateFormat, input){
  oid <- as.numeric(input$rdo_dateFormat)
  rv_DateFormat$SelectedDateFormat <-  
    rv_DateFormat$DateFormat[rv_DateFormat$DateFormat$OID == oid, ]
}
