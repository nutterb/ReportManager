OE_rdo_reportUser <- function(rv_ReportUser, input){
  oid <- as.numeric(input$rdo_reportUser)
  ThisUser <- rv_ReportUser$ReportUser
  ThisUser <- ThisUser[ThisUser$OID == oid, ]
  
  rv_ReportUser$SelectedReportUser <- ThisUser
}