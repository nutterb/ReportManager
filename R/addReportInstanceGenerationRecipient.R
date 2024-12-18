#' @name addReportInstanceGenerationRecipient
#' @title Record Recipients to Whom a Report was Sent
#' 
#' @description Makes a record tying a recipient/user to an attempt to deliver
#'   a report to that user.
#'   
#' @param report_instance_generation_oid `integerish(1)`. The OID of the report 
#'   instance generation.
#' @param user_oid `integerish`. The OIDs of the users receiving the 
#'   report instance.
#'
#' @export

addReportInstanceGenerationRecipient <- function(report_instance_generation_oid, 
                                                 user_oid){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_generation_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = user_oid, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  AddData <- data.frame(ParentReportInstanceGeneration = rep(report_instance_generation_oid, 
                                                             length(user_oid)),
                        ParentUser = user_oid)
  
  insertRecord(AddData, 
               table_name = "ReportInstanceGenerationRecipient")
}
