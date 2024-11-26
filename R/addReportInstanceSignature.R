#' @name addReportInstanceSignature
#' @title Add a Signature Event to a Report Instance
#' 
#' @description Provides the user with the tools to add or remove signatures 
#'   from a report instance.
#'   
#' @param report_instance_oid `integerish(1)`. The OID of the report instance
#'   being signed (or unsigned).
#' @param signed `logical(1)`. If `TRUE`, the report will be marked as signed.
#'   If `FALSE`, the signature is reversed.
#' @param event_user `integerish(1)`. The OID of the user signing the report.
#' 
#' @export

addReportInstanceSignature <- function(report_instance_oid,
                                       report_template_signature,
                                       signed, 
                                       event_user, 
                                       signed_datetime = Sys.time()){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = report_template_signature, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = signed, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertPOSIXct(x = signed_datetime, 
                           len = 1, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .addReportInstanceSignature_sqlite, 
                      "sql_server" = .addReportInstanceSignature_sqlServer, 
                      stop(sprintf("Query not defined for SQL flavor '%s'",
                                   getOption("RM_sql_flavor"))))
  
  User <- queryUser(oid = event_user)
  
  result <- 
    DBI::dbSendStatement(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        report_instance_oid = report_instance_oid, 
        report_template_signature = report_template_signature, 
        event_user = event_user, 
        signed_datetime = format(signed_datetime, 
                                 format = "%Y-%m-%d %H:%M:%S"), 
        signed_name = sprintf("%s %s", 
                              User$FirstName,
                              User$LastName), 
        signed = as.numeric(signed)
      )
    )
  
  DBI::dbClearResult(result)
}

# Unexported --------------------------------------------------------

.addReportInstanceSignature_sqlite <- "
INSERT INTO ReportInstanceSignature
(ParentReportInstance, ParentReportTemplateSignature, ParentUser, 
 SignatureDateTime, SignatureName, Signed)
VALUES
(?report_instance_oid, ?report_template_signature, ?event_user, 
 ?signed_datetime, ?signed_name, ?signed)
"

.addReportInstanceSignature_sqlServer <- "
INSERT INTO dbo.ReportInstanceSignature
(ParentReportInstance, ParentReportTemplateSignature, ParentUser, 
 SignatureDateTime, SignatureName, Signed)
VALUES
(?report_instance_oid, ?report_template_signature, ?event_user, 
 ?signed_datetime, ?signed_name, ?signed)
"