#' @name queryCurrentRevisionNumber
#' @title Retrieve Current Revision Number for Active Report Submission
#' 
#' @description Returns the number of the current revision in progress. 
#'   If no submissions have been made, it starts at 0. 
#'   
#' @param report_instance_oid `integerish(1)`. The OID of the report instance.
#' 
#' @export

queryCurrentRevisionNumber <- function(report_instance_oid){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- .queryCurrentRevisionNumber_statement()
  
  Rev <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        report_instance_oid = report_instance_oid
      )
    )
  
  Rev$RevisionNumber
}

.queryCurrentRevisionNumber_statement <- function(flavor = getOption("RM_sql_flavor")){
  
  schema <- if(flavor == "sql_server") "dbo." else ""
  
  sprintf("SELECT COUNT(IsSubmission) AS RevisionNumber
          FROM %sReportInstanceGeneration
          WHERE ParentReportInstance = ?report_instance_oid
            AND IsSubmission = 1", 
          schema)
}
