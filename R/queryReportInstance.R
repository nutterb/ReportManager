#' @name queryReportInstance
#' @title Retrieve Report Instances from the Database
#' 
#' @description Executes a query to the database and retrieves the report
#'   instances associated with the report template provided. May also 
#'   return the instance data for a specific instance.
#'   
#' @param report_template_oid `integerish(0/1)`. The OID of the report 
#'   template for which instances are returned. If length 0, instances for 
#'   all report templates are returned.
#' @param report_instance_oid `integerish(0/1)`. The OID of the report 
#'   instance to be queried. If length 0, all instances are returned. 
#'   Note: if both `report_template_oid` and `report_instance_oid` are 
#'   provided and the instance OID does not belong to the template OID, 
#'   no results will be returned.
#' 
#' @return Returns a data frame of the instance details for a report template.
#' 
#' @export

queryReportInstance <- function(report_template_oid = numeric(0), 
                                report_instance_oid = numeric(0)){
  
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_template_oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .queryReportInstance_sqlite, 
                      "sql_server" = .queryReportInstance_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  if (length(report_template_oid) > 0){
    statement <- paste0(statement, " AND [ParentReportTemplate] = ?")
  }
  
  if (length(report_instance_oid) > 0){
    statement <- paste0(statement, " AND [OID] = ?")
  }
  
  param_list <- list(report_template_oid, 
                     report_instance_oid)
  param_list <- param_list[lengths(param_list) > 0]
  
  ReportInstance <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    ReportInstance$IsSignatureRequired <- as.logical(ReportInstance$IsSignatureRequired)
    ReportInstance$IsScheduled <- as.logical(ReportInstance$IsScheduled)
    ReportInstance$IsSubmitted <- as.logical(ReportInstance$IsSubmitted)
  }
  
  ReportInstance
}

# Unexported --------------------------------------------------------

.queryReportInstance_sqlite <- "
  SELECT [OID],
    [ParentReportTemplate], 
    [StartDateTime],
    [EndDateTime],
    [IsSignatureRequired],
    [IsScheduled],
    [InstanceTitle],
    [IsSubmitted]
  FROM ReportInstance
  WHERE 1=1
"

.queryReportInstance_sqlServer <- "
  SELECT [OID],
    [ParentReportTemplate], 
    [StartDateTime],
    [EndDateTime],
    [IsSignatureRequired],
    [IsScheduled],
    [InstanceTitle],
    [IsSubmitted]
  FROM dbo.ReportInstance
  WHERE 1=1
"
