#' @name queryAutoDistribution
#' @title Retrieve Auto Distribution Configuration from the Database
#'
#' @description Provides the user with tools to retrieve the configuration 
#'   settings of automated report distribution. 
#'   
#' @param oid `integerish(0/1)`. The OID of the AutoDistribution object.
#' @param report_template_oid `integerish(0/1)`. The OID of the report template.
#'   If none is provided, all records are returned. 
#'   
#' @export

queryAutoDistribution <- function(oid = numeric(0), 
                                  report_template_oid = numeric(0)){
  
  # Argument Valication ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = report_template_oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sqlite" = .queryAutoDistribution_sqlite, 
           "sql_server" = .queryAutoDistribution_sqlServer, 
           stop(sprintf("Query not defined for SQL flavor '%s'", 
                        getOption("RM_sql_flavor"))))
  
  if (length(oid) > 0){
    statement <- paste0(statement, " AND [OID] = ?")
  }
  
  if (length(report_template_oid) > 0){
    statement <- paste0(statement, " AND [ParentReportTemplate] = ?")
  }
  
  param_list <- list(oid, report_template_oid)
  param_list <- param_list[lengths(param_list) > 0]
  
  AutoDistribution <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    AutoDistribution$IsActive <- as.logical(AutoDistribution$IsActive)
    AutoDistribution$IsAddToArchive <- as.logical(AutoDistribution$IsAddToArchive)
    AutoDistribution$IsDistributeInternalOnly <- as.logical(AutoDistribution$IsDistributeInternalOnly)
    AutoDistribution$IsEmbedHtml <- as.logical(AutoDistribution$IsEmbedHtml)
  }
  
  if (!inherits(AutoDistribution$StartDateTime, "POSIXct")){
    AutoDistribution$StartDateTime <- as.POSIXct(AutoDistribution$StartDateTime, 
                                                 origin = as.POSIXct("1970-01-01 00:00:00", 
                                                                     tz = "UTC"), 
                                                 tz = "UTC")
  }
  
  AutoDistribution
}

.queryAutoDistribution_sqlite <- "
SELECT [OID], 
  [ParentReportTemplate],
  [ParentSchedule], 
  [StartDateTime], 
  [IsActive], 
  [DelayAfterInstanceEnd], 
  [DelayUnits],
  [CurrentOrLastInstance], 
  [IsAddToArchive], 
  [ReportFormat], 
  [IsDistributeInternalOnly], 
  [IsEmbedHtml]
FROM [AutoDistribution]
 WHERE 1=1
"

.queryAutoDistribution_sqlServer <- "
SELECT [OID], 
  [ParentReportTemplate],
  [ParentSchedule], 
  [StartDateTime], 
  [IsActive], 
  [DelayAfterInstanceEnd], 
  [DelayUnits],
  [CurrentOrLastInstance], 
  [IsAddToArchive], 
  [ReportFormat], 
  [IsDistributeInternalOnly], 
  [IsEmbedHtml]
FROM [dbo].[AutoDistribution]
 WHERE 1=1
"
