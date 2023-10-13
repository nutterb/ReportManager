#' @name queryReportTemplateDisclaimer
#' @title Retrieve ReportTemplateDisclaimer Objects
#' 
#' @description Provides the user with an interface to retrieve 
#'   ReportTemplateDisclaimer objects.
#'   
#' @param oid `integerish(0/1)`. When length is 1, the OID of the 
#'   ReportTemplateDisclaimer to be retrieved. Otherwise, all 
#'   records are returned.
#' @param parent_report_template `integerish(0/1)`. When length is 1, the 
#'   OID of the ReportTemplate to include in the query clause. Otherwise, 
#'   all records are returned.
#' @param parent_disclaimer `integerish(0/1)`. When length is 1, the OID
#'   of the Disclaimer to include in the query clause. Otherwise, all 
#'   records are returned.
#'   
#' @details The WHERE clause for the query is constructed using any of the
#'   `oid`, `parent_report_template`, or `parent_disclaimer` arguments 
#'   that have a non-zero length. The query returns all records that 
#'   satisfy all of the conditions.
#'   
#' @export

queryReportTemplateDisclaimer <- function(oid = numeric(0), 
                                          parent_report_template = numeric(0), 
                                          parent_disclaimer = numeric(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_template, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_disclaimer, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sql_server" = .queryReportTemplateDisclaimer_statement_sqlServer, 
           "sqlite"     = .queryReportTemplateDisclaimer_statement_sqlite)
  
  where <- 
    c(if (length(oid)) "OID = ?" else character(0), 
      if (length(parent_report_template)) "ParentReportTemplate = ?" else character(0), 
      if (length(parent_disclaimer)) "ParentDisclaimer = ?" else character(0))
  
  if (length(where) > 0 ){
    where <- sprintf("WHERE %s", 
                     paste0(where, collapse = " AND "))
    statement <- paste(statement, where, sep = "\n")
  }
  
  param_list <- list(oid, parent_report_template, parent_disclaimer)
  param_list <- param_list[lengths(param_list) > 0]
  
  ReportTemplateDisclaimer <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    ReportTemplateDisclaimer$IsActive <- as.logical(ReportTemplateDisclaimer$IsActive)
  }
  
  ReportTemplateDisclaimer
}


# Unexported --------------------------------------------------------

.queryReportTemplateDisclaimer_statement_sqlServer <- "
  SELECT [OID], 
         [ParentReportTemplate], 
         [ParentDisclaimer], 
         [IsActive],
         [Order]
  FROM dbo.[ReportTemplateDisclaimer]
"

.queryReportTemplateDisclaimer_statement_sqlite <- "
  SELECT [OID], 
         [ParentReportTemplate], 
         [ParentDisclaimer], 
         [IsActive], 
         [Order]
  FROM [ReportTemplateDisclaimer]
"
