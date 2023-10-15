#' @name queryReportTemplateSchedule
#' @title Retrive ReportTemplateSchedule Records
#' 
#' @description Provides the user with an interface to retrieve 
#'   ReportTemplateRecords
#'   
#' @param oid `integerish(0/1)`. When length is 1, the OID of the 
#'   ReportTemplateDisclaimer to be retrieved. Otherwise, all 
#'   records are returned.
#' @param parent_report_template `integerish(0/1)`. When length is 1, the 
#'   OID of the ReportTemplate to include in the query clause. Otherwise, 
#'   all records are returned.
#' @param parent_schedule `integerish(0/1)`. When length is 1, the OID
#'   of the Schedule to include in the query clause. Otherwise, all 
#'   records are returned.
#'   
#' @details The WHERE clause for the query is constructed using any of the
#'   `oid`, `parent_report_template`, or `parent_schedule` arguments 
#'   that have a non-zero length. The query returns all records that 
#'   satisfy all of the conditions.
#'   
#' @export

queryReportTemplateSchedule <- function(oid = numeric(0), 
                                        parent_report_template = numeric(0), 
                                        parent_schedule = numeric(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_template, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_schedule, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sql_server" = .queryReportTemplateSchedule_statement_sqlServer, 
           "sqlite"     = .queryReportTemplateSchedule_statement_sqlite)
  
  where <- 
    c(if (length(oid)) "OID = ?" else character(0), 
      if (length(parent_report_template)) "ParentReportTemplate = ?" else character(0), 
      if (length(parent_schedule)) "ParentSchedule = ?" else character(0))
  
  if (length(where) > 0 ){
    where <- sprintf("WHERE %s", 
                     paste0(where, collapse = " AND "))
    statement <- paste(statement, where, sep = "\n")
  }
  
  param_list <- list(oid, parent_report_template, parent_schedule)
  param_list <- param_list[lengths(param_list) > 0]
  
  ReportTemplateSchedule <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    ReportTemplateSchedule$IsActive <- as.logical(ReportTemplateSchedule$IsActive)
    ReportTemplateSchedule$StartDateTime <- as.POSIXct(ReportTemplateSchedule$StartDateTime, 
                                                       origin = "1970-01-01 00:00:00")
  }
  
  ReportTemplateSchedule
}


# Unexported --------------------------------------------------------

.queryReportTemplateSchedule_statement_sqlServer <- "
  SELECT [OID], 
         [ParentReportTemplate], 
         [ParentSchedule], 
         [StartDateTime],
         [IsActive]
  FROM dbo.[ReportTemplateSchedule]
"

.queryReportTemplateSchedule_statement_sqlite <- "
  SELECT [OID], 
         [ParentReportTemplate], 
         [ParentSchedule], 
         [StartDateTime],
         [IsActive]
  FROM [ReportTemplateSchedule]
"
