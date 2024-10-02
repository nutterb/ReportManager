#' @name queryReportSelection
#' @title Retrieve Report Templates Configured for Reporting
#' 
#' @description Retrieve from the database the report templates that have
#'   been configured for reporting along with information necessary to 
#'   generate previews.
#'   
#' @return 
#' Retuns a `data.frame` with attributes of the report template needed for
#'   navigating the generation of reports in the UI.
#'   
#' @export

queryReportSelection <- function(){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sql_server" = .queryReportSelection_statement_sqlServer, 
           "sqlite"     = .queryReportSelection_statement_sqlite)
  
  ReportSelection <- 
    DBI::dbGetQuery(
      conn, 
      statement
    )
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    ReportSelection$IsSignatureRequired <- as.logical(ReportSelection$IsSignatureRequired)
    ReportSelection$IncludeTableOfContents <- as.logical(ReportSelection$IncludeTableOfContents)
  }
  
  ReportSelection
}

# Unexported --------------------------------------------------------

.queryReportSelection_statement_sqlServer <- 
  "SELECT RT.[OID], 
	RT.[TemplateDirectory], 
	RT.[Title], 
	S.[ScheduleName], 
	RT.[IsSignatureRequired], 
	RTS.[StartDateTime], 
	RT.[TemplateDirectory], 
	S.[Frequency] AS ScheduleFrequency,
	S.[FrequencyUnit] AS ScheduleUnit,
	S.[OffsetOverlap], 
	S.[OffsetOverlapUnit], 
	RT.[IncludeTableOfContents], 
	RTS.[IndexDateTime]
FROM dbo.ReportTemplate RT
	LEFT JOIN dbo.ReportTemplateSchedule RTS
		ON RT.[OID] = RTS.[ParentReportTemplate]
	LEFT JOIN dbo.Schedule S
		ON RTS.[ParentSchedule] = S.[OID]"

.queryReportSelection_statement_sqlite <- 
  "SELECT RT.[OID], 
	RT.[TemplateDirectory], 
	RT.[Title], 
	S.[ScheduleName], 
	RT.[IsSignatureRequired], 
	RTS.[StartDateTime], 
	RT.[TemplateDirectory], 
	S.[Frequency] AS ScheduleFrequency,
	S.[FrequencyUnit] AS ScheduleUnit,
	S.[OffsetOverlap], 
	S.[OffsetOverlapUnit], 
	RT.[IncludeTableOfContents], 
	RTS.[IndexDateTime]
FROM ReportTemplate RT
	LEFT JOIN ReportTemplateSchedule RTS
		ON RT.[OID] = RTS.[ParentReportTemplate]
	LEFT JOIN Schedule S
		ON RTS.[ParentSchedule] = S.[OID]"