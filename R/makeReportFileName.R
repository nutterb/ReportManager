#' @name makeReportFileName
#' @title Make File Names for Report Instances
#' 
#' @description Produces a standardized file name for report instances
#' when downloading a preview or generating a report. 
#' 
#' @param report_instance_oid `integerish(1)`. The start time of the reporting period.
#'   This will be truncated to just the date portion in the filename.
#' @param is_preview `logical(1)`. When `TRUE`, the "Preview" modifier is
#'   added to the file name. 
#' @param is_submission `logical(1)`. Then `TRUE`, the next revision number
#'   is added to the file name.
#' @param file_extension `character(1)`. One of `c("zip", "html", "pdf")`.
#'   
#' @details File names take the format of 
#'   '[start_date] - [end_date]-[is_preview]-[template_name]-Rev[number].[file_extension]'.
#'   
#' @export

makeReportFileName <- function(report_instance_oid,
                               is_preview = TRUE, 
                               is_submission = FALSE, 
                               file_extension = c("zip", "html", "pdf")){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = is_preview, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_submission, 
                           len = 1, 
                           add = coll)
  
  file_extension <- checkmate::matchArg(x = file_extension, 
                                        choices = c("zip", "html", "pdf"), 
                                        .var.name = "file_extension", 
                                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .makeReportFileStatement_sqlite, 
                      "sql_server" = .makeReportFileStatement_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  TemplateInfo <- 
    DBI::dbGetQuery(
      conn = conn, 
      DBI::sqlInterpolate(
        conn = conn, 
        statement, 
        report_instance_oid = report_instance_oid
      )
    )
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    TemplateInfo$StartDateTime <- as.POSIXct(TemplateInfo$StartDateTime, 
                                             tz = "UTC")
    TemplateInfo$EndDateTime <- as.POSIXct(TemplateInfo$EndDateTime, 
                                             tz = "UTC")
  }
    
  sprintf("%s - %s%s-%s%s.%s", 
          format(TemplateInfo$StartDateTime, format = "%Y-%m-%d"),
          format(TemplateInfo$EndDateTime, format = "%Y-%m-%d"), 
          if (is_preview) "-Preview" else "", 
          TemplateInfo$TemplateName, 
          if (is_submission) "INCORPORATE REVISION" else "", 
          file_extension)
}

.makeReportFileStatement_sqlServer <- "
  SELECT RI.StartDateTime, 
  	RI.EndDateTime, 
  	RT.TemplateName
  FROM dbo.ReportInstance RI
  	LEFT JOIN dbo.ReportTemplate RT
  		ON RI.ParentReportTemplate = RT.OID
  WHERE RI.OID = ?report_instance_oid
"

.makeReportFileStatement_sqlite <- "
  SELECT RI.StartDateTime, 
  	RI.EndDateTime, 
  	RT.TemplateName
  FROM ReportInstance RI
  	LEFT JOIN ReportTemplate RT
  		ON RI.ParentReportTemplate = RT.OID
  WHERE RI.OID = ?report_instance_oid
"
