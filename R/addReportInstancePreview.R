#' @name addReportInstancePreview
#' @title Add Records for Report Instance Previews
#' 
#' @description Provides the user with tools to add records for when an 
#' instance is previewed. 
#' 
#' @param report_instance_oid `integerish(1)`. The OID of the report instance
#'   being previewed.
#' @param preview_type `character(1)`. The type of preview being produced by
#'   the application.  One of `c("preview", "html", "pdf", "shiny")`.
#' @param include_data `logical(1)`. Is the data being included with the 
#'   download of the preview. Only applies to the HTML and PDF preview 
#'   downloads.
#' @param preview_datetime `POSIXct(1)`. The date and time the preview was 
#'   initiated.
#' @param preview_user `integerish(1)`. The OID of the user initiating the
#'   preview.
#'   
#' @export

addReportInstancePreview <- function(report_instance_oid, 
                                     include_data, 
                                     preview_user, 
                                     preview_type = c("preview", "html", "pdf", "shiny"), 
                                     preview_datetime = Sys.time()){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = include_data, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = preview_user, 
                              len = 1, 
                              add = coll)
  
  preview_type <- checkmate::matchArg(x = preview_type, 
                                      choices = c("preview", "html", "pdf", "shiny"),
                                      .var.name = "preview_type",
                                      add = coll)
  
  checkmate::assertPOSIXct(x = preview_datetime, 
                           len = 1,
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .addReportInstancePreview_sqlite, 
                      "sql_server" = .addReportInstancePreview_sqlServer, 
                      stop(sprintf("Query not defined for SQL flavor '%s'",
                                   getOption("RM_sql_flavor"))))
  
  result <- 
    DBI::dbSendStatement(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        report_instance_oid = report_instance_oid, 
        preview_type = preview_type,
        include_data = include_data, 
        preview_datetime = format(preview_datetime, 
                                  format = "%Y-%m-%d %H:%M:%S"), 
        preview_user = preview_user
      )
    )
  
  DBI::dbClearResult(result)
}

# Unexported --------------------------------------------------------

.addReportInstancePreview_sqlite <- "
INSERT INTO ReportInstancePreview
(ParentReportInstance, PreviewType, IncludeData, PreviewDateTime, PreviewUser)
VALUES
(?report_instance_oid, ?preview_type, ?include_data, ?preview_datetime, ?preview_user);
"

.addReportInstancePreview_sqlServer <- "
INSERT INTO dbo.ReportInstancePreview
(ParentReportInstance, PreviewType, IncludeData, PreviewDateTime, PreviewUser)
VALUES
(?report_instance_oid, ?preview_type, ?include_data, ?preview_datetime, ?preview_user);
"
