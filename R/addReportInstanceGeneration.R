#' @name addReportInstanceGeneration
#' @title Add Records for Report Instance Previews
#' 
#' @description Provides the user with tools to add records for when an 
#' instance is previewed. 
#' 
#' @param report_instance_oid `integerish(1)`. The OID of the report instance
#'   being previewed.
#' @param report_template_oid `integerish(1)`. The OID of the report template
#'   of which an instance is being previewed. 
#' @param start_date_time `POSIXct(1)`. The start time of the reporting period.
#' @param end_date_time `POSIXct(1)`. The end time of the reporting period.
#' @param report_format `character(1)`. The format of the instance being produced by
#'   the application.  One of `c("preview", "html", "pdf", "shiny")`.
#' @param include_data `logical(1)`. Is the data being included with the 
#'   download of the preview. Only applies to the HTML and PDF preview 
#'   downloads.
#' @param is_preview `logical(1)`. Indicates if the action is generating a 
#'   preview of the report. Previews cannot be archived in the database; may 
#'   not be distributed; and is not submitted externally.
#' @param is_distributed `logical(1)`. Indicates if the instance was distributed
#'   to the users on the distribution list.
#' @param is_archived `logical(1)`. Indicates if the instance was saved to 
#'   the archive.
#' @param is_submission `logical(1)`. Indicates if the instance was submitted
#'   externally. 
#' @param user_oid `integerish(1)`. The OID of the user initiating the
#'   preview.
#' @param preview_date_time `POSIXct(1)`. The date and time the preview was 
#'   initiated.
#'   
#' @export

addReportInstanceGeneration <- function(report_instance_oid,
                                        report_template_oid, 
                                        start_date_time, 
                                        end_date_time, 
                                        report_format = c("preview", "html", "pdf", "shiny"), 
                                        include_data, 
                                        is_preview, 
                                        is_distributed, 
                                        is_archived, 
                                        is_submission,
                                        user_oid, 
                                        preview_date_time = Sys.time()){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = report_template_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertPOSIXct(x = start_date_time, 
                           len = 1,
                           add = coll)
  
  checkmate::assertPOSIXct(x = end_date_time, 
                           len = 1,
                           add = coll)
  
  report_format <- checkmate::matchArg(x = report_format, 
                                       choices = c("preview", "html", "pdf", "shiny"),
                                       .var.name = "report_format",
                                       add = coll)
  
  checkmate::assertLogical(x = include_data, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_preview, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_distributed, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_archived, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_submission, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = user_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertPOSIXct(x = preview_date_time, 
                           len = 1,
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  AddData <- data.frame(ParentReportInstance = report_instance_oid, 
                        ParentReportTemplate = report_template_oid, 
                        StartDateTime = start_date_time, 
                        EndDateTime = end_date_time, 
                        ReportFormat = report_format, 
                        IncludeData = include_data, 
                        IsPreview = is_preview, 
                        IsDistributed = is_distributed, 
                        IsArchived = is_archived, 
                        IsSubmission = is_submission, 
                        PreviewDateTime = format(preview_date_time, 
                                                 format = "%Y-%m-%d %H:%M:%S"),
                        ParentUser = user_oid)
  
  insertRecord(data = AddData, 
               table_name = "ReportInstanceGeneration")
}
