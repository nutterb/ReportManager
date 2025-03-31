#' @name submitReport
#' @title Submit a Report Instance
#' 
#' @description Generate report files, add them to the archive, and 
#'   submit them to internal/external clients.
#'   
#' @param report_instance_oid `integerish(1)`. The OID of the report instance.
#' @param is_submission `logical(1)`. When `TRUE`, the report is being counted
#'   as a submission and will be assigned a revision number.
#' @param is_distribute `logical(1)`. When `TRUE`, the report will be distributed
#'   via e-mail.
#' @param is_distribute_internal_only `logical(1)`. When `TRUE` 
#'   (and `is_distribute` is `TRUE`), the report is only distributed to internal
#'   recipients. (Ignored when `is_distribute` is `FALSE`).
#' @param is_add_to_archive `logical(1)`. When `TRUE`, report files are added
#'   to the file archive.
#' @param is_embed_html `logical(1)`. When `TRUE` (and the `report_format` is 
#'   `"html"`), the report content is embedded in the e-mail (instead of 
#'   attached).
#' @param params named `list`. Parameters to pass to the report template.
#' @param report_format `character(1)`. The file format for the report. One of
#'   `c("html", "pdf")`. 
#' @param current_user_oid `integerish(1)`. The OID of the current user OID.
#' 
#' @export

submitReport <- function(report_instance_oid, 
                         is_submission, 
                         is_distribute, 
                         is_distribute_internal_only,
                         is_add_to_archive, 
                         is_embed_html,
                         params = list(), 
                         report_format, 
                         current_user_oid){
  
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = is_submission, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_distribute, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_distribute_internal_only, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_add_to_archive, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_embed_html, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertList(x = params, 
                        names = "named", 
                        add = coll)
  
  report_format <- checkmate::matchArg(x = report_format, 
                                       choices = c("html", "pdf"), 
                                       add = coll, 
                                       .var.name = "report_format")
  
  checkmate::assertIntegerish(x = current_user_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  # Get the Report Instance Data and Report Template Data
  
  ReportInstance <- queryReportInstance(report_instance_oid = report_instance_oid)
  
  ReportTemplate <- queryReportTemplate(oid = ReportInstance$ParentReportTemplate)
  
  
  # Generate the report files
  report_files <- 
    makeReportForArchive(report_instance_oid = report_instance_oid, 
                         include_data = ReportTemplate$IsIncludeData,
                         is_submission = is_submission,
                         build_dir = tempdir(), 
                         params = params, 
                         report_format = report_format)
  
  # Add the generation of the report to the event history
  InstanceGeneration <- 
    addReportInstanceGeneration(
      report_instance_oid = report_instance_oid, 
      report_template_oid = ReportTemplate$OID,
      start_date_time = ReportInstance$StartDateTime, 
      end_date_time = ReportInstance$EndDateTime, 
      report_format = report_format, 
      include_data = ReportTemplate$IsIncludeData, 
      is_preview = FALSE, 
      is_distributed = is_distribute, 
      is_archived = is_add_to_archive, 
      is_submission = is_submission, 
      user_oid = current_user_oid
    )
  
  # Add files to the archive
  if (is_add_to_archive){
    for (rf in report_files){
      addFileArchive(parent_report_template = ReportTemplate$OID,
                     parent_report_instance = report_instance_oid, 
                     file_path = rf)
    }
  }
  
  # Distribute files via e-mail
  if (is_distribute){
    Distribution <- queryInstanceDistributionSelection(report_instance_oid = report_instance_oid)
    Distribution <- Distribution[Distribution$IsActive, ]
    
    if (!is_submission){
      Distribution <- Distribution[Distribution$IsInternal, ]
    }
    
    sendEmail(from_user_oid = current_user_oid,
              to_address = Distribution$EmailAddress,
              report_template = ReportTemplate,
              report_instance_oid = report_instance_oid,
              message = "",#input$txt_genReport_reportInstance_emailMessage,
              filename = report_files, 
              embed_html = is_embed_html)
    
    if (is_submission){
      addReportInstanceGenerationRecipient(
        report_instance_generation_oid = InstanceGeneration$OID, 
        user_oid = Distribution$ParentUser
      )
      updateInstanceIsSubmitted(report_instance_oid = report_instance_oid, 
                                is_submitted = TRUE, 
                                current_user_oid = current_user_oid)
    }
    
  }
}
