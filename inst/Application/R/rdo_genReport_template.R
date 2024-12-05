..rdo_genReport_template <- function(rdo_genReport_template, 
                                     current_user_oid, 
                                     rv_GenerateReport){
  template_oid <- as.numeric(rdo_genReport_template)
  rv_GenerateReport$SelectedTemplate <- template_oid
  
  updateReportInstanceSchedule(report_template_oid = template_oid,
                               event_user = current_user_oid)
  
  rv_GenerateReport$SelectedTemplateData <- queryReportTemplate(oid = template_oid)
  
  ReportInstance <- queryReportInstance(report_template_oid = template_oid)
  
  rv_GenerateReport$ScheduledReportInstance <-
    ReportInstance[ReportInstance$IsScheduled, ]
  
  rv_GenerateReport$UnscheduledReportInstance <-
    ReportInstance[!ReportInstance$IsScheduled, ]
  
  rv_GenerateReport$ReportTemplateUserPermission <- 
    queryReportTemplateUserPermission(parent_report_template = template_oid, 
                                      parent_user = current_user_oid)
  
  shinyjs::show("div_genReport_reportInstance") 
  shinyjs::hide("h3_genReport_reportInstance_noTemplateSelected")
}
