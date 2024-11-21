..btn_genReport_unscheduled_addUnscheduledReport <- function(dttm_genReport_newUnscheduledInstance,
                                                             chk_genReport_unscheduled_isSignatureRequired,
                                                             rv_GenerateReport, 
                                                             current_user_oid){
  start_end <- 
    strsplit(dttm_genReport_newUnscheduledInstance, 
             split = " - ") %>% 
    unlist() %>% 
    as.POSIXct(format = "%d-%b-%Y %H:%M", tz = "UTC")
  
  addEditReportInstance(report_instance_oid = numeric(0), 
                        parent_report_template = rv_GenerateReport$SelectedTemplate, 
                        start_time = start_end[1], 
                        end_time = start_end[2], 
                        is_signature_required = chk_genReport_unscheduled_isSignatureRequired, 
                        is_scheduled = FALSE, 
                        instance_title = "", 
                        is_submitted = FALSE, 
                        event_user = current_user_oid)
  
  rv_GenerateReport$ScheduledReportInstance <- 
    queryReportInstance(report_template_oid = rv_GenerateReport$SelectedTemplate)
}