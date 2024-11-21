..btn_genReport_unscheduled_changeSignatureRequirement <- function(rv_GenerateReport, 
                                                                   current_user_oid){
  ThisInstance <- rv_GenerateReport$UnscheduledReportInstance 
  ThisInstance <- ThisInstance[ThisInstance$OID == 
                                 rv_GenerateReport$SelectedUnscheduledReportInstance, ]
  
  addEditReportInstance(report_instance_oid = ThisInstance$OID, 
                        parent_report_template = ThisInstance$ParentReportTemplate, 
                        start_time = ThisInstance$StartDateTime, 
                        end_time = ThisInstance$EndDateTime, 
                        is_signature_required = !ThisInstance$IsSignatureRequired, 
                        is_scheduled = FALSE, 
                        instance_title = ThisInstance$InstanceTitle, 
                        is_submitted = ThisInstance$IsSubmitted, 
                        event_user = current_user_oid)
  
  NewInstance <- queryReportInstance(rv_GenerateReport$SelectedTemplate)
  NewInstance <- NewInstance[!NewInstance$IsScheduled, ]
  
  rv_GenerateReport$UnscheduledReportInstance <- NewInstance 
}
