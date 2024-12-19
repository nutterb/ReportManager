..dt_instance_unscheduled_cell_edit <- function(dt_instance_unscheduled_cell_edit, 
                                                rv_GenerateReport, 
                                                current_user_oid){
  edit_info <- dt_instance_unscheduled_cell_edit
  row <- edit_info$row
  val <- edit_info$value
  
  Ref <- rv_GenerateReport$UnscheduledReportInstance
  Ref <- Ref[!Ref$IsScheduled, 
             !names(Ref) %in% c("IsScheduled", "InstanceTitle")]
  Ref <- Ref[order(Ref$StartDateTime, decreasing = TRUE), ]
  
  instance_oid <- Ref$OID[row]
  
  ThisInstance <- rv_GenerateReport$UnscheduledReportInstance 
  ThisInstance <- ThisInstance[ThisInstance$OID == instance_oid, ]
  
  addEditReportInstance(report_instance_oid = ThisInstance$OID, 
                        parent_report_template = ThisInstance$ParentReportTemplate, 
                        start_time = ThisInstance$StartDateTime, 
                        end_time = ThisInstance$EndDateTime, 
                        is_signature_required = ThisInstance$IsSignatureRequired, 
                        is_scheduled = FALSE, 
                        instance_title = val, 
                        is_submitted = ThisInstance$IsSubmitted, 
                        event_user = current_user_oid)
  
  NewReportInstance <- queryReportInstance(rv_GenerateReport$SelectedTemplate)
  NewReportInstance <- NewReportInstance[!NewReportInstance$IsScheduled, ]
  
  rv_GenerateReport$UnscheduledReportInstance <- NewReportInstance
}
