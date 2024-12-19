..btn_addReportInstanceNote <- function(selected_instance_oid, 
                                        current_user_oid, 
                                        txt_reportInstanceNote, 
                                        proxy_dt_reportInstanceNote, 
                                        session){
  addReportInstanceNote(report_instance_oid = selected_instance_oid, 
                        parent_user = current_user_oid,
                        note = txt_reportInstanceNote)
  
  DT::replaceData(proxy = proxy_dt_reportInstanceNote, 
                  data = queryReportInstanceNote(selected_instance_oid), 
                  resetPaging = FALSE,
                  rownames = FALSE)
  updateTextAreaInput(session = session, 
                      inputId = "txt_reportInstanceNote", 
                      value = "")
}
