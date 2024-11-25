..rdo_template <- function(rdo_template, 
                           rv_Template, 
                           rv_Schedule, 
                           session){
  oid <- as.numeric(rdo_template)
  
  rv_Template$SelectedTemplate <- 
    rv_Template$Template[rv_Template$Template$OID == oid, ]
  
  rv_Template$SelectedTemplateSchedule <- 
    queryReportTemplateSchedule(parent_report_template = oid)
  
  choice <- rv_Schedule$Schedule$OID
  names(choice) <- rv_Schedule$Schedule$ScheduleName
  
  sel <- rv_Template$SelectedTemplateSchedule$ParentSchedule
  sel = choice[choice == sel]
  
  updateSelectInput(session = session, 
                    inputId = "sel_templateSchedule", 
                    choices = choice,
                    selected = sel)
  
  updateTextInput(session = session,
                  inputId = "dttm_templateSchedule",
                  value = format(rv_Template$SelectedTemplateSchedule$StartDateTime,
                                 format = "%d-%b-%Y %H:%M"))
  
  updateTextInput(session = session,
                  inputId = "dttm_templateIndexDateTime",
                  value = format(rv_Template$SelectedTemplateSchedule$IndexDateTime,
                                 format = "%d-%b-%Y %H:%M"))
  
  rv_Template$SelectedTemplateDisclaimer <- 
    queryReportTemplateDisclaimer(parent_report_template = oid)
  
  rv_Template$SelectedTemplateFooter <-
    queryReportTemplateFooter(parent_report_template = oid)
  
  rv_Template$SelectedTemplateSignature <- 
    queryReportTemplateSignature(parent_report_template = oid)
  
  rv_Template$SelectedTemplateDistribution <- 
    queryReportTemplateDistribution(parent_report_template = oid)
}