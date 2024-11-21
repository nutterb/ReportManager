..out_dt_instance_scheduled <- function(rv_GenerateReport){
  selected <- as.character(rv_GenerateReport$SelectedScheduledReportInstance)
  
  Instance <- rv_GenerateReport$ScheduledReportInstance 
  Instance <- Instance[Instance$IsScheduled, 
                       !names(Instance) %in% c("ParentReportTemplate", 
                                               "IsScheduled"), ]
  Instance <- Instance[order(Instance$StartDateTime, 
                             Instance$EndDateTime, 
                             decreasing = TRUE), ]
  Instance %>% 
    radioDataTable(data = ., 
                   id_variable = "OID", 
                   element_name = "rdo_report_instance_scheduled", 
                   checked = selected) %>% 
    RM_datatable(escape = -1) %>% 
    DT::formatDate(c("StartDateTime", "EndDateTime"),
                   method = 'toLocaleTimeString',
                   params = list('en-gb',
                                 list(year = 'numeric',
                                      month = 'short',
                                      day = 'numeric',
                                      hour = 'numeric',
                                      minute = 'numeric',
                                      second = 'numeric',
                                      timeZone = 'UTC')))
}

..out_dt_instance_unscheduled <- function(rv_GenerateReport){
  selected <- as.character(rv_GenerateReport$SelectedUnscheduledReportInstance)
  
  Instance <- rv_GenerateReport$ScheduledReportInstance 
  Instance <- Instance[!Instance$IsScheduled, 
                       !names(Instance) %in% c("ParentReportTemplate", 
                                               "IsScheduled"), ]
  Instance <- Instance[order(Instance$StartDateTime, 
                             Instance$EndDateTime, 
                             decreasing = TRUE), ]
  
  Instance %>% 
    radioDataTable(data = ., 
                   id_variable = "OID", 
                   element_name = "rdo_report_instance_unscheduled", 
                   checked = selected) %>% 
    RM_datatable(escape = -1, 
                 editable = list(target = "cell", 
                                 disable = list(columns = c(0:3, 5)))) %>% 
    DT::formatDate(c("StartDateTime", "EndDateTime"),
                   method = 'toLocaleTimeString',
                   params = list('en-gb',
                                 list(year = 'numeric',
                                      month = 'short',
                                      day = 'numeric',
                                      hour = 'numeric',
                                      minute = 'numeric',
                                      second = 'numeric',
                                      timeZone = 'UTC')))
}
