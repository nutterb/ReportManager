..out_dt_genReport_template <- function(rv_GenerateReport){
  rv_GenerateReport$Templates[c("OID", "TemplateDirectory", 
                                "Title", "ScheduleName", 
                                "IsSignatureRequired", "StartDateTime")] %>%
    radioDataTable(id_variable = "OID", 
                   element_name = "rdo_genReport_template") %>% 
    transform(TemplateDirectory = factor(TemplateDirectory), 
              Title = factor(Title), 
              ScheduleName = factor(ScheduleName)) %>% 
    RM_datatable(colnames = c("Select", "Template", "Title", 
                              "Schedule", "Signature Required", 
                              "Reporting Start"),
                 escape = -1) %>% 
    DT::formatDate(c("StartDateTime"),
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