..cd_genReport_scheduledReport <- function(cd_genReport_scheduledReport, 
                                           session){
  if (isTRUE(cd_genReport_scheduledReport)){
    updateCheckboxInput(session = session, 
                        inputId = "cd_genReport_unscheduledReport", 
                        value = FALSE)
    updateCheckboxInput(session = session, 
                        inputId = "cd_genReport_adhocReport", 
                        value = FALSE)
  }
}

..cd_genReport_unscheduledReport <- function(cd_genReport_unscheduledReport, 
                                             session){
  if (isTRUE(cd_genReport_unscheduledReport)){
    updateCheckboxInput(session = session, 
                        inputId = "cd_genReport_scheduledReport", 
                        value = FALSE)
    updateCheckboxInput(session = session, 
                        inputId = "cd_genReport_adhocReport", 
                        value = FALSE)
  }
}

..cd_genReport_adhocReport <- function(cd_genReport_adhocReport, 
                                       session){
  if (isTRUE(cd_genReport_adhocReport)){
    updateCheckboxInput(session = session, 
                        inputId = "cd_genReport_scheduledReport", 
                        value = FALSE)
    updateCheckboxInput(session = session, 
                        inputId = "cd_genReport_unscheduledReport", 
                        value = FALSE)
    
  }
}