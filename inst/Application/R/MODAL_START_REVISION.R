MODAL_START_REVISION <- 
  bsModal(
    id = "modal_genReport_instanceSubmit_startRevision", 
    title = "Start Revision", 
    size = "large", 
    trigger = "trg_none", 
    textAreaInput(inputId = "txt_genReport_instanceSubmit_reasonRevise", 
                  label = "Reason for Revision", 
                  width = "70%", 
                  height = "200px"), 
    actionButton(inputId = "btn_genReport_instanceSubmit_confirmRevision", 
                 label = "Start Revision")
  )