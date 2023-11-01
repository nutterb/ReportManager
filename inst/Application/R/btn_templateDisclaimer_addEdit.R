..btn_templateDisclaimer_addEdit <- function(session,
                                                     input, 
                                                     rv_Template,
                                                     current_user_oid,
                                                     proxy){
  Disclaimer <- jsonlite::fromJSON(input$templateDisclaimer)
  Input <- Disclaimer[c("choices", "order", "selected")]
  names(Input) <- c("ParentDisclaimer", "Order", "IsActive")
  Input <- merge(Input, 
                 rv_Template$SelectedTemplateDisclaimer[c("OID", "ParentDisclaimer", "ParentReportTemplate")], 
                 by = "ParentDisclaimer", 
                 all.x = TRUE, 
                 all.y = TRUE)
  for(i in seq_len(nrow(Input))){
    addEditReportTemplateDisclaimer(
      oid = if (is.na(Input$OID[i])) numeric(0) else Input$OID[i],
      parent_report_template = as.numeric(input$rdo_template),
      parent_disclaimer = as.numeric(Input$ParentDisclaimer[i]),
      order = Input$Order[i],
      is_active = isTRUE(Input$IsActive[i]),
      event_user = current_user_oid
    )
  }
  New <- queryReportTemplateDisclaimer(parent_report_template = as.numeric(input$rdo_template))
  rv_Template$SelectedTemplateDisclaimer <- New
  DT::replaceData(proxy = proxy,
                  data = New,
                  resetPaging = FALSE,
                  rownames = FALSE)
  toggleModal(session = session, 
              modalId = "modal_templateDisclaimer_edit", 
              toggle = "close")
}
