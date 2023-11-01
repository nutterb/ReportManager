..btn_templateSignature_addEdit <- function(session,
                                             input, 
                                             rv_Template,
                                             current_user_oid,
                                             proxy){
  Footer <- jsonlite::fromJSON(input$templateSignature)
  Input <- Footer[c("choices", "order", "selected")]
  names(Input) <- c("ParentRole", "Order", "IsActive")
  Input <- merge(Input, 
                 rv_Template$SelectedTemplateSignature[c("OID", "ParentRole", "ParentReportTemplate")], 
                 by = "ParentRole", 
                 all.x = TRUE, 
                 all.y = TRUE)
  for(i in seq_len(nrow(Input))){
    addEditReportTemplateSignature(
      oid = if (is.na(Input$OID[i])) numeric(0) else Input$OID[i],
      parent_report_template = as.numeric(input$rdo_template),
      parent_role = as.numeric(Input$ParentRole[i]),
      order = Input$Order[i],
      is_active = isTRUE(Input$IsActive[i]),
      event_user = current_user_oid
    )
  }
  New <- queryReportTemplateSignature(parent_report_template = as.numeric(input$rdo_template))
  rv_Template$SelectedTemplateSignature <- New
  DT::replaceData(proxy = proxy,
                  data = New,
                  resetPaging = FALSE,
                  rownames = FALSE)
  toggleModal(session = session, 
              modalId = "modal_templateSignature_edit", 
              toggle = "close")
}
