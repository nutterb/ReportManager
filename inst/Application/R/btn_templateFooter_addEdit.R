..btn_templateFooter_addEdit <- function(session,
                                          input, 
                                          rv_Template,
                                          current_user_oid,
                                          proxy){
  Footer <- jsonlite::fromJSON(input$templateFooter)
  Input <- Footer[c("choices", "order", "selected")]
  names(Input) <- c("ParentFooter", "Order", "IsActive")
  Input <- merge(Input, 
                 rv_Template$SelectedTemplateFooter[c("OID", "ParentFooter", "ParentReportTemplate")], 
                 by = "ParentFooter", 
                 all.x = TRUE, 
                 all.y = TRUE)
  for(i in seq_len(nrow(Input))){
    addEditReportTemplateFooter(
      oid = if (is.na(Input$OID[i])) numeric(0) else Input$OID[i],
      parent_report_template = as.numeric(input$rdo_template),
      parent_footer = as.numeric(Input$ParentFooter[i]),
      order = Input$Order[i],
      is_active = isTRUE(Input$IsActive[i]),
      event_user = current_user_oid
    )
  }
  New <- queryReportTemplateFooter(parent_report_template = as.numeric(input$rdo_template))
  rv_Template$SelectedTemplateFooter <- New
  DT::replaceData(proxy = proxy,
                  data = New,
                  resetPaging = FALSE,
                  rownames = FALSE)
  toggleModal(session = session, 
              modalId = "modal_templateFooter_edit", 
              toggle = "close")
}
