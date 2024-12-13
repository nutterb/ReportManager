..btn_templateDistribution_addEdit <- function(templateDistributionUser,
                                               templateDistributionRole, 
                                               rdo_template, 
                                               rv_Template,
                                               current_user_oid,
                                               session){
  TemplateDist <- rv_Template$SelectedTemplateDistribution

  DistributionUser <- jsonlite::fromJSON(templateDistributionUser)
  InputUser <- DistributionUser[c("choices", "selected")]
  names(InputUser) <- c("ParentUser", "IsActive")
  InputUser$ParentRole <- rep(NA_real_, nrow(InputUser))
  InputUser <- merge(InputUser, 
                     TemplateDist[!is.na(TemplateDist$ParentUser), 
                                  c("OID", "ParentUser", "ParentReportTemplate")], 
                     by = c("ParentUser"),
                     all.x = TRUE, 
                     all.y = TRUE)
  InputUser <- InputUser[(InputUser$IsActive & is.na(InputUser$ParentReportTemplate)) | # New records
                           !is.na(InputUser$ParentReportTemplate), ]

  DistributionRole <- jsonlite::fromJSON(templateDistributionRole)
  InputRole <- DistributionRole[c("choices", "selected")]
  names(InputRole) <- c("ParentRole", "IsActive")
  InputRole$ParentUser <- rep(NA_real_, nrow(InputRole))
  InputRole <- merge(InputRole, 
                     TemplateDist[!is.na(TemplateDist$ParentRole), 
                                  c("OID", "ParentRole", "ParentReportTemplate")], 
                     by = c("ParentRole"),
                     all.x = TRUE, 
                     all.y = TRUE)
  InputRole <- InputRole[(InputRole$IsActive & is.na(InputRole$ParentReportTemplate)) | # New records
                           !is.na(InputRole$ParentReportTemplate), ]                    # Existing records
  
  Input <- rbind(InputUser[c("OID", "ParentUser", "ParentRole", "IsActive", "ParentReportTemplate")], 
                 InputRole[c("OID", "ParentUser", "ParentRole", "IsActive", "ParentReportTemplate")])

  for(i in seq_len(nrow(Input))){
    addEditReportTemplateDistribution(
      oid = if (is.na(Input$OID[i])) numeric(0) else Input$OID[i],
      parent_report_template = as.numeric(rdo_template),
      parent_user = if (is.na(Input$ParentUser[i])) numeric(0) else as.numeric(Input$ParentUser[i]),
      parent_role = if (is.na(Input$ParentRole[i])) numeric(0) else as.numeric(Input$ParentRole[i]),
      is_active = isTRUE(Input$IsActive[i]),
      event_user = current_user_oid
    )
  }
  New <- queryReportTemplateDistribution(parent_report_template = as.numeric(rdo_template))
  rv_Template$SelectedTemplateDistribution <- New
  
  toggleModal(session = session, 
              modalId = "modal_templateDistribution_edit", 
              toggle = "close")
}
