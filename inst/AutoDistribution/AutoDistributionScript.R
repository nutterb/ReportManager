args <- commandArgs(trailingOnly = TRUE)

library(ReportManager)

SETTINGS <- queryApplicationSetting()

SMTP <- SETTINGS$SettingValue[SETTINGS$SettingKey == "smtpServer"]

if (!is.na(SMTP)){
  options(RM_smtpServer = SMTP)
}

# Set the ZIP Executable location on startup 

ZIP <- SETTINGS$SettingValue[SETTINGS$SettingKey == "zipExecutable"]

if (!is.na(ZIP) && trimws(ZIP) != ""){
  Sys.setenv("R_ZIPCMD" = ZIP)
}

# Set the Pandoc directory location on startup

PANDOC <- trimws(SETTINGS$SettingValue[SETTINGS$SettingKey == "pandocDirectory"])

if (!isTRUE(length(PANDOC) == 0 | PANDOC %in% c(NA, ""))){
  rmarkdown::find_pandoc(dir = PANDOC, 
                         cache = FALSE)
  Sys.setenv(PATH = PANDOC)
  Sys.setenv(RSTUDIO_PANDOC = PANDOC)
}

# Get the current user

USER <- queryUser()
USER <- USER[USER$LoginId == Sys.info()["user"], ]


ToDistribute <- getInstanceToAutoDistribute()

message("Reports to Distribute This Iteration--------------------")

ToDistribute

for (i in seq_len(nrow(ToDistribute))){
  submitReport(
    report_instance_oid = ToDistribute$OID[i],
    is_submission = TRUE, 
    is_distribute = TRUE, 
    is_distribute_internal_only = ToDistribute$IsDistributeInternalOnly[i],
    is_add_to_archive = ToDistribute$IsAddToArchive[i], 
    is_embed_html = ToDistribute$IsEmbedHtml[i],
    params = list(), 
    report_format = ToDistribute$ReportFormat[i], 
    current_user_oid = USER$OID
  )
}
