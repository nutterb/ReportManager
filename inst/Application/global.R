library(shiny)
library(shinyBS)
library(shinybusy)
library(shinydashboard)
library(shinyjs)
library(DT)
library(magrittr)
library(ReportManager)
library(sendmailR)

TEMPLATE_FOLDERS <- system.file(package = "ReportManager")
TEMPLATE_FOLDERS <- file.path(TEMPLATE_FOLDERS, "ReportTemplate")
TEMPLATE_FOLDERS <- list.dirs(TEMPLATE_FOLDERS, recursive = FALSE)
TEMPLATE_FOLDERS <- basename(TEMPLATE_FOLDERS)

SETTINGS <- queryApplicationSetting()

# Set the SMTP Server on startup

SMTP <- SETTINGS$SettingValue[SETTINGS$SettingKey == "smtpServer"]

if (!is.na(SMTP)){
  options(RM_smtpServer = SMTP)
}

# Set the ZIP Executable location on startup 

ZIP <- SETTINGS$SettingValue[SETTINGS$SettingKey == "zipExecutable"]

if (!is.na(ZIP) && trimws(ZIP) != ""){
  Sys.setenv("R_ZIPCMD" = ZIP)
}