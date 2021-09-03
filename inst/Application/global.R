#--------------------------------------------------------------------
# File Name: inst/Application/global.R
# Description: Loads packages and options required globally
#              by the ReportManager application
#

# Packages ----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ReportManager)

options(
  report_manager_db_path = system.file("ReportManagerStructureDevel.accdb", 
                                       package = "ReportManager")
)

# Options -----------------------------------------------------------

# Other Objects -----------------------------------------------------



# Change Log --------------------------------------------------------
# Date         User       Note
# 2021-09-03   bnutter    Created file.

