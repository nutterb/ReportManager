# -------------------------------------------------------------------
# File Name: inst/Application/ui.R
# Description: User Interface design for the Reports Manager Application

# Dashboard Header --------------------------------------------------
header <- dashboardHeader(title = "Report Manager")

# Dashboard Sidebar -------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Generate Report"), 
    menuItem("Report Templates"), 
    menuItem("Schedules"), 
    menuItem("Roles"), 
    menuItem("Distribution")
  )
)

# Dashboard Body ----------------------------------------------------
body <- dashboardBody(
  
)

# Form the Dashboard ------------------------------------------------
dashboardPage(header = header, 
              sidebar = sidebar,
              body = body)


# Change Log --------------------------------------------------------
# Date         User       Note
# 2021-09-03   bnutter    Created file.
