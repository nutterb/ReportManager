UI_SIDEBAR_MENU <- 
  sidebarMenu(
    menuItem("Generate a Report", tabName = "tab_generateReport"), 
    menuItem("Report Templates", tabName = "tab_reportTemplate"), 
    menuItem("Schedules", tabName = "tab_scheduleManagement"), 
    menuItem("Report Layout Options", tabName = "tab_reportLayout"),
    menuItem("User Roles", tabName = "tab_roles"), 
    menuItem("Users", tabName = "tab_users"), 
    menuItem("Auto Distribution", tabName = "tab_autoDistribute"), 
    menuItem("Settings", tabName = "tab_setting")
  )