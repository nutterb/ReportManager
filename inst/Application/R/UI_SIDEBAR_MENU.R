UI_SIDEBAR_MENU <- 
  sidebarMenu(
    menuItem("Generate a Report", tabName = "tab_generateReport"), 
    menuItem("Report Templates", tabName = "tab_reportTemplate"), 
    menuItem("Schedules", tabName = "tab_scheduleManagement"), 
    menuItem("Disclaimers and Footers", tabName = "tab_disclaimerFooter"),
    menuItem("User Roles", tabName = "tab_userRoles"), 
    menuItem("Users", tabName = "tab_users")
  )