dashboardPage(
  title = "Report Manager",
  # Dashboard Header ------------------------------------------------
  dashboardHeader(
    title = "Report Manager", 
    titleWidth = 250
  ), 
  
  # Dashboard Sidebar -----------------------------------------------
  dashboardSidebar(
    width = 250, 
    UI_SIDEBAR_MENU # Defined in Application/R/ui-sidebar-menu.R
  ), 
  
  # Dashboard Body --------------------------------------------------
  dashboardBody(
    # Styles and javascript -----------------------------------------
    UI_STYLES_AND_SCRIPTS, # Defined in Application/R/ui-styles-scripts.R
    
    # Modals --------------------------------------------------------
    
    MODAL_SCHEDULE,
    MODAL_DATE_REPORT_FORMAT,
    MODAL_ROLES,
    MODAL_USER_ROLE,
    MODAL_USER,
    
    # Menu Pages ----------------------------------------------------
    
    # Generate a Report ---------------------------------------------
    tabItems(
      tabItem(
        "tab_generateReport" 
      ), 
      
      tabItem(
        "tab_reportTemplate"
      ), 
      
      tabItem("tab_scheduleManagement", UI_TAB_SCHEDULE_PAGE), 
      
      tabItem(
        "tab_disclaimerFooter"
      ), 
      
      tabItem("tab_roles", UI_TAB_ROLES), 
      
      tabItem("tab_users", UI_TAB_REPORT_USER), 
      
      tabItem("tab_autoDistribute")
    )
  )
)