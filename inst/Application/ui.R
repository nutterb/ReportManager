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
    
    MODAL_ROLES,
    
    bsModal(
      id = "modal_userRole_edit", 
      title = textOutput("title_userRole_edit"), 
      size = "large", 
      trigger = "trg_none", 
      multiSelect(inputId = "multi_userRole", 
                  label = "User Role Assignments", 
                  choices = character(0), 
                  selected = character(0), 
                  size = 15), 
      br(),
      actionButton(inputId = "btn_userRole_save", 
                   label = "Save")
    ),
    
    MODAL_REPORT_USER,
    
    # Menu Pages ----------------------------------------------------
    
    # Generate a Report ---------------------------------------------
    tabItems(
      tabItem(
        "tab_generateReport" 
      ), 
      
      tabItem(
        "tab_reportTemplate"
      ), 
      
      tabItem(
        "tab_scheduleManagement"
      ), 
      
      tabItem(
        "tab_disclaimerFooter"
      ), 
      
      tabItem("tab_roles", UI_TAB_ROLES), 
      
      tabItem("tab_users", UI_TAB_REPORT_USER), 
      
      tabItem("tab_autoDistribute")
    )
  )
)