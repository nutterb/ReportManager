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
    
    bsModal(
      id = "modal_schedule_addEdit", 
      title = textOutput("title_schedule_addEdit"), 
      size = "large", 
      trigger = "trg_none", 
      textInput(inputId = "txt_schedule_scheduleName", 
                label = "Schedule Name"), 
      splitLayout(numericInput(inputId = "num_schedule_frequency", 
                               label = "Frequency", 
                               value = 0), 
                  selectInput(inputId = "sel_schedule_frequencyUnit", 
                              label = "Unit of Time", 
                              choices = UNIT_OF_TIME, 
                              selected = "Day"), 
                  cellWidths = c("15%", "50%")), 
      splitLayout(numericInput(inputId = "num_schedule_offset", 
                               label = "Offset", 
                               value = 0), 
                  selectInput(inputId = "sel_schedule_offsetUnit", 
                              label = "Unit of Time", 
                              choices = UNIT_OF_TIME, 
                              selected = "Day"), 
                  cellWidths = c("15%", "50%")), 
      checkboxInput(inputId = "chk_schedule_isActive", 
                    label = "Active", 
                    value = TRUE),
      actionButton(inputId = "btn_schedule_addEditSchedule", 
                   label = "Save")
    ),
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