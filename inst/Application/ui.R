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
    
    MODAL_REPORT_INSTANCE_DISTRIBUTE,
    MODAL_START_REVISION,
    MODAL_REPORT_TEMPLATE,
    MODAL_REPORT_TEMPLATE_DISCLAIMER,
    MODAL_REPORT_TEMPLATE_FOOTER,
    MODAL_REPORT_TEMPLATE_SIGNATURE,
    MODAL_REPORT_TEMPLATE_DISTRIBUTION,
    MODAL_SCHEDULE,
    MODAL_DATE_REPORT_FORMAT,
    MODAL_DISCLAIMER,
    MODAL_FOOTER,
    MODAL_LOGO_ADD,
    MODAL_ROLES,
    MODAL_USER_ROLE,
    MODAL_USER,
    
    bsModal(
      id = "modal_templatePermission_addEdit",
      title = "Edit Template Permissions",
      trigger = "trg_none",
      size = "large",

      selectInput(inputId = "sel_templatePermissionRole",
                  label = "Role", 
                  choices = character(0)),

      checkboxGroupInput(inputId = "chkgrp_templatePermission",
                         label = "Permissions for Role",
                         choices = c("View" = "CanView",
                                     "Add Notes" = "CanAddNotes",
                                     "Edit Narrative" = "CanEditNarrative",
                                     "Submit" = "CanSubmit",
                                     "Start Revision" = "CanStartRevision"),
                         selected = "CanView"),

      actionButton(inputId = "btn_saveTemplatePermission",
                   label = "Save")
    ),
    
    # Menu Pages ----------------------------------------------------
    
    add_busy_spinner(spin = "fading-circle", 
                     position = "top-right"),
    
    # Generate a Report ---------------------------------------------
    tabItems(
      tabItem("tab_generateReport", UI_TAB_GENERATE_REPORT), 
      
      tabItem("tab_reportTemplate", UI_TAB_REPORT_TEMPLATE_PAGE), 
      
      tabItem("tab_scheduleManagement", UI_TAB_SCHEDULE_PAGE), 
      
      tabItem("tab_reportLayout", UI_TAB_REPORT_LAYOUT), 
      
      tabItem("tab_roles", UI_TAB_ROLES), 
      
      tabItem("tab_users", UI_TAB_REPORT_USER), 
      
      tabItem("tab_autoDistribute", UI_TAB_AUTODISTRIBUTE)
    )
  )
)