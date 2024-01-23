UI_TAB_REPORT_LAYOUT <- 
  tabsetPanel(
    id = "tabset_disclaimerFooter", 
    type = "pills",
    tabPanel(
      title = "Disclaimers",
      RM_tabLayout("disclaimer")
    ), 
    tabPanel(
      title = "Footers",
      RM_tabLayout("footer")
    ),
    tabPanel(
      title = "Logo", 
      RM_tabLayout("logo")
    )
  )
