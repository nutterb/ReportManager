# Disclaimer-Footer UI ----------------------------------------------

UI_TAB_DISCLAIMER_FOOTER <- 
  tabsetPanel(
    id = "tabset_disclaimerFooter", 
    type = "pills", 
    tabPanel(
      title = "Disclaimers", 
      disabled(actionButton(inputId = "btn_disclaimer_add", 
                            label = "Add", 
                            style = "float:right")), 
      disabled(actionButton(inputId = "btn_disclaimer_edit", 
                            label = "Edit", 
                            style = "float:right")), 
      disabled(actionButton(inputId = "btn_disclaimer_deactivate", 
                            label = "Deactivate", 
                            style = "float:right")), 
      disabled(actionButton(inputId = "btn_disclaimer_activate", 
                            label = "Activate", 
                            style = "float:right")), 
      radioDataTableOutput(outputId = "dt_disclaimer", 
                           radioId = "rdo_disclaimer")
    ), 
    tabPanel(
      title = "Footers", 
      disabled(actionButton(inputId = "btn_footer_add", 
                            label = "Add", 
                            style = "float:right")), 
      disabled(actionButton(inputId = "btn_footer_edit", 
                            label = "Edit", 
                            style = "float:right")), 
      disabled(actionButton(inputId = "btn_footer_deactivate", 
                            label = "Deactivate", 
                            style = "float:right")), 
      disabled(actionButton(inputId = "btn_footer_activate", 
                            label = "Activate", 
                            style = "float:right")), 
      radioDataTableOutput(outputId = "dt_footer", 
                           radioId = "rdo_footer")
    )
  )

# Modal Disclaimer --------------------------------------------------
MODAL_DISCLAIMER <- 
  bsModal(
    id = "modal_disclaimer_addEdit", 
    title = uiOutput("title_disclaimer_addEdit"), 
    size = "large", 
    trigger = "trg_none", 
    textAreaInput(inputId = "txt_disclaimer_text", 
                  label = "Disclaimer", 
                  width = "400px", 
                  height = "100px"),
    checkboxInput(inputId = "chk_disclaimer_isActive", 
                  label = "Active", 
                  value = TRUE),
    actionButton(inputId = "btn_disclaimer_addEditDisclaimer", 
                 label = "Save")
  )

# Modal Footer ------------------------------------------------------

MODAL_FOOTER <- 
  bsModal(
    id = "modal_footer_addEdit", 
    title = uiOutput("title_footer_addEdit"), 
    size = "large", 
    trigger = "trg_none", 
    textInput(inputId = "txt_footer_text", 
              label = "Footer"),
    checkboxInput(inputId = "chk_footer_isActive", 
                  label = "Active", 
                  value = TRUE),
    actionButton(inputId = "btn_footer_addEditFooter", 
                 label = "Save")
  )  