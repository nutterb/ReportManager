# UI - Report Template Page -----------------------------------------
UI_TAB_REPORT_TEMPLATE_PAGE <- 
  tabsetPanel(
    id = "tabset_reportTemplate",
    type = "pills",
    header = uiOutput("title_reportTemplateTabset"),
    tabPanel(
      title = "Template",
      RM_tabLayout("template")
    ),
    tabPanel(
      title = "Schedule"
    ),
    tabPanel(
      title = "Layout", 
      tabsetPanel(
        id = "tabset_reportTemplateLayout", 
        type = "pills", 
        tabPanel(
          title = "Disclaimers",
          RM_tabLayout("reportTemplate_disclaimer", 
                       add = FALSE, 
                       activate = FALSE)
        ),
        tabPanel(
          title = "Footers", 
          RM_tabLayout("reportTemplate_footer", 
                       add = FALSE, 
                       activate = FALSE)
          # radioButtons(inputId = "rdo_reportTemplate_footer", 
          #              label = "Footers to Include", 
          #              choices = "Initialization", 
          #              selected = character(0))
        )
      )
    ),
    tabPanel(
      title = "Signatures", 
      RM_tabLayout("templateSignature", 
                   add = FALSE, 
                   edit = TRUE, 
                   activate = FALSE)
    ),
    tabPanel(
      title = "Distribution"
    ),
    tabPanel(
      title = "Permissions"
    )
  )

# MODAL_REPORT_TEMPLATE ---------------------------------------------

MODAL_REPORT_TEMPLATE <- 
  bsModal(
    id = "modal_template_addEdit", 
    title = uiOutput("title_reportTemplateModal"), 
    size = "large", 
    trigger = "trg_none",
    fluidPage(
      column(
        width = 6,
        textInput(inputId = "txt_template_title", 
                  label = "Title"), 
        selectInput(inputId = "sel_template_directory", 
                    label = "Template Directory", 
                    choices = TEMPLATE_FOLDERS), 
        selectInput(inputId = "sel_template_file", 
                    label = "Primary Template File", 
                    choices = character(0)), 
        checkboxInput(inputId = "chk_template_isSignatureRequired", 
                      label = "Signature Required"), 
        checkboxInput(inputId = "chk_template_isActive", 
                      label = "Active", 
                      value = TRUE), 
        actionButton(inputId = "btn_template_addEdit", 
                     label = "Save")
      ), 
      column(
        width = 6, 
        selectInput(inputId = "sel_template_titleSize", 
                    label = "Title Size", 
                    choices = FONT_SIZES, 
                    selected = "LARGE"), 
        selectInput(inputId = "sel_template_logo", 
                    label = "Logo", 
                    choices = character(0)), 
        imageOutput("img_template_logo_preview")
      )
    )
  )

# MODAL_REPORT_TEMPLATE_SIGNATURE -----------------------------------

MODAL_REPORT_TEMPLATE_SIGNATURE <- 
  bsModal(
    id = "modal_templateSignature_edit", 
    title = "Edit Template Signatures", 
    trigger = "trg_none", 
    size = "large", 
    multiSelect(inputId = "templateSignature", 
                label = "Required Signatures", 
                choices = character(0), 
                selected = character(0), 
                size = 15,
                up_down = TRUE)
  )

# MODAL_REPORT_TEMPLATE_DISCLAIMER ----------------------------------

MODAL_REPORT_TEMPLATE_DISCLAIMER <- 
  bsModal(
    id = "modal_templateDisclaimer_edit", 
    title = "Assign Disclaimers to Template", 
    trigger = "trg_none", 
    size = "large", 
    checkboxGroupInput(inputId = "chkgrp_reportTemplate_disclaimer",
                       label = "Disclaimers to Include",
                       choices = "Initialization",
                       width = "100%"), 
    actionButton(inputId = "btn_reportTemplate_disclaimer_addEdit", 
                 label = "Save")
  )
