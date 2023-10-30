# UI - Report Template Page -----------------------------------------
UI_TAB_REPORT_TEMPLATE_PAGE <- 
  tabsetPanel(
    id = "tabset_reportTemplate",
    type = "pills",
    # header = uiOutput("title_reportTemplateTabset"),
    tabPanel(
      title = "Template",
      RM_tabLayout("template")
    ),
    tabPanel(
      title = "Schedule", 
      actionButton(inputId = "btn_templateSchedule_edit", 
                   label = "Edit", 
                   style = "float:right;"),
      selectInput(inputId = "sel_templateSchedule", 
                           label = "Schedule", 
                           width = "300px", 
                           choices = character(0),
                           selected = character(0)),
      airDatepickerInput(inputId = "dttm_templateSchedule",
                                  label = "Schedule Start Time",
                                  timepicker = TRUE,
                                  value = Sys.Date()),
      disabled(actionButton(inputId = "btn_templateSchedule_save", 
                            label = "Save")),
    ),
    tabPanel(
      title = "Layout", 
      tabsetPanel(
        id = "tabset_reportTemplateLayout", 
        type = "pills", 
        tabPanel(
          title = "Disclaimers",
          RM_tabLayout("templateDisclaimer", 
                       add = FALSE, 
                       activate = FALSE)
        ),
        tabPanel(
          title = "Footers", 
          RM_tabLayout("templateFooter", 
                       add = FALSE, 
                       activate = FALSE)
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
      title = "Distribution", 
      RM_tabLayout("templateDistribution", 
                   add = FALSE, 
                   edit = TRUE, 
                   activate = FALSE)
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
        checkboxInput(inputId = "chk_template_includeTableOfContents", 
                      label = "Include Table of Contents"),
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
        textAreaInput(inputId = "txt_template_defaultEmailText", 
                      label = "Default E-mail Text", 
                      height = 200, 
                      width = 600),
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

# MODAL_REPORT_TEMPLATE_DISCLAIMER ----------------------------------

MODAL_REPORT_TEMPLATE_DISCLAIMER <- 
  bsModal(
    id = "modal_templateDisclaimer_edit", 
    title = "Assign Disclaimers to Template", 
    trigger = "trg_none", 
    size = "large", 
    multiSelect(inputId = "templateDisclaimer", 
                label = "Disclaimers", 
                choices = character(0), 
                selected = character(0), 
                size = 10,
                up_down = TRUE), 
    br(),
    br(),
    actionButton(inputId = "btn_templateDisclaimer_addEdit", 
                 label = "Save")
  )

# MODAL_REPORT_TEMPLATE_FootER --------------------------------------

MODAL_REPORT_TEMPLATE_FOOTER <- 
  bsModal(
    id = "modal_templateFooter_edit", 
    title = "Assign Footers to Template", 
    trigger = "trg_none", 
    size = "large", 
    multiSelect(inputId = "templateFooter", 
                label = "Footers", 
                choices = character(0), 
                selected = character(0), 
                size = 10,
                up_down = TRUE), 
    br(),
    br(),
    actionButton(inputId = "btn_templateFooter_addEdit", 
                 label = "Save")
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
                up_down = TRUE), 
    br(),
    br(),
    actionButton(inputId = "btn_templateSignature_addEdit", 
                 label = "Save")
  )

# MODAL_REPORT_TEMPLATE_DISTRIBUTION --------------------------------

MODAL_REPORT_TEMPLATE_DISTRIBUTION <- 
  bsModal(
    id = "modal_templateDistribution_edit", 
    title = "Edit Template Distribution", 
    trigger = "trg_none", 
    size = "large", 
    multiSelect(inputId = "templateDistribution", 
                label = "Recipients", 
                choices = character(0), 
                selected = character(0), 
                size = 15,
                up_down = TRUE), 
    br(),
    br(),
    actionButton(inputId = "btn_templateDistribution_addEdit", 
                 label = "Save")
  )
