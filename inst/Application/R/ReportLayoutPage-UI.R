# Disclaimer-Footer UI ----------------------------------------------

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

# Modal Logo Add ----------------------------------------------------

MODAL_LOGO_ADD <- 
  bsModal(
    id = "modal_logo_addEdit", 
    title = "Add a New Logo File", 
    size = "large", 
    trigger = "trg_none", 
    fluidRow(
      column(width = 6,
             fileInput(inputId = "file_logo_add", 
                       label = "Select a File"), 
             textInput(inputId = "txt_logo_add_fileName", 
                       label = "File Name"), 
             textInput(inputId = "txt_logo_add_description", 
                       label = "Description"),
             disabled(textInput(inputId = "txt_logo_add_extension", 
                                label = "File Extension")), 
             actionButton(inputId = "btn_logo_addEdit", 
                          label = "Save")
      ),
      column(width = 6, 
             imageOutput("img_logo_preview")
      )
    )
  )
