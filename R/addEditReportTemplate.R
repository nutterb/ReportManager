#' @name addEditReportTemplate
#' @title Add or Edit ReportTemplate Objects
#' 
#' @description Enables the user to add ReportTemplate objects or edit
#'   existing objects. Only the properties stored directly on 
#'   the ReportTemplate object are edited with this function.
#'   
#' @param oid `integerish(0/1)`. The OID of the ReportTemplate object to be
#'   edited. Use `numeric(0)` to add a new object. 
#' @param template_directory `character(1)`. The directory in which the 
#'   report template files are stored. No more than 50 characters.
#' @param template_file `character(1)`. The primary template file that 
#'   directs the progress of the report template. No more than 50 characters.
#' @param title `character(1)`. The title to display on the report. No more
#'   than 200 characters.
#' @param title_size `character(1)`. The size of the font to use when 
#'   rendering the title. 
#' @param is_signature_required `logical(1)`. When `TRUE`, the template 
#'   requires signatures before it can be delivered to external users. 
#' @param is_active `logical(1)`. When `TRUE`, the template is flagged for
#'   active use.
#' @param logo_oid `integerish(0/1)`. The OID of the logo to display when
#'   rendering the title. 
#' @param event_user `integerish(1)`. The OID of the user performing the 
#'   action.
#'   
#' @export

addEditReportTemplate <- function(oid = numeric(0), 
                                  template_directory, 
                                  template_file, 
                                  title, 
                                  title_size, 
                                  is_signature_required = FALSE, 
                                  is_active = TRUE, 
                                  logo_oid = numeric(0), 
                                  event_user){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertString(x = template_directory, 
                          max.chars = 50, 
                          add = coll)
  
  checkmate::assertString(x = template_file, 
                          max.chars = 50, 
                          add = coll)
  
  checkmate::assertString(x = title, 
                          max.chars = 200, 
                          add = coll)
  
  title_size <- checkmate::matchArg(x = title_size, 
                                    choices = unname(FONT_SIZES), 
                                    .var.name = "title_size",
                                    add = coll)
  
  checkmate::assertLogical(x = is_signature_required, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_active, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = logo_oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  title <- trimws(title)
  
  event_time <- Sys.time()
  
  AddEditData <- data.frame(TemplateDirectory = template_directory, 
                            TemplateFile = template_file, 
                            Title = title, 
                            TitleSize = title_size, 
                            IsSignatureRequired = is_signature_required, 
                            IsActive = as.numeric(is_active), 
                            LogoFileArchive = logo_oid)
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 8), 
               EventType = c("Add", 
                             if (is_signature_required) "SetSignatureRequiredTrue" else "SetSignatureRequiredFalse", 
                             if (is_active) "Activate" else "Deactivate",
                             "EditTemplateFolder", 
                             "EditTemplateFile", 
                             "EditTitle", 
                             "EditTitleSize",
                             "EditLogoFile"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 8), 
               NewValue = c("", 
                            is_signature_required, 
                            is_active, 
                            template_directory, 
                            template_file, 
                            title, 
                            title_size, 
                            logo_oid))
  
  if (length(oid) == 0){
    
    OID <- insertRecord(AddEditData, 
                        table_name = "ReportTemplate")
    
    EventList$ParentReportTemplate <- rep(OID$OID, 
                                          nrow(EventList))
  } else {
    EventList <- .addEditReportTemplate_editedEventList(EventList = EventList,
                                                        oid       = oid,
                                                        conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "ReportTemplate")
    }
  }
  
  if (nrow(EventList)){
    insertRecord(EventList, 
                 table_name = "ReportTemplateEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditReportTemplate_editedEventList <- function(EventList, 
                                                   oid,
                                                   conn){
  EventList$ParentReportTemplate <- rep(oid, 
                                        nrow(EventList))
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisReportTemplate <- queryReportTemplate(oid)

  CurrentValue <- c(ThisReportTemplate$IsSignatureRequired, 
                    ThisReportTemplate$IsActive, 
                    ThisReportTemplate$TemplateDirectory, 
                    ThisReportTemplate$TemplateFile, 
                    ThisReportTemplate$Title, 
                    ThisReportTemplate$TitleSize, 
                    ThisReportTemplate$LogoFileArchive)
  
  EventList[CurrentValue != EventList$NewValue, ]
}
