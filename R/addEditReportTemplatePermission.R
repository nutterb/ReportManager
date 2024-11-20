#' @name addEditReportTemplatePermission
#' @title Add or Edit ReportTemplatePermission Objects
#' 
#' @description Enables the user to add ReportTemplatePermission objects 
#'   or edit an existing ReportTemplateDisclaimer object. Also manages the
#'   recording of events associated with the action. 
#'   
#' @param oid `integerish(0/1)`. The OID of the ReportTemplateDisclaimer 
#'   object to be edited. Use `numeric(0)` to add a new object.
#' @param parent_report_template `integerish(0/1)`. The OID of the ReportTemplate
#'   object being associated with a permission. 
#' @param parent_role `integerish(0/1)`. The OID of the Permission
#'   object being associated with the ReportTemplate.
#' @param is_active `logical(1)`. When `TRUE`, the association will be marked
#'   as active. 
#' @param event_user `integerish(1)`. The OID of the User performing the action.
#' 
#' @export

addEditReportTemplatePermission <- function(oid = numeric(0), 
                                            parent_report_template = numeric(0), 
                                            parent_role = numeric(0), 
                                            can_view, 
                                            can_add_notes, 
                                            can_edit_narrative, 
                                            can_submit, 
                                            can_start_revision,
                                            is_active, 
                                            event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_template, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_role, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = can_view, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(can_add_notes, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = can_edit_narrative, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = can_submit, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = can_start_revision, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_active, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (length(oid) == 0){
    CurrentReportTemplatePermission <- 
      queryReportTemplatePermission(parent_report_template = parent_report_template, 
                                    parent_role = parent_role)
    
    if (nrow(CurrentReportTemplatePermission) > 0){
      coll$push(sprintf("A ReportTemplatePermission record for ReportTemplate.OID = %s and Role.OID = %s already exists. Edit the exisitng record instead of adding another", 
                        parent_report_template, 
                        parent_role))
    }
  }
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  AddEditData <- data.frame(ParentReportTemplate = parent_report_template, 
                            ParentRole = parent_role,
                            CanView = can_view, 
                            CanAddNotes = can_add_notes, 
                            CanEditNarrative = can_edit_narrative, 
                            CanSubmit = can_submit, 
                            CanStartRevision = can_start_revision,
                            IsActive = is_active)
  
  event_time <- Sys.time()
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 7), 
               EventType = c("SetCanView", 
                             "SetCanAddNotes", 
                             "SetCanEditNarrative", 
                             "SetCanSubmit", 
                             "SetCanStartRevision", 
                             if (is_active) "Activate" else "Deactivate", 
                             "Add"), 
               EventDateTime = rep(event_time, 7), 
               NewValue = c(can_view, 
                            can_add_notes, 
                            can_edit_narrative,
                            can_submit, 
                            can_start_revision, 
                            is_active, 
                            "Add"), 
               stringsAsFactors = FALSE)
  
  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "ReportTemplatePermission", 
                        return_oid = TRUE)
    EventList$ParentReportTemplatePermission <- rep(OID$OID,
                                                    nrow(EventList))
  } else {
    EventList <- .addEditReportTemplatePermission_editedEventList(EventList = EventList,
                                                                  oid       = oid,
                                                                  conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "ReportTemplatePermission")      
    }
  }
  
  if (nrow(EventList) > 0){
    insertRecord(EventList, 
                 table_name = "ReportTemplatePermissionEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditReportTemplatePermission_editedEventList <- function(EventList, 
                                                             oid,
                                                             conn){
  EventList$ParentReportTemplatePermission <- rep(oid, 
                                                  nrow(EventList))
  
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisRTP <- queryReportTemplatePermission(oid)
  
  CurrentValue <- c(ThisRTP$CanView, 
                    ThisRTP$CanAddNotes,
                    ThisRTP$CanEditNarrative,
                    ThisRTP$CanSubmit,
                    ThisRTP$CanStartRevision,
                    ThisRTP$IsActive)
  
  EventList[compareValues(CurrentValue, EventList$NewValue), ]
}
