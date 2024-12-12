#' @name addEditReportInstanceDistribution
#' @title Add or Edit ReportInstanceDistribution Objects
#' 
#' @description Enables the user to add ReportInstanceDistribution objects 
#'   or edit an existing ReportInstanceDistribution object. Also manages the
#'   recording of events associated with the action. 
#'   
#' @param oid `integerish(0/1)`. The OID of the ReportInstanceDistribution 
#'   object to be edited. Use `numeric(0)` to add a new object.
#' @param parent_report_instance `integerish(1)`. The OID of the ReportInstance
#'   object being associated with a disclaimer. 
#' @param parent_user `integerish(0/1)`. The OID of the User object being 
#'   designated as a recipient. Only one of `parent_user` or `parent_role`
#'   may be given.
#' @param parent_role `integerish(0/1)`. The OID of the Role object being 
#'   designated as a recipient. Only one of `parent_user` or `parent_role`
#'   may be given.
#' @param order `integerish(1)`. The ordered position in which the 
#'   signature is displayed on the report.
#' @param is_active `logical(1)`. When `TRUE`, the association will be marked
#'   as active. 
#' @param event_user `integerish(1)`. The OID of the User performing the action.
#' 
#' @export

addEditReportInstanceDistribution <- function(oid = numeric(0), 
                                              parent_report_instance, 
                                              parent_user = numeric(0),
                                              parent_role = numeric(0),
                                              is_active = TRUE, 
                                              event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_instance, 
                              len = 1,
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_user, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_role, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = is_active, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (length(parent_user) > 0 && length(parent_role) > 0){
    coll$push("Only one of 'parent_user' or 'parent_role' may be provided.")
  }
  
  checkmate::reportAssertions(coll)
  
  if (length(oid) == 0 && length(parent_user) > 0){
    CurrentReportInstanceDistribution <- 
      queryReportInstanceDistribution(parent_report_instance = parent_report_instance, 
                                      parent_user = parent_user)
    
    if (nrow(CurrentReportInstanceDistribution) > 0){
      coll$push(sprintf("A ReportInstanceDistribution record for ReportInstance.OID = %s and User.OID = %s already exists. Edit the exisitng record instead of adding another", 
                        parent_report_instance, 
                        parent_user))
    }
  }

  if (length(oid) == 0 && length(parent_role) > 0){
    CurrentReportInstanceDistribution <- 
      queryReportInstanceDistribution(parent_report_instance = parent_report_instance, 
                                      parent_role = parent_role)
    
    if (nrow(CurrentReportInstanceDistribution) > 0){
      coll$push(sprintf("A ReportInstanceDistribution record for ReportInstance.OID = %s and Role.OID = %s already exists. Edit the exisitng record instead of adding another", 
                        parent_report_instance, 
                        parent_role))
    }
  }  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  AddEditData <- data.frame(ParentReportInstance = parent_report_instance, 
                            ParentUser = if (length(parent_user) == 0) NA_real_ else parent_user,
                            ParentRole = if (length(parent_role) == 0) NA_real_ else parent_role,
                            IsActive   = is_active)
  
  event_time <- Sys.time()
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 2), 
               EventType = c("Add", 
                             if (is_active) "Activate" else "Deactivate"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 2), 
               NewValue = c("", 
                            is_active), 
               stringsAsFactors = FALSE)
  
  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "ReportInstanceDistribution", 
                        return_oid = TRUE)
    
    EventList$ParentReportInstanceDistribution <- rep(OID$OID, 
                                                      nrow(EventList))
  } else {
    EventList <- .addEditReportInstanceDistribution_editedEventList(EventList = EventList,
                                                                    oid       = oid,
                                                                    conn      = conn)
    
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "ReportInstanceDistribution")      
    }
  }
  
  if (nrow(EventList) > 0){
    insertRecord(EventList, 
                 table_name = "ReportInstanceDistributionEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditReportInstanceDistribution_editedEventList <- function(EventList, 
                                                               oid,
                                                               conn){
  EventList$ParentReportInstanceDistribution <- rep(oid, 
                                                    nrow(EventList))
  
  EventList <- EventList[!EventList$EventType == "Add", ]
  ThisRID <- queryReportInstanceDistribution(oid)
  
  CurrentValue <- c(ThisRID$IsActive)
  
  EventList[compareValues(CurrentValue, EventList$NewValue), ]
}
