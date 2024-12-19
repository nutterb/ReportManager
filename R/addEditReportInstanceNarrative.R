#' @name addEditReportInstanceNarrative
#' @title Add and Edit ReportInstanceNarrative Records
#' 
#' @description Provides the user with the tools to add or edit the narrative
#'   associated with a report instance. 
#'   
#' @param report_instance_oid `integerish(0/1)`. The OID of the report 
#'   instance to be for which the narrative is to be edited. 
#' @param narrative `character(1)`. The text of the narrative. Whitespace
#'   will be trimmed before sending to the database. If, after trimming, 
#'   the text is an empty string, it will still be sent to the database as
#'   that may be a valid edit. 
#' @param event_user `integerish(1)`. The OID of the user making changes to 
#'   the narrative.
#'   
#' @details All changes to the narrative are recorded as an edit. The 
#'   ReportInstanceNarrativeEvent table records any save events, even if no
#'   actual changes are made.
#'   
#' @export

addEditReportInstanceNarrative <- function(report_instance_oid, 
                                           narrative, 
                                           event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertCharacter(x = narrative, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  narrative <- trimws(narrative)
  
  event_time <- Sys.time()
  
  CurrentNarrative <- queryReportInstanceNarrative(report_instance_oid = report_instance_oid)
  
  AddEditData <- data.frame(ParentReportInstance = report_instance_oid, 
                            Narrative = narrative)
  
  EventList <- data.frame(EventUser = event_user, 
                          EventDateTime = event_time, 
                          NewValue = narrative)
  
  
  if (nrow(CurrentNarrative) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "ReportInstanceNarrative")
    
    EventList$ParentReportInstanceNarrative <- OID$OID
  } else {
    AddEditData$ParentReportInstance <- NULL
    updateRecord(data = AddEditData, 
                 where_data = data.frame(ParentReportInstance = report_instance_oid), 
                 table_name = "ReportInstanceNarrative")
    EventList$ParentReportInstanceNarrative <- CurrentNarrative$OID
  }
  
  if (nrow(EventList)){
    insertRecord(EventList, 
                 table_name = "ReportInstanceNarrativeEvent", 
                 return_oid = FALSE)
  }
  
}
