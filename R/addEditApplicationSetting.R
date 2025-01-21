#' @name addEditApplicationSetting
#' @title Add or Edit Application Settings
#' 
#' @description Provides the user with tools to add or edit application 
#'   setting objects. 
#'   
#' @param oid `integerish(0/1)`. The OID of an application setting. If 
#'   length is zero, a new setting is added.
#' @param setting_key `character(1)`. The name of the setting key. Whitespace
#'   will be trimmed off the ends.
#' @param setting_value `character(1)`. The value for the setting. Whitespace
#'   will be trimmed off the end.
#' @param event_user `integerish(1)`. The OID of the user adding or editing
#'   the setting.
#' 
#' @export

addEditApplicationSetting <- function(oid = numeric(0), 
                                      setting_key, 
                                      setting_value, 
                                      event_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertCharacter(x = setting_key, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = setting_value, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertIntegerish(x = event_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  setting_key <- trimws(setting_key)
  setting_value <- trimws(setting_value)
  
  event_time <- Sys.time()
  
  AddEditData <- data.frame(SettingKey = setting_key, 
                            SettingValue = setting_value, 
                            stringsAsFactors = FALSE)
  
  EventList <- 
    data.frame(EventUser = rep(event_user, 2), 
               EventType = c("EditKey", "EditValue"), 
               EventDateTime = rep(format(event_time, 
                                          format = "%Y-%m-%d %H:%M:%S"), 2), 
               NewValue = c(setting_key, 
                            setting_value), 
               stringsAsFactors = FALSE)
  
  if (length(oid) == 0){
    OID <- insertRecord(AddEditData, 
                        table_name = "ApplicationSetting")
    EventList$ParentApplicationSetting <- rep(OID$OID, 
                                              nrow(EventList))
  } else {
    EventList <- .addEditApplicationSetting_editedEventList(EventList = EventList, 
                                                            oid = oid, 
                                                            conn = conn)
    if (nrow(EventList) > 0){
      updateRecord(data = AddEditData, 
                   where_data = data.frame(OID = oid), 
                   table_name = "ApplicationSetting")
    }
  }
  
  if (nrow(EventList)){
    insertRecord(EventList, 
                 table_name = "ApplicationSettingEvent", 
                 return_oid = FALSE)
  }
}

# Unexported --------------------------------------------------------

.addEditApplicationSetting_editedEventList <- function(EventList, 
                                                       oid, 
                                                       conn){
  EventList$ParentApplicationSetting <- rep(oid, 
                                            nrow(EventList))
  
  ThisApplicationSetting <- queryApplicationSetting(oid = oid)
  
  CurrentValue <- c(ThisApplicationSetting$SettingKey, 
                    ThisApplicationSetting$SettingValue)
  
  if (length(CurrentValue) > 0){
    EventList[compareValues(CurrentValue, EventList$NewValue), ]
  } else {
    EventList
  }
}
