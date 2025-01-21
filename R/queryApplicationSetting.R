#' @name queryApplicationSetting
#' @title Query Application Settings from the Database
#' 
#' @description Provides the user with an interface to retrieve data about 
#'   application settings. One function allows retrieval by OID, 
#'   the other by the SettingKey value.
#'   
#' @param oid `integerish(0/1)`. The OID of the setting to be retrieved. 
#'   When length is 0, all settings are returned.
#' @param setting_key `character(1)`. The setting key value for an 
#'   application setting.
#'   
#' @export

queryApplicationSetting <- function(oid = numeric(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .queryApplicationSetting_sqlite, 
                      "sql_server" = .queryApplicationSetting_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  if (length(oid) > 0){
    statement <- paste0(statement, " WHERE [OID] = ?")
  }
  
  param_list <- list(oid)
  param_list <- param_list[lengths(param_list) > 0]
  
  ApplicationSetting <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  ApplicationSetting
}

#' @rdname queryApplicationSetting
#' @export

queryApplicationSettingByKey <- function(setting_key){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = setting_key, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .queryApplicationSetting_sqlite, 
                      "sql_server" = .queryApplicationSetting_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  statement <- paste0(statement, 
                      " WHERE SettingKey = ?")
  
  
  ApplicationSetting <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        setting_key))
  
  ApplicationSetting
}

# Unexported --------------------------------------------------------

.queryApplicationSetting_sqlite <- 
  "SELECT [OID], 
      [SettingKey],
      [SettingValue]
    FROM [ApplicationSetting]"

.queryApplicationSetting_sqlServer <- 
  "SELECT [OID], 
      [SettingKey],
      [SettingValue]
    FROM dbo.[ApplicationSetting]"


