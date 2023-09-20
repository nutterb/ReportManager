if (SQLITE_READY){
  # Drop Existing Tables --------------------------------------------
  require(RSQLite)
  options(RM_sql_flavor = "sqlite")
  conn <- connectToReportManager()
  
  dropTable <- function(table, conn){
    tables <- DBI::dbListTables(conn)
    
    if (table %in% tables){
      result <- DBI::dbSendStatement(conn, 
                                     sprintf("DROP TABLE %s", 
                                             table))
      DBI::dbClearResult(result)
    }
  }
  
  dropTable("ReportUserEvent", conn)
  dropTable("ReportUser", conn)
  
  DBI::dbDisconnect(conn)
  
  # Rebuild the Database --------------------------------------------
  
  initializeReportManagerDatabase(system.file("Sql/SQLite.sql", 
                                              package = "ReportManager"))
  
}