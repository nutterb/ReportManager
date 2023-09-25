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
  
  dropTable("UserRoleEvent", conn)
  dropTable("UserRole", conn)
  dropTable("UserEvent", conn)
  dropTable("User", conn)
  dropTable("RoleEvent", conn)
  dropTable("Role", conn)
  
  DBI::dbDisconnect(conn)
  
  # Rebuild the Database --------------------------------------------
  
  initializeReportManagerDatabase(system.file("Sql/SQLite.sql", 
                                              package = "ReportManager"), 
                                  last_name = "Doe", 
                                  first_name = "Jane", 
                                  login_id = "jdoe", 
                                  email = "jdoe@domain.com")
  
}