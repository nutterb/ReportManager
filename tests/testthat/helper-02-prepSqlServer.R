if (SQL_SERVER_READY){
  # Drop Existing Tables --------------------------------------------
  options(RM_sql_flavor = "sql_server")
  
  conn <- connectToReportManager()
  
  dropTable <- function(table, conn){
    tables <- DBI::dbListTables(conn, schema = "dbo")
    
    if (table %in% tables){
      result <- DBI::dbSendStatement(conn, 
                                     sprintf("DROP TABLE dbo.[%s]", 
                                             table))
      DBI::dbClearResult(result)
    }
  }
  
  dropTable("DateReportingFormatEvent", conn)
  dropTable("DateReportingFormat", conn)
  dropTable("ScheduleEvent", conn)
  dropTable("Schedule", conn)
  dropTable("UserRoleEvent", conn)
  dropTable("UserRole", conn)
  dropTable("RoleEvent", conn)
  dropTable("Role", conn)
  dropTable("UserEvent", conn)
  dropTable("User", conn)
  
  DBI::dbDisconnect(conn)
  
  # Rebuild the Database --------------------------------------------
  
  initializeReportManagerDatabase(system.file("Sql/SqlServer.sql", 
                                              package = "ReportManager"), 
                                  last_name = "Doe", 
                                  first_name = "Jane", 
                                  login_id = "jdoe", 
                                  email = "jdoe@domain.com")
}
