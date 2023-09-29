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
  
  dropTable("FileArchive", conn)
  dropTable("ReportTemplateEvent", conn)
  dropTable("ReportInstanceEvent", conn)
  dropTable("ReportInstance", conn)
  dropTable("ReportTemplateDisclaimerEvent", conn)
  dropTable("ReportTemplateFooterEvent", conn)
  dropTable("ReportTemplateScheduleEvent", conn)
  dropTable("ReportTemplateDisclaimer", conn)
  dropTable("ReportTemplateFooter", conn)
  dropTable("ReportTemplateSchedule", conn)
  dropTable("ReportTemplate", conn)
  dropTable("DisclaimerEvent", conn)
  dropTable("Disclaimer", conn)
  dropTable("FooterEvent", conn)
  dropTable("Footer", conn)
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
  
  initializeReportManagerDatabase(system.file("Sql/SQLite.sql", 
                                              package = "ReportManager"), 
                                  last_name = "Doe", 
                                  first_name = "Jane", 
                                  login_id = "jdoe", 
                                  email = "jdoe@domain.com")
  
}