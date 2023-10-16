if (SQL_SERVER_READY){
  # Drop Constraints Statement --------------------------------------
  statement <- "DECLARE @sql NVARCHAR(MAX);
  SET @sql = N'';
  
  SELECT @sql = @sql + N'
  ALTER TABLE ' + QUOTENAME(s.name) + N'.'
  + QUOTENAME(t.name) + N' DROP CONSTRAINT '
  + QUOTENAME(c.name) + ';'
  FROM sys.objects AS c
  INNER JOIN sys.tables AS t
  ON c.parent_object_id = t.[object_id]
  INNER JOIN sys.schemas AS s 
  ON t.[schema_id] = s.[schema_id]
  WHERE c.[type] IN ('D','C','F','PK','UQ')
  ORDER BY c.[type];
  
  EXEC sys.sp_executesql @sql;"
  
  # Drop Existing Tables --------------------------------------------
  options(RM_sql_flavor = "sql_server")
  
  conn <- connectToReportManager()
  
  res <- DBI::dbSendStatement(conn, statement)
  DBI::dbClearResult(res)
  
  dropTable <- function(table, conn){
    tables <- DBI::dbListTables(conn, schema = "dbo")
    
    if (table %in% tables){
      result <- DBI::dbSendStatement(conn, 
                                     sprintf("DROP TABLE dbo.[%s]", 
                                             table))
      DBI::dbClearResult(result)
    }
  }
  
  dropTable("ReportTemplateDistribution", conn)
  dropTable("ReportTemplateDistributionEvent", conn)
  dropTable("ReportTemplateSignature", conn)
  dropTable("ReportTemplateSignatureEvent", conn)
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
  dropTable("FileArchive", conn)
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
  
  initializeReportManagerDatabase(system.file("Sql/SqlServer.sql", 
                                              package = "ReportManager"), 
                                  last_name = "Doe", 
                                  first_name = "Jane", 
                                  login_id = "jdoe", 
                                  email = "jdoe@domain.com")
}
