# Drop Existing Tables ----------------------------------------------
conn <- connectToReportManager_sqlServer()

tables <- dbListTables(conn, schema = "dbo")

dropTable <- function(table, conn){
  result <- dbSendStatement(conn, 
                            sprintf("DROP TABLE dbo.%s", 
                                    table))
  dbClearResult(result)
}

dropTable("ReportUserEvent", conn)
dropTable("ReportUser", conn)

dbDisconnect(conn)

# Rebuild the Database ----------------------------------------------

connectToReportManager <- connectToReportManager_sqlServer

initializeReportManagerDatabase(system.file("Sql/SqlServer.sql", 
                                            package = "ReportManager"))
