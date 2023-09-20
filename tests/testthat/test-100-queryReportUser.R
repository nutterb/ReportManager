# Report Users for SQL Server ---------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "queryReportUser works in SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    ReportUser <- queryReportUser()
    
    expect_data_frame(ReportUser, 
                      ncols = 6)
  }
)


# Report Users for SQL Server ---------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "queryReportUser works in SQLite", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    ReportUser <- queryReportUser()
    
    expect_data_frame(ReportUser, 
                      ncols = 6)
  }
)
