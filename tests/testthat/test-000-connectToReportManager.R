# SQL Server Connection ---------------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "Argument validations for driver, server, and database", 
  {
    expect_error(connectToReportManager_sqlServer(driver = c("file1", "file2")), 
                 "'driver': Must have length 1")
    
    expect_error(connectToReportManager_sqlServer(driver = 123), 
                 "'driver': Must be of type 'character'")
    
    
    expect_error(connectToReportManager_sqlServer(server = c("file1", "file2")), 
                 "'server': Must have length 1")
    
    expect_error(connectToReportManager_sqlServer(server = 123), 
                 "'server': Must be of type 'character'")
    
    
    expect_error(connectToReportManager_sqlServer(database = c("file1", "file2")), 
                 "'database': Must have length 1")
    
    expect_error(connectToReportManager_sqlServer(database = 123), 
                 "'database': Must be of type 'character'")
  }
)

test_that(
  "Test the connection to a SQL Server database", 
  {
    server_is_present <- TRUE
    test_connect <- 
      tryCatch(connectToReportManager_sqlServer(), 
               error = function(cond) server_is_present <<- FALSE)
    
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    expect_true("Microsoft SQL Server" %in% class(test_connect))
    
    DBI::dbDisconnect(test_connect)
  }
)


# SQLite Connection -------------------------------------------------

require(RSQLite)
options(RM_sql_flavor = "sqlite")

test_that(
  "Return an error when database_file is not character(1)", 
  {
    expect_error(connectToReportManager_sqlite(c("file1", "file2")), 
                 "'database_file': Must have length 1")
    
    expect_error(connectToReportManager_sqlite(123), 
                 "'database_file': Must be of type 'character'")
  }
)

test_that(
  "Test the connection to a SQLite Database", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)

    test_connect <- connectToReportManager_sqlite()
   
    expect_true("SQLiteConnection" %in% class(test_connect))
   
    DBI::dbDisconnect(test_connect)
  }
)

detach("package:RSQLite", unload=TRUE)
