# SQLite Connection -------------------------------------------------

require(RSQLite)
connectToReportManager <- connectToReportManager_sqlite

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
    skip_if(!requireNamespace("RSQLite", quietly=TRUE), 
            "`RSQLite` is required to test SQLite connections")
    
    temp_file <- tempfile()
    test_connect <- connectToReportManager(temp_file)
    
    expect_class(test_connect, "SQLiteConnection")
    
    DBI::dbDisconnect(test_connect)
    file.remove(temp_file)
  }
)

detach("package:RSQLite", unload=TRUE)
