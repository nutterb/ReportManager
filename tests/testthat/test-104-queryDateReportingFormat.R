# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryDateReportingFormat(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryDateReportingFormat(oid = 1:2), 
                 "'oid': Must have length <= 1")
  }
)

# Functionality - SQL Server ----------------------------------------

configureReportManager(flavor = "sql_server")

test_that(
  "Retrieving DateReportingFormat from SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    # Get all DateReportingFormat objects
    
    expect_data_frame(queryDateReportingFormat())
    
    # Get a single DateReportingFormat object
    expect_data_frame(queryDateReportingFormat(oid = 1), 
                      nrows = 1)
  }
)

# Functionality - SQLite --------------------------------------------

configureReportManager(flavor = "sqlite")

test_that(
  "Retrieving DateReportingFormat from SQLite", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    # Get all DateReportingFormat objects
    
    expect_data_frame(queryDateReportingFormat())
    
    # Get a single DateReportingFormat object
    expect_data_frame(queryDateReportingFormat(oid = 1), 
                      nrows = 1)
  }
)