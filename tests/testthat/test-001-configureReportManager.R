# Argument Validation -----------------------------------------------

test_that(
  "configureReportManager Argument Validations", 
  {
    expect_error(configureReportManager(sql_flavor = c("not sql")), 
                 "'sql_flavor': Must be element of set")
    
    
    
    expect_error(configureReportManager(database_file = c("file1", "file2")), 
                 "'database_file': Must have length 1")
    expect_error(configureReportManager(database_file = 123), 
                 "'database_file': Must be of type 'character'")
    
    
    expect_error(configureReportManager(driver = c("file1", "file2")), 
                 "'driver': Must have length 1")
    expect_error(configureReportManager(driver = 123), 
                 "'driver': Must be of type 'character'")
    
    
    expect_error(configureReportManager(server = c("file1", "file2")), 
                 "'server': Must have length 1")
    expect_error(configureReportManager(server = 123), 
                 "'server': Must be of type 'character'")
    
    
    expect_error(configureReportManager(database = c("file1", "file2")), 
                 "'database': Must have length 1")
    expect_error(configureReportManager(database = 123), 
                 "'database': Must be of type 'character'")
  }
)

# Functionality -----------------------------------------------------

orig_opts <- options()[c("RM_sql_flavor", 
                         "RM_sqlite_file", 
                         "RM_sqlServer_driver", 
                         "RM_sqlServer_server", 
                         "RM_sqlServer_database")]

test_that(
  "Set the options", 
  {
    configureReportManager(sql_flavor = "sqlite", 
                           database_file = "filename", 
                           driver = "driver", 
                           server = "server", 
                           database = "database")
    
    expect_equal(getOption("RM_sql_flavor"), "sqlite")
    expect_equal(getOption("RM_sqlite_file"), "filename")
    expect_equal(getOption("RM_sqlServer_driver"), "driver")
    expect_equal(getOption("RM_sqlServer_server"), "server")
    expect_equal(getOption("RM_sqlServer_database"), "database")
  }
)

test_that(
  "Using NULL doesn't change the value", 
  {
    configureReportManager(sql_flavor = NULL, 
                           database_file = NULL, 
                           driver = NULL, 
                           server = NULL, 
                           database = NULL)
    
    expect_equal(getOption("RM_sql_flavor"), "sqlite")
    expect_equal(getOption("RM_sqlite_file"), "filename")
    expect_equal(getOption("RM_sqlServer_driver"), "driver")
    expect_equal(getOption("RM_sqlServer_server"), "server")
    expect_equal(getOption("RM_sqlServer_database"), "database")
  }
)

test_that(
  "Values change when provided", 
  {
    configureReportManager(sql_flavor = "sql_server", 
                           database_file = "filename2", 
                           driver = "driver2", 
                           server = "server2", 
                           database = "database2")
    
    expect_equal(getOption("RM_sql_flavor"), "sql_server")
    expect_equal(getOption("RM_sqlite_file"), "filename2")
    expect_equal(getOption("RM_sqlServer_driver"), "driver2")
    expect_equal(getOption("RM_sqlServer_server"), "server2")
    expect_equal(getOption("RM_sqlServer_database"), "database2")
  }
)

# Restore the original options

do.call(options, orig_opts)
