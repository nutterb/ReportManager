# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(1)", 
  {
    expect_error(queryFromFileArchive(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryFromFileArchive(oid = 1:2), 
                 "'oid': Must have length 1")
  }
)

test_that(
  "Return an error if filedir is not character(0/1)", 
  {
    expect_error(queryFromFileArchive(oid = 1, 
                                      file_dir = 123), 
                 "'file_dir': Must be of type 'character'")
    
    expect_error(queryFromFileArchive(oid = 1, 
                                      file_dir = letters), 
                 "'file_dir': Must have length <= 1")
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "Get a file without saving", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    File <- queryFromFileArchive(oid = 1)
    
    expect_data_frame(File, 
                      nrows = 1)
    
    expect_false("SavedTo" %in% names(File))
  }
)

test_that(
  "Get a file and save", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    temp_dir <- tempdir()
    File <- queryFromFileArchive(oid = 1, 
                                 file_dir = temp_dir)
    
    expect_data_frame(File, 
                      nrows = 1)
    
    expect_true("SavedTo" %in% names(File))
    expect_true(file.exists(File$SavedTo))
  }
)

# Functionality - SQLite --------------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "Get a file without saving", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    File <- queryFromFileArchive(oid = 1)
    
    expect_data_frame(File, 
                      nrows = 1)
    
    expect_false("SavedTo" %in% names(File))
  }
)

test_that(
  "Get a file and save", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    temp_dir <- tempdir()
    File <- queryFromFileArchive(oid = 1, 
                                 file_dir = temp_dir)
    
    expect_data_frame(File, 
                      nrows = 1)
    
    expect_true("SavedTo" %in% names(File))
    expect_true(file.exists(File$SavedTo))
  }
)
