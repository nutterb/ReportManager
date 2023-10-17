# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryFileArchive(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryFileArchive(oid = 1:2), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(0/1)", 
  {
    expect_error(queryFileArchive(parent_report_template = "1"), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(queryFileArchive(parent_report_template = 1:2), 
                 "'parent_report_template': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_instance is not integerish(0/1)", 
  {
    expect_error(queryFileArchive(parent_report_instance = "1"), 
                 "'parent_report_instance': Must be of type 'integerish'")
    
    expect_error(queryFileArchive(parent_report_instance = 1:2), 
                 "'parent_report_instance': Must have length <= 1")
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "query FileArchive under SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    FileArchive <- queryFileArchive()
    expect_data_frame(FileArchive)
    expect_true(nrow(FileArchive) > 0)
    
    FileArchive <- queryFileArchive(oid = 1)
    expect_data_frame(FileArchive, 
                      nrows = 1)
    
    FileArchive <- queryFileArchive(parent_report_template = 1)
    expect_data_frame(FileArchive)
    expect_true(nrow(FileArchive) > 0)
    
    FileArchive <- queryFileArchive(parent_report_instance = 1)
    expect_data_frame(FileArchive)
    expect_true(nrow(FileArchive) > 0)
    
    FileArchive <- queryFileArchive(parent_report_template = 1, 
                                    parent_report_instance = 1)
    expect_data_frame(FileArchive)
    expect_true(nrow(FileArchive) > 0)
  }
)


# Functionality - SQLite --------------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "query FileArchive under SQLite", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    FileArchive <- queryFileArchive()
    expect_data_frame(FileArchive)
    expect_true(nrow(FileArchive) > 0)
    
    FileArchive <- queryFileArchive(oid = 1)
    expect_data_frame(FileArchive, 
                      nrows = 1)
    
    FileArchive <- queryFileArchive(parent_report_template = 1)
    expect_data_frame(FileArchive)
    expect_true(nrow(FileArchive) > 0)

    FileArchive <- queryFileArchive(parent_report_instance = 1)
    expect_data_frame(FileArchive)
    expect_true(nrow(FileArchive) > 0)
    
    FileArchive <- queryFileArchive(parent_report_template = 1, 
                                    parent_report_instance = 1)
    expect_data_frame(FileArchive)
    expect_true(nrow(FileArchive) > 0)
  }
)
