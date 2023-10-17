# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not length(0/1)", 
  {
    expect_error(queryReportTemplate(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryReportTemplate(oid = 1:2), 
                 "'oid': Must have length <= 1")
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "Retrieve ReportTemplate configurations", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    Template <- queryReportTemplate()
    
    expect_data_frame(Template)
    expect_true(nrow(Template) > 0)
    
    expect_data_frame(queryReportTemplate(1), 
                      nrows = 1)
  }
)

# Functionality - SQLite --------------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "Retrieve ReportTemplate configurations", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    Template <- queryReportTemplate()
    
    expect_data_frame(Template)
    expect_true(nrow(Template) > 0)
    
    expect_data_frame(queryReportTemplate(1), 
                      nrows = 1)
  }
)
