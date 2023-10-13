# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryFooter(oid = c(1, 2)), 
                 "'oid': Must have length <= 1")
    
    expect_error(queryFooter(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
  }
)

# Report Users for SQL Server ---------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "queryFooter works in SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    Footer <- queryFooter()
    
    expect_data_frame(Footer, 
                      ncols = 4)
  }
)


# Report Users for SQLite -------------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "queryFooter works in SQLite", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    Footer <- queryFooter()
    
    expect_data_frame(Footer, 
                      ncols = 4)
  }
)
