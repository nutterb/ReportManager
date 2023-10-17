# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(queryRole(oid = c(1, 2)), 
                 "'oid': Must have length <= 1")
    
    expect_error(queryRole(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "queryRole using SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    Role <- queryRole()
    
    expect_data_frame(Role, 
                      ncols = 4, 
                      nrows = 5)
    
    Role <- queryRole(oid = 1)
    
    expect_data_frame(Role, 
                      ncols = 4, 
                      nrows = 1)
  }
)

# Functionality - Sqlite --------------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "queryRole using SQLite", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    Role <- queryRole()
    
    expect_data_frame(Role, 
                      ncols = 4, 
                      nrows = 5)
    
    Role <- queryRole(oid = 1)
    
    expect_data_frame(Role, 
                      ncols = 4, 
                      nrows = 1)
  }
)
