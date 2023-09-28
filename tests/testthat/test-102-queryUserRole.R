# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not numeric(0/1)", 
  {
    expect_error(queryUserRole(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryUserRole(oid = 1:2), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if user_oid is not numeric(0/1)", 
  {
    expect_error(queryUserRole(user_oid = "1"), 
                 "'user_oid': Must be of type 'integerish'")
    
    expect_error(queryUserRole(user_oid = 1:2), 
                 "'user_oid': Must have length <= 1")
  }
)


test_that(
  "Return an error if role_oid is not numeric(0/1)", 
  {
    expect_error(queryUserRole(role_oid = "1"), 
                 "'role_oid': Must be of type 'integerish'")
    
    expect_error(queryUserRole(role_oid = 1:2), 
                 "'role_oid': Must have length <= 1")
  }
)

# Functionality - SQL Server ----------------------------------------

configureReportManager(flavor = "sql_server")

test_that(
  "queryUserRole returns intended results", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    UserRole <- queryUserRole()
    
    expect_data_frame(UserRole, 
                      nrows = 3)
    
    UserRole_UserAdmin <- queryUserRole(role_oid = 1)
    expect_data_frame(UserRole_UserAdmin, 
                      nrows = 1)
    
    UserRole_Jdoe <- queryUserRole(user_oid = 1)
    expect_data_frame(UserRole_Jdoe, 
                      nrows = 3)
    
    
    UserRole_Dual <- queryUserRole(user_oid = 1, 
                                   role_oid = 2)
    expect_data_frame(UserRole_Dual, 
                      nrows = 1)
  }
)

# Functionality - SQLite --------------------------------------------

configureReportManager(flavor = "sqlite")

test_that(
  "queryUserRole returns intended results", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    UserRole <- queryUserRole()
    
    expect_data_frame(UserRole, 
                      nrows = 3)
    
    UserRole_UserAdmin <- queryUserRole(role_oid = 1)
    expect_data_frame(UserRole_UserAdmin, 
                      nrows = 1)
    
    UserRole_Jdoe <- queryUserRole(user_oid = 1)
    expect_data_frame(UserRole_Jdoe, 
                      nrows = 3)
    
    
    UserRole_Dual <- queryUserRole(user_oid = 1, 
                                   role_oid = 2)
    expect_data_frame(UserRole_Dual, 
                      nrows = 1)
  }
)
