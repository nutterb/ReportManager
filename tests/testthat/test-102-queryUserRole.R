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

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }

  test_that(
    "queryUserRole returns intended results", 
    {
      skip_if_not(.ready, 
                  .message)
      
      UserRole <- queryUserRole()
      
      expect_data_frame(UserRole, 
                        nrows = 5)
      
      UserRole_UserAdmin <- queryUserRole(role_oid = 1)
      expect_data_frame(UserRole_UserAdmin, 
                        nrows = 1)
      
      UserRole_Jdoe <- queryUserRole(user_oid = 1)
      expect_data_frame(UserRole_Jdoe, 
                        nrows = 4)
      
      
      UserRole_Dual <- queryUserRole(user_oid = 1, 
                                     role_oid = 2)
      expect_data_frame(UserRole_Dual, 
                        nrows = 1)
    }
  )
}
