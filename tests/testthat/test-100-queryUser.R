# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryUser(oid = c(1, 2)), 
                 "'oid': Must have length <= 1")
    
    expect_error(queryUser(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
  }
)

# Report Users Functionality ----------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
    purgeReportManagerDatabase()
    initializeReportManagerDatabase(SQL_FILE[flavor], 
                                    last_name = "Doe", 
                                    first_name = "Jane", 
                                    login_id = "jdoe", 
                                    email = "jdoe@domain.com")
  }
  
  test_that(
    "queryUser works in SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      User <- queryUser()
      
      expect_data_frame(User, 
                        ncols = 7)
    }
  )
}
