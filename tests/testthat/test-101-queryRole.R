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

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  test_that(
    "queryRole using SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
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
}
