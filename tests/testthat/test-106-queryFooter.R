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

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  test_that(
    "queryFooter works in SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      Footer <- queryFooter()
      
      expect_data_frame(Footer, 
                        ncols = 3)
    }
  )
}
