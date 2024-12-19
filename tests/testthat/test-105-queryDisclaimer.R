# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryDisclaimer(oid = c(1, 2)), 
                 "'oid': Must have length <= 1")
    
    expect_error(queryDisclaimer(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
  }
)

# Report Users for SQL Server ---------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }

  test_that(
    "queryDisclaimer works in SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      Disclaimer <- queryDisclaimer()
      
      expect_data_frame(Disclaimer, 
                        ncols = 3)
    }
  )
}