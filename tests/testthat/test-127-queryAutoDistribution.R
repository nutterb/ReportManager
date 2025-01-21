# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(
      queryAutoDistribution(oid = "1"), 
      "'oid': Must be of type 'integerish'")
    
    expect_error(
      queryAutoDistribution(oid = 1:2), 
      "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if report_template_oid is not integerish(0/1)", 
  {
    expect_error(
      queryAutoDistribution(report_template_oid = "1"), 
      "'report_template_oid': Must be of type 'integerish'")
    
    expect_error(
      queryAutoDistribution(report_template_oid = 1:2), 
      "'report_template_oid': Must have length <= 1")
  }
)

# Functionality -----------------------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  test_that(
    "Return the appropriate data frames", 
    {
      skip_if_not(.ready, 
                  .message)
      
      All <- queryAutoDistribution()
      
      expect_data_frame(All, 
                        nrows = 3)
      
      expect_data_frame(queryAutoDistribution(oid = 1), 
                        nrows = 1)
      
      expect_data_frame(queryAutoDistribution(report_template_oid = 1), 
                        nrows = 3)
    }
  )
}
