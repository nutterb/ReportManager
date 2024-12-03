# Argument Validation -----------------------------------------------

test_that(
  "Return an error when report_instance_oid is not integerish(1)",
  {
    expect_error(
      queryReportInstanceNarrative(report_instance_oid = "1"),
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      queryReportInstanceNarrative(report_instance_oid = 1:2),
      "'report_instance_oid': Must have length 1"
    )
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
    "Returns a data frame with a single row", 
    {
      Narrative <- queryReportInstanceNarrative(report_instance_oid = 1)
      
      expect_data_frame(Narrative, 
                        nrows = 1)
    }
  )
}