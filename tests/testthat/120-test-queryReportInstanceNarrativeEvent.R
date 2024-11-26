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
    "Returns a data frame", 
    {
      Narrative <- queryReportInstanceNarrativeEvent(report_instance_oid = 1)
      
      expect_data_frame(Narrative)
      
      addEditReportInstanceNarrative(report_instance_oid = 1, 
                                     narrative = "Another Text", 
                                     event_user = 1)
      
      NewNarrative <- queryReportInstanceNarrativeEvent(report_instance_oid = 1)
      
      expect_data_frame(NewNarrative, 
                        nrows = nrow(Narrative) + 1)
    }
  )
}