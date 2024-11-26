# Argument Validation -----------------------------------------------

test_that(
  "Return an error when report_instance_oid is not integerish(1)",
  {
    expect_error(
      addEditReportInstanceNarrative(report_instance_oid = "1", 
                                     narrative = "text", 
                                     event_user = 1),
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addEditReportInstanceNarrative(report_instance_oid = 1:2, 
                                     narrative = "text", 
                                     event_user = 1),
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error when narrative is not character(1)",
  {
    expect_error(
      addEditReportInstanceNarrative(report_instance_oid = 1, 
                                     narrative = 1, 
                                     event_user = 1),
      "'narrative': Must be of type 'character'"
    )
    
    expect_error(
      addEditReportInstanceNarrative(report_instance_oid = 1, 
                                     narrative = letters, 
                                     event_user = 1),
      "'narrative': Must have length 1"
    )
  }
)

test_that(
  "Return an error when event_user is not integerish(1)",
  {
    expect_error(
      addEditReportInstanceNarrative(report_instance_oid = 1, 
                                     narrative = "text", 
                                     event_user = "1"),
      "'event_user': Must be of type 'integerish'"
    )
    
    expect_error(
      addEditReportInstanceNarrative(report_instance_oid = 1, 
                                     narrative = "text", 
                                     event_user = 1:2),
      "'event_user': Must have length 1"
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
    "Adding a Narrative functionality",
    {
      CurrentNarrative <- queryReportInstanceNarrative(report_instance_oid = 1)
      
      addEditReportInstanceNarrative(report_instance_oid = 1, 
                                     narrative = "sample text", 
                                     event_user = 1)
      
      NewNarrative <- queryReportInstanceNarrative(report_instance_oid = 1)
      
      expect_data_frame(NewNarrative, 
                        nrows = nrow(CurrentNarrative) + 1)
      
      expect_equal(NewNarrative$Narrative, 
                   "sample text")
    }
  )
  
  test_that(
    "Editing a Narrative does not create a new row",
    {
      CurrentNarrative <- queryReportInstanceNarrative(report_instance_oid = 1)
      
      expect_data_frame(CurrentNarrative, 
                        nrows = 1)
      
      addEditReportInstanceNarrative(report_instance_oid = 1, 
                                     narrative = "edited text", 
                                     event_user = 1)
      
      NewNarrative <- queryReportInstanceNarrative(report_instance_oid = 1)
      
      expect_data_frame(NewNarrative, 
                        nrows = 1)
    }
  )
}