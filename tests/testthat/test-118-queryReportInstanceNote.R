# Argument Valdiation -----------------------------------------------

test_that(
  "report_instance_oid is numeric(1)", 
  {
    expect_error(queryReportInstanceNote(report_instance_oid = "1"), 
                 "'report_instance_oid': Must be of type 'integerish'")
    
    expect_error(queryReportInstanceNote(report_instance_oid = 1:2), 
                 "'report_instance_oid': Must have length 1")
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

      # Add an extra record just so there are more than 1
      addReportInstanceNote(report_instance_oid = 1, 
                            parent_user = 1, 
                            note = "Another note for additional testing")
      
      Notes <- queryReportInstanceNote(report_instance_oid = 1)
      
      expect_data_frame(Notes)
      
      expect_true(nrow(Notes) > 1)
    }
  )
}
