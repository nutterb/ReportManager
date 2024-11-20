# Argument Validation -----------------------------------------------

test_that(
  "Return an error when report_template_oid is not integerish(1)", 
  {
    expect_error(
      updateReportInstanceSchedule(report_template_oid = "1", 
                                   event_user = 1), 
      "'report_template_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      updateReportInstanceSchedule(report_template_oid = 1:2, 
                                   event_user = 1), 
      "'report_template_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error when event_user is not integerish(1)", 
  {
    expect_error(
      updateReportInstanceSchedule(report_template_oid = 1, 
                                   event_user = "1"), 
      "'event_user': Must be of type 'integerish'"
    )
    
    expect_error(
      updateReportInstanceSchedule(report_template_oid = 1, 
                                   event_user = 1:2), 
      "'event_user': Must have length 1"
    )
  }
)


# Functionality -------------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
    purgeReportManagerDatabase()
    initializeUiTestingDatabase(SQL_FILE[flavor])
  }
  
  
  test_that(
    "Schedules are written to the database", 
    {
      Start <- queryReportInstance(report_template_oid = 2)
      expect_data_frame(Start, 
                        nrows = 0)
      
      updateReportInstanceSchedule(report_template_oid = 2, 
                                   event_user = 1)
      
      End <- queryReportInstance(report_template_oid = 2)
      expect_data_frame(End)
      expect_true(nrow(End) > 0)
    }
  )
  
}
