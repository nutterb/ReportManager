# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateSchedule(oid = "1", 
                                               parent_report_template = 1, 
                                               parent_schedule = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateSchedule(oid = 1:2, 
                                               parent_report_template = 1, 
                                               parent_schedule = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateSchedule(oid = 1, 
                                               parent_report_template = "1", 
                                               parent_schedule = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateSchedule(oid = 1, 
                                               parent_report_template = 1:2, 
                                               parent_schedule = 1), 
                 "'parent_report_template': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_schedule is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateSchedule(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_schedule = "1"), 
                 "'parent_schedule': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateSchedule(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_schedule = 1:2), 
                 "'parent_schedule': Must have length <= 1")
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
    "Return the appropriate data frames", 
    {
      skip_if_not(.ready, 
                  .message)
      
      # Add an extra record just so there are more than 1
      
      addEditReportTemplateSchedule(parent_report_template = 2, 
                                    parent_schedule = 4, 
                                    start_date = Sys.time(),
                                    event_user = 1)
      
      Test <- queryReportTemplateSchedule()
      
      expect_data_frame(Test)
      expect_true(nrow(Test) > 0)
      
      
      Test <- queryReportTemplateSchedule(oid = 1)
      expect_data_frame(Test, 
                        nrows = 1)
      expect_equal(Test$OID, 1)
      
      
      Test <- queryReportTemplateSchedule(parent_report_template = 1)
      expect_data_frame(Test)
      expect_true(all(Test$ParentReportSchedule == 1))
      
      
      Test <- queryReportTemplateSchedule(parent_schedule = 4)
      expect_data_frame(Test)
      expect_true(all(Test$ParentSchedule == 4))
    }
  )
}
