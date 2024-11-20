# Argument Valdiation -----------------------------------------------

test_that(
  "report_template_oid is numeric(0/1)", 
  {
    expect_error(queryReportInstance(report_template_oid = "1"), 
                 "'report_template_oid': Must be of type 'integerish'")
    
    expect_error(queryReportInstance(report_template_oid = 1:2), 
                 "'report_template_oid': Must have length [<][=] 1")
  }
)

test_that(
  "report_instance_oid is numeric(0/1)", 
  {
    expect_error(queryReportInstance(report_instance_oid = "1"), 
                 "'report_instance_oid': Must be of type 'integerish'")
    
    expect_error(queryReportInstance(report_instance_oid = 1:2), 
                 "'report_instance_oid': Must have length [<][=] 1")
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
      
      addEditReportInstance(report_instance_oid = numeric(0), 
                            parent_report_template = 2, 
                            start_time = Sys.time(), 
                            end_time = Sys.time(), 
                            is_signature_required = FALSE, 
                            is_scheduled = FALSE, 
                            instance_title = "Specific Title", 
                            is_submitted = FALSE, 
                            event_user = 1)
      
      Test <- queryReportInstance()
      
      expect_data_frame(Test)
      expect_true(nrow(Test) > 0)
      
      
      Test <- queryReportInstance(report_instance_oid = 1)
      expect_data_frame(Test, 
                        nrows = 1)
      expect_equal(Test$OID, 1)
      
      
      Test <- queryReportInstance(report_template_oid = 1)
      expect_data_frame(Test)
      expect_true(all(Test$ParentReportTemplate == 1))
  
      Test <- queryReportInstance(report_instance_oid = 1, 
                                  report_template_oid = 2)
      expect_data_frame(Test, 
                        nrows = 0)
    }
  )
}
