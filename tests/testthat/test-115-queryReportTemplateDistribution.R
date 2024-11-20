# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateDistribution(oid = "1", 
                                               parent_report_template = 1, 
                                               parent_role = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateDistribution(oid = 1:2, 
                                               parent_report_template = 1, 
                                               parent_role = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateDistribution(oid = 1, 
                                               parent_report_template = "1", 
                                               parent_role = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateDistribution(oid = 1, 
                                               parent_report_template = 1:2, 
                                               parent_role = 1), 
                 "'parent_report_template': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_user is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateDistribution(oid = 1, 
                                              parent_report_template = 1, 
                                              parent_user = "1"), 
                 "'parent_user': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateDistribution(oid = 1, 
                                              parent_report_template = 1, 
                                              parent_user = 1:2), 
                 "'parent_user': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_role is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateDistribution(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_role = "1"), 
                 "'parent_role': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateDistribution(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_role = 1:2), 
                 "'parent_role': Must have length <= 1")
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
      
      addEditReportTemplateDistribution(parent_report_template = 2, 
                                        parent_role = 1, 
                                        event_user = 1)
      
      Test <- queryReportTemplateDistribution()
      
      expect_data_frame(Test)
      expect_true(nrow(Test) > 0)
      
      
      Test <- queryReportTemplateDistribution(oid = 1)
      expect_data_frame(Test, 
                        nrows = 1)
      expect_equal(Test$OID, 1)
      
      
      Test <- queryReportTemplateDistribution(parent_report_template = 1)
      expect_data_frame(Test)
      expect_true(all(Test$ParentReportTemplate == 1))
      
      Test <- queryReportTemplateDistribution(parent_user = 1)
      expect_data_frame(Test)
      expect_true(all(Test$ParentUser == 1))
      
      Test <- queryReportTemplateDistribution(parent_role = 1)
      expect_data_frame(Test)
      expect_true(all(Test$ParentRole == 1))
    }
  )
}
