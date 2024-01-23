# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateFooter(oid = "1", 
                                               parent_report_template = 1, 
                                               parent_footer = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateFooter(oid = 1:2, 
                                               parent_report_template = 1, 
                                               parent_footer = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateFooter(oid = 1, 
                                               parent_report_template = "1", 
                                               parent_footer = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateFooter(oid = 1, 
                                               parent_report_template = 1:2, 
                                               parent_footer = 1), 
                 "'parent_report_template': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_footer is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateFooter(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_footer = "1"), 
                 "'parent_footer': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateFooter(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_footer = 1:2), 
                 "'parent_footer': Must have length <= 1")
  }
)

# Functionality - SQL Server ----------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
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
      
      addEditReportTemplateFooter(parent_report_template = 2, 
                                  parent_footer = 4,
                                  order = 1,
                                  event_user = 1)
      
      Test <- queryReportTemplateFooter()
      
      expect_data_frame(Test)
      expect_true(nrow(Test) > 0)
      
      
      Test <- queryReportTemplateFooter(oid = 1)
      expect_data_frame(Test, 
                        nrows = 1)
      expect_equal(Test$OID, 1)
      
      
      Test <- queryReportTemplateFooter(parent_report_template = 1)
      expect_data_frame(Test)
      expect_true(all(Test$ParentReportTemplate == 1))
      
      
      Test <- queryReportTemplateFooter(parent_footer = 4)
      expect_data_frame(Test)
      expect_true(all(Test$ParentFooter == 4))
    }
  )
}
