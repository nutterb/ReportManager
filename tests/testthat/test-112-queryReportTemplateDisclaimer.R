# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateDisclaimer(oid = "1", 
                                               parent_report_template = 1, 
                                               parent_disclaimer = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateDisclaimer(oid = 1:2, 
                                               parent_report_template = 1, 
                                               parent_disclaimer = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateDisclaimer(oid = 1, 
                                               parent_report_template = "1", 
                                               parent_disclaimer = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateDisclaimer(oid = 1, 
                                               parent_report_template = 1:2, 
                                               parent_disclaimer = 1), 
                 "'parent_report_template': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_disclaimer is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateDisclaimer(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_disclaimer = "1"), 
                 "'parent_disclaimer': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateDisclaimer(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_disclaimer = 1:2), 
                 "'parent_disclaimer': Must have length <= 1")
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "Return the appropriate data frames", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    # Add an extra record just so there are more than 1
    
    addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                    parent_disclaimer = 3, 
                                    order = 1,
                                    event_user = 1)
    
    Test <- queryReportTemplateDisclaimer()
    
    expect_data_frame(Test)
    expect_true(nrow(Test) > 0)
    
    
    Test <- queryReportTemplateDisclaimer(oid = 1)
    expect_data_frame(Test, 
                      nrows = 1)
    expect_equal(Test$OID, 1)
    
    
    Test <- queryReportTemplateDisclaimer(parent_report_template = 1)
    expect_data_frame(Test)
    expect_true(all(Test$ParentReportTemplate == 1))
    
    
    Test <- queryReportTemplateDisclaimer(parent_disclaimer = 3)
    expect_data_frame(Test)
    expect_true(all(Test$ParentDisclaimer == 3))
  }
)

# Functionality - SQLite --------------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "Return the appropriate data frames", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    # Add an extra record just so there are more than 1
    
    addEditReportTemplateDisclaimer(parent_report_template = 2, 
                                    parent_disclaimer = 3, 
                                    order = 3,
                                    event_user = 1)
    
    Test <- queryReportTemplateDisclaimer()
    
    expect_data_frame(Test)
    expect_true(nrow(Test) > 0)
    
    
    Test <- queryReportTemplateDisclaimer(oid = 1)
    expect_data_frame(Test, 
                      nrows = 1)
    expect_equal(Test$OID, 1)
    
    
    Test <- queryReportTemplateDisclaimer(parent_report_template = 1)
    expect_data_frame(Test)
    expect_true(all(Test$ParentReportTemplate == 1))
    
    
    Test <- queryReportTemplateDisclaimer(parent_disclaimer = 3)
    expect_data_frame(Test)
    expect_true(all(Test$ParentDisclaimer == 3))
  }
)
