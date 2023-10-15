# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateSignature(oid = "1", 
                                               parent_report_template = 1, 
                                               parent_role = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateSignature(oid = 1:2, 
                                               parent_report_template = 1, 
                                               parent_role = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateSignature(oid = 1, 
                                               parent_report_template = "1", 
                                               parent_role = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateSignature(oid = 1, 
                                               parent_report_template = 1:2, 
                                               parent_role = 1), 
                 "'parent_report_template': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_role is not integerish(0/1)", 
  {
    expect_error(queryReportTemplateSignature(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_role = "1"), 
                 "'parent_role': Must be of type 'integerish'")
    
    expect_error(queryReportTemplateSignature(oid = 1, 
                                               parent_report_template = 1, 
                                               parent_role = 1:2), 
                 "'parent_role': Must have length <= 1")
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
    
    addEditReportTemplateSignature(parent_report_template = 2, 
                                   parent_role = 3, 
                                   order = 3,
                                   event_user = 1)
    
    Test <- queryReportTemplateSignature()
    
    expect_data_frame(Test)
    expect_true(nrow(Test) > 0)
    
    
    Test <- queryReportTemplateSignature(oid = 1)
    expect_data_frame(Test, 
                      nrows = 1)
    expect_equal(Test$OID, 1)
    
    
    Test <- queryReportTemplateSignature(parent_report_template = 1)
    expect_data_frame(Test)
    expect_true(all(Test$ParentReportTemplate == 1))
    
    
    Test <- queryReportTemplateSignature(parent_footer = 3)
    expect_data_frame(Test)
    expect_true(all(Test$ParentRole == 3))
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
    
    addEditReportTemplateSignature(parent_report_template = 2, 
                                    parent_role = 3, 
                                    order = 3,
                                    event_user = 1)
    
    Test <- queryReportTemplateSignature()
    
    expect_data_frame(Test)
    expect_true(nrow(Test) > 0)
    
    
    Test <- queryReportTemplateSignature(oid = 1)
    expect_data_frame(Test, 
                      nrows = 1)
    expect_equal(Test$OID, 1)
    
    
    Test <- queryReportTemplateSignature(parent_report_template = 1)
    expect_data_frame(Test)
    expect_true(all(Test$ParentReportTemplate == 1))
    
    
    Test <- queryReportTemplateSignature(parent_role = 3)
    expect_data_frame(Test)
    expect_true(all(Test$ParentRole == 3))
  }
)
