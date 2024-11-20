# Arugment Validation -----------------------------------------------

test_that(
  "cast an error if oid is not integerish(0/1)", 
  {
    expect_error(
      queryReportTemplatePermission(oid = "1", 
                                      parent_report_template = 1, 
                                      parent_role = 1), 
      "'oid': Must be of type 'integerish'"
    )
    
    expect_error(
      queryReportTemplatePermission(oid = 1:2, 
                                      parent_report_template = 1, 
                                      parent_role = 1), 
      "'oid': Must have length [<][=] 1"
    )
  }
)

test_that(
  "cast an error if parent_report_template is not integerish(0/1)", 
  {
    expect_error(
      queryReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = "1", 
                                      parent_role = 1), 
      "'parent_report_template': Must be of type 'integerish'"
    )
    
    expect_error(
      queryReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1:2, 
                                      parent_role = 1), 
      "'parent_report_template': Must have length [<][=] 1"
    )
  }
)

test_that(
  "cast an error if parent_role is not integerish(0/1)", 
  {
    expect_error(
      queryReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = "1"), 
      "'parent_role': Must be of type 'integerish'"
    )
    
    expect_error(
      queryReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1:2), 
      "'parent_role': Must have length [<][=] 1"
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
    "Return the appropriate data frames", 
    {
      skip_if_not(.ready, 
                  .message)
      
      # Add an extra record so that there are multiple
      addEditReportTemplatePermission(parent_report_template = 2, 
                                      parent_role = 3, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = FALSE, 
                                      can_submit = FALSE, 
                                      can_start_revision = FALSE, 
                                      is_active = TRUE, 
                                      event_user = 1)
      
      Test <- queryReportTemplatePermission()
      
      expect_data_frame(Test)
      expect_true(nrow(Test) > 0)
      
      
      Test <- queryReportTemplatePermission(oid = 1)
      expect_data_frame(Test, 
                        nrows = 1)
      
      
      Test <- queryReportTemplatePermission(parent_report_template = 2)
      expect_true(Test$ParentReportTemplate == 2)
      
      
      Test <- queryReportTemplatePermission(parent_role = 3)
      expect_true(Test$ParentRole == 3)
    }
  )
}