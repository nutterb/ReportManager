# Argument Validation -----------------------------------------------

test_that(
  "Cast an error if parent_report_template is not integerish(1)", 
  {
    expect_error(
      queryReportTemplateUserPermission(parent_report_template = "1", 
                                        parent_user = 1),
      "'parent_report_template': Must be of type 'integerish'"
    )
    
    expect_error(
      queryReportTemplateUserPermission(parent_report_template = 1:2, 
                                        parent_user = 1),
      "'parent_report_template': Must have length 1"
    )
  }
)


test_that(
  "Cast an error if parent_user is not integerish(1)", 
  {
    expect_error(
      queryReportTemplateUserPermission(parent_report_template = 1, 
                                        parent_user = "1"),
      "'parent_user': Must be of type 'integerish'"
    )
    
    expect_error(
      queryReportTemplateUserPermission(parent_report_template = 1, 
                                        parent_user = 1:2),
      "'parent_user': Must have length 1"
    )
  }
)

# Functionality -----------------------------------------------------

test_that(
  "Return a data frame of permissions", 
  {
    Permission <- queryReportTemplateUserPermission(parent_report_template = 1, 
                                                    parent_user = 1)
    
    expect_data_frame(Permission, 
                      ncols = 7)
  }
)
