# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(1)", 
  {
    expect_error(activateRecord(oid = "1", 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "ReportUser", 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = "ParentReportUser"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(activateRecord(oid = c(1, 2), 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "ReportUser", 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = "ParentReportUser"), 
                 "'oid': Must have length 1")
  }
)

test_that(
  "Return an error if active is not logical(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = "TRUE", 
                                event_user = 1, 
                                table_name = "ReportUser", 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = "ParentReportUser"), 
                 "'active': Must be of type 'logical'")
    
    expect_error(activateRecord(oid = 1, 
                                active = c(TRUE, FALSE), 
                                event_user = 1, 
                                table_name = "ReportUser", 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = "ParentReportUser"), 
                 "'active': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = "1", 
                                table_name = "ReportUser", 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = "ParentReportUser"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = c(1, 2), 
                                table_name = "ReportUser", 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = "ParentReportUser"), 
                 "'event_user': Must have length 1")
  }
)

test_that(
  "Return an error if table_name is not character(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = 123, 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = "ParentReportUser"), 
                 "'table_name': Must be of type 'character'")
    
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = c("ReportUser", "ReportRole"), 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = "ParentReportUser"), 
                 "'table_name': Must have length 1")
  }
)

test_that(
  "Return an error if event_table_name is not character(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "ReportUser", 
                                event_table_name = 123, 
                                parent_field_name = "ParentReportUser"), 
                 "'event_table_name': Must be of type 'character'")
    
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "ReportUser", 
                                event_table_name = c("ReportUserEvent", "RoleEvent"), 
                                parent_field_name = "ParentReportUser"), 
                 "'event_table_name': Must have length 1")
  }
)

test_that(
  "Return an error if parent_field_name is not character(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "ReportUser", 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = 123), 
                 "'parent_field_name': Must be of type 'character'")
    
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "ReportUser", 
                                event_table_name = "ReportUserEvent", 
                                parent_field_name = c("ParentReportUser", "ParentRole")), 
                 "'parent_field_name': Must have length 1")
  }
)

# Functionality - SQL Server ----------------------------------------

# TODO: SQL Server validation

# Functionality - SQLite --------------------------------------------

configureReportManager(flavor = "sqlite")

test_that(
  "Activate and Deactivate Users", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    UserNow <- queryReportUser(oid = 1)
    
    expect_true(UserNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "ReportUser", 
                   event_table_name = "ReportUserEvent", 
                   parent_field_name = "ParentReportUser")
    
    expect_false(queryReportUser(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "ReportUser", 
                   event_table_name = "ReportUserEvent", 
                   parent_field_name = "ParentReportUser")
    
    expect_true(queryReportUser(oid = 1)$IsActive)
  }
)