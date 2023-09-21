# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(1)", 
  {
    expect_error(activateReportUser(oid = "1", 
                                    active = TRUE, 
                                    event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(activateReportUser(oid = c(1, 2), 
                                    active = TRUE, 
                                    event_user = 1), 
                 "'oid': Must have length 1")
  }
)

test_that(
  "Return an error if active is not logical(1)", 
  {
    expect_error(activateReportUser(oid = 1, 
                                    active = "TRUE", 
                                    event_user = 1), 
                 "'active': Must be of type 'logical'")
    
    expect_error(activateReportUser(oid = 1, 
                                    active = c(TRUE, FALSE), 
                                    event_user = 1), 
                 "'active': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(activateReportUser(oid = 1, 
                                    active = TRUE, 
                                    event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(activateReportUser(oid = 1, 
                                    active = TRUE, 
                                    event_user = c(1, 2)), 
                 "'event_user': Must have length 1")
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
    
    activateReportUser(oid = 1, 
                       active = FALSE, 
                       event_user = 1)
    
    expect_false(queryReportUser(oid = 1)$IsActive)
    
    activateReportUser(oid = 1, 
                       active = TRUE, 
                       event_user = 1)
    
    expect_true(queryReportUser(oid = 1)$IsActive)
  }
)