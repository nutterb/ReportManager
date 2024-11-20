# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(addEditReportTemplateDistribution(oid = "1", 
                                             parent_report_template = 1, 
                                             parent_role = 1, 
                                             event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDistribution(oid = 1:2, 
                                             parent_report_template = 1, 
                                             parent_role = 1,  
                                             event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(1)", 
  {
    expect_error(addEditReportTemplateDistribution(parent_report_template = "1", 
                                             parent_role = 1,  
                                             event_user = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1:2, 
                                             parent_role = 1, 
                                             event_user = 1), 
                 "'parent_report_template': Must have length 1")
  }
)

test_that(
  "Return an error when parent_user is not integerish(1)", 
  {
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                                   parent_user = "1", 
                                                   event_user = 1), 
                 "'parent_user': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                                   parent_user = 1:2, 
                                                   event_user = 1), 
                 "'parent_user': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_role is not integerish(1)", 
  {
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                             parent_role = "1", 
                                             event_user = 1), 
                 "'parent_role': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                             parent_role = 1:2, 
                                             event_user = 1), 
                 "'parent_role': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_user and parent_role are both given", 
  {
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                                   parent_user = 1,
                                                   parent_role = 1, 
                                                   event_user = 1), 
                 "Only one of 'parent_user' or 'parent_role' may be provided.")
  }
)

test_that(
  "Return an error when is_active is not logical(1)", 
  {
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                             parent_role = 1, 
                                             is_active = "TRUE", 
                                             event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                             parent_role = 1,  
                                             is_active = c(TRUE, FALSE), 
                                             event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error when event_user is not integerish(1)", 
  {
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                             parent_role = 1, 
                                             event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                             parent_role = 1, 
                                             event_user = 1:2), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - SQL Server ----------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
    purgeReportManagerDatabase()
    initializeUiTestingDatabase(SQL_FILE[flavor], 
                                include = c("User", "Role", "UserRole", 
                                            "ReportTemplate"))
  }

  test_that(
    "Record can be added for a user", 
    {
      skip_if_not(.ready, 
                  .message)
      
      CurrentObjects <- queryReportTemplateDistribution()
      
      next_oid <- nrow(CurrentObjects) + 1
      
      addEditReportTemplateDistribution(parent_report_template = 1, 
                                        parent_user = 1,  
                                        event_user = 1)
      
      New <- queryReportTemplateDistribution(oid = next_oid)
      
      expect_data_frame(New, 
                        nrows = 1)
      expect_equal(New$ParentReportTemplate, 
                   1)
      expect_equal(New$ParentUser, 
                   1)
    }
  )
    
  test_that(
    "Record can be added for a role", 
    {
      skip_if_not(.ready, 
                  .message)
      
      CurrentObjects <- queryReportTemplateDistribution()
      
      next_oid <- nrow(CurrentObjects) + 1
      
      addEditReportTemplateDistribution(parent_report_template = 1, 
                                        parent_role = 1,  
                                        event_user = 1)
      
      New <- queryReportTemplateDistribution(oid = next_oid)
      
      expect_data_frame(New, 
                        nrows = 1)
      expect_equal(New$ParentReportTemplate, 
                   1)
      expect_equal(New$ParentRole, 
                   1)
    }
  )
  
  test_that(
    "Return an error if attempting to add a record for an existing template-signature", 
    {
      skip_if_not(.ready, 
                  .message)
      
      expect_error(addEditReportTemplateDistribution(parent_report_template = 1, 
                                                  parent_role = 1,  
                                                  event_user = 1), 
                   "A ReportTemplateDistribution record for ReportTemplate.OID")
    }
  )
  
  test_that(
    "Edit an existing record", 
    {
      skip_if_not(.ready, 
                  .message)
      
      addEditReportTemplateDistribution(oid = 1, 
                                     parent_report_template = 1, 
                                     parent_role = 2, 
                                     is_active = FALSE,
                                     event_user = 1)
      
      New <- queryReportTemplateDistribution(oid = 1)
      
      expect_data_frame(New, 
                        nrows = 1)
      expect_equal(New$ParentReportTemplate, 
                   1)
      expect_equal(New$ParentRole, 
                   2)
      expect_false(New$IsActive)
    }
  )
  
  test_that(
    "Confirm events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
      conn <- connectToReportManager()
      
      last_oid <- max(queryReportTemplateDistribution()$OID)
      next_oid <- last_oid + 1
      
      addEditReportTemplateDistribution(parent_report_template = 2,
                                        parent_role = 3,
                                        is_active = TRUE,
                                        event_user = 1)
      
      TemplateEvent <- dbGetQuery(conn, 
                                  sqlInterpolate(
                                    conn,
                                    switch(flavor, 
                                           "sql_server" = "SELECT * FROM dbo.ReportTemplateDistributionEvent WHERE ParentReportTemplateDistribution = ?",
                                           "SELECT * FROM ReportTemplateDistributionEvent WHERE ParentReportTemplateDistribution = ?"),
                                    next_oid))
      
      expect_equal(TemplateEvent$EventType,
                   c("Add", "Activate"))
      expect_true(all(table(TemplateEvent$EventType) == 1))
      
      addEditReportTemplateDistribution(oid = next_oid, 
                                     parent_report_template = 2, 
                                     parent_role = 3,
                                     is_active = FALSE,
                                     event_user = 1)
      
      
      TemplateEvent2 <- dbGetQuery(conn, 
                                   sqlInterpolate(
                                     conn,
                                     switch(flavor, 
                                            "sql_server" = "SELECT * FROM dbo.ReportTemplateDistributionEvent WHERE ParentReportTemplateDistribution = ?",
                                            "SELECT * FROM ReportTemplateDistributionEvent WHERE ParentReportTemplateDistribution = ?"),
                                     next_oid))
      
      expect_true(
        all(table(TemplateEvent2$EventType) ==
              c("Activate" = 1, 
                "Add" = 1, 
                "Deactivate" = 1))
      )
      
      dbDisconnect(conn)
    }
  )
}
