# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(addEditReportTemplateDisclaimer(oid = "1", 
                                                 parent_report_template = 1, 
                                                 parent_disclaimer = 1, 
                                                 order = 1,
                                                 event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDisclaimer(oid = 1:2, 
                                                 parent_report_template = 1, 
                                                 parent_disclaimer = 1,  
                                                 order = 1,
                                                 event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(1)", 
  {
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = "1", 
                                                 parent_disclaimer = 1,  
                                                 order = 1,
                                                 event_user = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1:2, 
                                                 parent_disclaimer = 1, 
                                                 order = 1, 
                                                 event_user = 1), 
                 "'parent_report_template': Must have length 1")
  }
)

test_that(
  "Return an error when parent_disclaimer is not integerish(1)", 
  {
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                                 parent_disclaimer = "1", 
                                                 order = 1, 
                                                 event_user = 1), 
                 "'parent_disclaimer': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                                 parent_disclaimer = 1:2, 
                                                 order = 1, 
                                                 event_user = 1), 
                 "'parent_disclaimer': Must have length 1")
  }
)

test_that(
  "Return an error when order is not integerish(1)", 
  {
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                                 parent_disclaimer = 1, 
                                                 order = "1", 
                                                 event_user = 1), 
                 "'order': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                                 parent_disclaimer = 1, 
                                                 order = 1:2, 
                                                 event_user = 1), 
                 "'order': Must have length 1")
  }
)

test_that(
  "Return an error when is_active is not logical(1)", 
  {
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                                 parent_disclaimer = 1, 
                                                 order = 1, 
                                                 is_active = "TRUE", 
                                                 event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                                 parent_disclaimer = 1,  
                                                 order = 1,
                                                 is_active = c(TRUE, FALSE), 
                                                 event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error when event_user is not integerish(1)", 
  {
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                                 parent_disclaimer = 1, 
                                                 order = 1, 
                                                 event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                                 parent_disclaimer = 1, 
                                                 order = 1, 
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
    "Record can be added", 
    {
      skip_if_not(.ready, 
                  .message)
      
      CurrentObjects <- queryReportTemplateDisclaimer()
      
      next_oid <- nrow(CurrentObjects) + 1
      
      addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                      parent_disclaimer = 1,  
                                      order = 1,
                                      event_user = 1)
      
      New <- queryReportTemplateDisclaimer(oid = next_oid)
      
      expect_data_frame(New, 
                        nrows = 1)
      expect_equal(New$ParentReportTemplate, 
                   1)
      expect_equal(New$ParentDisclaimer, 
                   1)
    }
  )
  
  test_that(
    "Return an error if attempting to add a record for an existing template-disclaimer", 
    {
      skip_if_not(.ready, 
                  .message)
      
      expect_error(addEditReportTemplateDisclaimer(parent_report_template = 1, 
                                                   parent_disclaimer = 1,  
                                                   order = 1,
                                                   event_user = 1), 
                   "A ReportTemplateDisclaimer record for ReportTemplate.OID")
    }
  )
  
  test_that(
    "Edit an existing record", 
    {
      skip_if_not(.ready, 
                  .message)
      
      addEditDisclaimer(disclaimer = "This is a second disclaimer for testings", 
                        event_user = 1)
      
      addEditReportTemplateDisclaimer(oid = 1, 
                                      parent_report_template = 1, 
                                      parent_disclaimer = 2, 
                                      order = 1,
                                      is_active = FALSE,
                                      event_user = 1)
      
      New <- queryReportTemplateDisclaimer(oid = 1)
      
      expect_data_frame(New, 
                        nrows = 1)
      expect_equal(New$ParentReportTemplate, 
                   1)
      expect_equal(New$ParentDisclaimer, 
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
      
      last_oid <- max(queryReportTemplateDisclaimer()$OID)
      next_oid <- last_oid + 1
      
      addEditReportTemplateDisclaimer(parent_report_template = 2,
                                      parent_disclaimer = 1,
                                      order = 1,
                                      is_active = TRUE,
                                      event_user = 1)
      
      TemplateEvent <- dbGetQuery(conn, 
                                  sqlInterpolate(
                                    conn,
                                    switch(flavor, 
                                           "sql_server" = "SELECT * FROM dbo.ReportTemplateDisclaimerEvent WHERE ParentReportTemplateDisclaimer = ?",
                                           "SELECT * FROM ReportTemplateDisclaimerEvent WHERE ParentReportTemplateDisclaimer = ?"),
                                    next_oid))
      
      expect_equal(TemplateEvent$EventType,
                   c("Add", "Reorder", "Activate"))
      expect_true(all(table(TemplateEvent$EventType) == 1))
      
      addEditReportTemplateDisclaimer(oid = next_oid, 
                                      parent_report_template = 2, 
                                      parent_disclaimer = 2,
                                      order = 2, 
                                      is_active = FALSE,
                                      event_user = 1)
      
      
      TemplateEvent2 <- dbGetQuery(conn, 
                                   sqlInterpolate(
                                     conn,
                                     switch(flavor, 
                                            "sql_server" = "SELECT * FROM dbo.ReportTemplateDisclaimerEvent WHERE ParentReportTemplateDisclaimer = ?",
                                            "SELECT * FROM ReportTemplateDisclaimerEvent WHERE ParentReportTemplateDisclaimer = ?"), 
                                     next_oid))
      
      expect_true(
        all(table(TemplateEvent2$EventType) ==
              c("Activate" = 1, 
                "Add" = 1, 
                "Deactivate" = 1, 
                "Reorder" = 2))
      )
      
      dbDisconnect(conn)
    }
  )
}
